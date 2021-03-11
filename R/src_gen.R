#' @title Generate Synthetic Rating Curve
#' @param comids A set of COMIDs. See \link{find_comids}.
#' @param hand A HAND \code{raster}.
#' @param stage A set of stage values. Defaults to \code{0:20}.
#' @param progress If \code{TRUE}, output a progress bar.
#' @return A \code{data.frame} representing the SRC table.
#' @importFrom nhdplusTools get_vaa
#' @importFrom data.table :=
#' @importFrom plyr .
#' @export
get_src <- function(comids, hand, stage = 0:20, progress = TRUE) {
    if (progress) {
        pb <- progress::progress_bar$new(
            total = length(stage) + 3,
            format = " generating SRC [:bar] :percent eta: :eta"
        )

        pb$tick(0)
    }

    if (missing(comids) & !missing(hand)) {
        hand_bb <- sf::st_bbox(hand) %>%
                   sf::st_as_sfc() %>%
                   sf::st_as_sf()

        comids <- find_comids(hand_bb)
    }

    # If the HAND raster is not passed, get from AWS
    if (missing(hand) & !missing(comids)) {
        nhd <- nhdplusTools::get_nhdplus(comid = comids)
        hand <- get_hand_raster(nhd)
    }

    # Fix NA values
    hand[is.na(hand[])] <- 0
    # Generate Slope Raster
    tmp <- tempfile(fileext = ".tif")

    sf::gdal_utils(util = "demprocessing",
                   source = hand@file@name,
                   destination = tmp,
                   processing = "slope",
                   options = c("-p"))

    slope <- raster::raster(tmp)
    slope[is.na(slope[])] <- 0
    unlink(tmp)

    # Get raster cell resolution
    res <- compute_cellres(hand)
    # Create HAND_Slope data frame

    if (progress) pb$tick()

    hand_slope <-
        data.frame(
            hand = raster::getValues(hand),
            slope = raster::getValues(slope)
        ) %>%
        na.omit() %>%
        dplyr::arrange(hand) %>%
        dplyr::mutate(
            res = res,
            BA  = res * res * sqrt(1 + slope)
        )

    if (progress) pb$tick()

    # Get VAAs via nhdplusTools
    vaa <-
        nhdplusTools::get_vaa(
            atts = c(
                "pathlength",
                "arbolatesu",
                "lengthkm",
                "areasqkm",
                "slope",
                "roughness"
            )
        ) %>%
        dplyr::filter(comid %in% comids) %>%
        na.omit()

    if (progress) pb$tick()

    synthetic_rating_curve <-
        lapply(
            stage,
            FUN = function(y) {

                if (progress) pb$tick()

                vaa %>%
                    dplyr::group_by(comid) %>%
                    dplyr::mutate(
                        n_prod = sqrt(slope) / ((lengthkm * 1000) * roughness),
                        Y = y
                    ) %>%
                    dplyr::ungroup() %>%
                    dplyr::filter(dplyr::across(.fns = is.finite)) %>%
                    data.table::as.data.table() %>%
                    .[,
                      Q := compute_flat_tub(y, hand_slope) * n_prod,
                      by = .(comid)
                     ]
            }
        ) %>%
        data.table::rbindlist() %>%
        as.data.frame() %>%
        dplyr::select(comid, Y, Q) %>%
        dplyr::rename(COMID = comid)

    synthetic_rating_curve
}

#' @title Get catchmask raster via nhdplusTools
#' @param aoi An area of interest. Can be retrieved via \code{AOI::aoi_get}.
#' @param template A \code{raster} to template
#'                 the catchmask raster to.
#' @return A \code{raster} representing the Catchmask
#'         templated to **template**.
#' @importFrom fasterize fasterize
#' @importFrom nhdplusTools get_nhdplus
#' @keywords internal
#' @export
get_catchmask <- function(aoi, template) {
    this_nhd <- nhdplusTools::get_nhdplus(
                    aoi,
                    realization = "catchment"
                )

    fasterize::fasterize(
        sf::st_cast(this_nhd),
        template,
        field = "featureid"
    )
}

#' @title Get roughness value prediction
#'
#' @description
#' This function querys a REST API serving the Gradient Boosting machine
#' learning model developed in the in-review paper:
#'
#' **Johnson, J.M., Eyelade D., Clarke K.C, Singh-Mohudpur, J. (2021)
#' *“Characterizing Reach-level Empirical Roughness Along the National
#' Hydrography Network: Developing DEM-based Synthetic Rating Curves.”***
#'
#' @param pathlength The distance from the flowline outlet to the end of
#'                   the network along the main network path
#' @param arbolatesu Arbolate sum: the cumulative length of the upstream
#'                   drainage network (mainstem and tributaries) from
#'                   the outlet of the catchment
#' @param lengthkm Length of a flowline in kilometers
#' @param areasqkm Drainage area in km^2
#' @param slope The longitudinal slope.
#' @return A roughness value
#' @export
get_roughness <- function(pathlength, arbolatesu, lengthkm, areasqkm, slope) {
    response <- httr::content(
        httr::POST(
            url = "https://src-api.justinsingh.me/roughness",
            query = list(
                pathlength = pathlength,
                arbolatesu = arbolatesu,
                lengthkm   = lengthkm,
                areasqkm   = areasqkm,
                slope      = slope
            )
        )
    )

    response[[1]]
}

#' @title Find COMIDs for an AOI via the NHD
#' @param aoi An area of interest. Can be retrieved via \code{AOI::aoi_get}.
#' @return A \code{vector} of COMIDs.
#' @importFrom nhdplusTools get_nhdplus
#' @keywords internal
#' @export
find_comids <- function(aoi) {
    nhdplusTools::get_nhdplus(aoi)$comid
}

#' @title Get cell resolution
#' @param raw Raster
#' @keywords internal
#' @export
compute_cellres <- function(raw) {
  area <- sf::st_bbox(raw) %>%
          sf::st_as_sfc() %>%
          sf::st_transform(5070) %>%
          sf::st_area()

  as.numeric(sqrt(area / raster::ncell(raw)))
}

#' @title Get flat tub
#' @param stage Stage value
#' @param df HAND/Slope dataframe. See \link{get_src}.
#' @keywords internal
#' @export
compute_flat_tub <- function(stage, df) {
    tryCatch({
        depth    <- pmax(0, (stage - df$hand))
        volume   <- cumsum((df$res ^ 2) * depth)
        bed_area <- sum(df$BA[1:which.max(volume)])
        final    <- volume[which.max(volume)]

        return(final * ((final / bed_area) ^ (2 / 3)))
    },
    error = function(cond) {return(NA)})
}