#' @title Generate Synthetic Rating Curve
#' @param comids A set of COMIDs. See \link{find_comids}.
#' @param hand A HAND \code{raster}.
#' @param stage A set of stage values. Defaults to \code{0:20}.
#' @param progress If \code{TRUE}, output a progress bar.
#' @param slope_scale Ratio of vertical units to horizontal.
#'                    Uses \code{slope_scale = 111120} by default.
#'                    See [gdaldem](https://gdal.org/programs/gdaldem.html).
#' @param verbose Verbose output for HAND download.
#' @return A \code{data.frame} representing the SRC table.
#' @importFrom nhdplusTools get_vaa
#' @importFrom data.table :=
#' @importFrom plyr .
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @export
get_src <- function(comids, hand, stage = 0:20,
                    progress = TRUE, slope_scale = 111120,
                    verbose = getOption("verbose")) {
    # nocov start
    if (missing(comids) & missing(hand)) {
        stop("[get_src()] one of `comids` or `hand` needs to be passed.",
             .call = FALSE)
    }

    if (missing(comids) & !missing(hand)) {
        hand_bb <- sf::st_bbox(hand) %>%
                   sf::st_as_sfc() %>%
                   sf::st_as_sf()

        comids <- find_comids(hand_bb)
    }

    if (any(is.na(comids), is.null(comids), is.nan(comids)) &
        missing(hand)) {
        stop("[get_src()] `comids` can't be NA/NaN/NULL",
             .call = FALSE)
    }

    # If the HAND raster is not passed, get from AWS
    if (missing(hand) & !missing(comids)) {
        nhd <- nhdplusTools::get_nhdplus(comid = comids)
        hand <- get_hand_raster(nhd, verbose = verbose, clip = "locations")
    }

    if (progress) {
        pb <- progress::progress_bar$new(
            total = length(stage) + 3,
            format = " generating SRC [:bar] :percent eta: :eta"
        )

        pb$tick(0)
    }

    # nocov end

    # Fix NA values
    hand[is.na(hand[])] <- 0

    # Get tempfiles for HAND and Slope rasters
    tmp_slope <- tempfile(fileext = ".tif")
    tmp_hand  <- tempfile(fileext = ".tif")

    # Temporarily write HAND to file
    raster::writeRaster(x = hand, filename = tmp_hand)

    # Generate Slope Raster
    sf::gdal_utils(util = "demprocessing",
                   source = tmp_hand,
                   destination = tmp_slope,
                   processing = "slope",
                   options = c(
                       "-p",
                       "-s", as.character(slope_scale)
                   ))

    slope <- raster::raster(tmp_slope)
    slope[is.na(slope[])] <- 0

    # Delete temporary rasters
    unlink(tmp_slope)
    unlink(tmp_hand)

    # Get raster cell resolution
    res <- compute_cellres(hand)

    if (progress) pb$tick() # nocov

    # Create HAND_Slope data frame
    hand_slope <-
        data.frame(
            hand = raster::getValues(hand),
            slope = raster::getValues(slope)
        ) %>%
        stats::na.omit() %>%
        dplyr::arrange(.data$hand) %>%
        dplyr::mutate(
            res = res,
            BA  = res * res * sqrt(1 + .data$slope)
        )

    if (progress) pb$tick() # nocov

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
        dplyr::filter(.data$comid %in% comids) %>%
        stats::na.omit()

    if (progress) pb$tick() # nocov

    synthetic_rating_curve <-
        lapply(
            stage,
            FUN = function(y) {

                if (progress) pb$tick() # nocov

                vaa %>%
                    dplyr::group_by(.data$comid) %>%
                    dplyr::mutate(
                        n_prod = sqrt(.data$slope) /
                                 ((.data$lengthkm * 1000) *
                                  .data$roughness),
                        Y = y
                    ) %>%
                    dplyr::ungroup() %>%
                    dplyr::filter(dplyr::across(.fns = is.finite)) %>%
                    data.table::as.data.table() %>%
                    .[,
                      Q := compute_flat_tub(y, hand_slope) * .data$n_prod,
                      by = .(comid)
                     ]
            }
        ) %>%
        data.table::rbindlist() %>%
        as.data.frame() %>%
        dplyr::select(.data$comid, .data$Y, .data$Q) %>%
        dplyr::rename(COMID = .data$comid)

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
    if (!is.numeric(unlist(as.list(environment())))) {
        stop("[get_roughness()] All parameters must be numeric", .call = FALSE)
    }

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
#' @return A \code{factor vector} of COMIDs.
#' @importFrom nhdplusTools get_nhdplus
#' @keywords internal
#' @export
find_comids <- function(aoi) {
    as.numeric(nhdplusTools::get_nhdplus(aoi)$comid)
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
    error = function(cond) {
        return(NA)
    })
}