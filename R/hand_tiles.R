#' @title Get HAND Raster for an AOI
#'
#' @description
#' This function provides access to a Amazon Web Services HAND S3 Bucket.
#' The function accepts a \code{data.frame} of x (long) and y (lat),
#' a \code{sf}, or \code{raster} object as input.
#' A \code{raster} object is returned.
#'
#'
#' @param locations Either a \code{data.frame} of x (long) and y (lat), a
#'                  \code{sf}, or \code{raster} object as input.
#' @param prj A PROJ.4 string defining the projection of the locations argument.
#'            If a \code{sp} or \code{raster} object is provided, the PROJ.4
#'            string will be taken from that.  This argument is required for a
#'            \code{data.frame} of locations."
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the terrain tiles. This can
#'               be used for features that fall close to the edge of a tile and
#'               additional area around the feature is desired. Default is NULL.
#' @param clip A character value used to determine clipping of returned HAND.
#'             The default value is "tile" which returns the full tiles.  Other
#'             options are "bbox" which returns the HAND clipped to the bounding
#'             box of the original locations (or expanded bounding box if used),
#'             or "locations" if the spatials data (e.g. polygons) in the input
#'             locations should be used to clip the HAND. Locations are not used
#'             to clip input point datasets.  Instead the bounding box is used.
#' @param verbose Toggles on and off the note about units and coordinate
#'                reference system, as well as download progress and merging
#'                messages. This does **not** prevent download size related
#'                messages from appearing.
#' @param neg_to_na Some of the data sources return large negative numbers as
#'                  missing data.  When the end result is a projected those
#'                  large negative numbers can vary.  When set to TRUE, only
#'                  zero and positive values are returned.  Default is FALSE.
#' @param override_size_check Boolean to override size checks.  Any download
#'                            between 100 Mb and 500Mb report a message but
#'                            continue.  Between 500Mb and 3000Mb requires
#'                            interaction and greater than 3000Mb fails. These
#'                            can be overriden with this argument set to TRUE.
#' @param ... Extra arguments to pass to \code{httr::GET} via a named vector,
#'            \code{config}. See
#'            \code{\link{download_tiles}} for more details.
#' @return Function returns a \code{raster} object.
#' @export
get_hand_raster <- function(locations, prj = NULL,
                            expand = NULL,
                            clip = c("tile", "bbox", "locations"),
                            verbose = TRUE, neg_to_na = FALSE,
                            override_size_check = FALSE, ...) {

    clip <- match.arg(clip)

    # Check location type and if sf, set prj. If no prj (for either) then error
    locations <- loc_check(locations, prj)
    prj       <- sf::st_crs(locations)$wkt

    # Check download size and provide feedback, stop if too big!
    dl_size <- estimate_raster_size(locations)

    # nocov start
    if (dl_size > 100 & dl_size < 500) {
        message(paste0("Note: Your request will download approximately ",
                       round(dl_size, 1), "Mb."))
    } else if (dl_size > 500 & dl_size <= 2000) {
        message(paste0("Your request will download approximately ",
                       round(dl_size, 1), "Mb."))

        if (!override_size_check) {
            y <- readline(prompt = "Press [y] to continue with this request.")
            if (tolower(y) != "y") return()
        }
    } else if (!override_size_check & dl_size > 2000) {
        stop(
            paste0(
                "Your request will download approximately ",
                round(dl_size, 1), "Mb. That's probably too big. If you
                really want to do this, set override_size_check = TRUE."
            ),
            .call = FALSE
        )
    }
    # nocov end

    # Pass of locations to APIs to get data as raster
    raster_hand <- download_tiles(
        locations,
        z = 0,
        prj = prj,
        expand = expand,
        verbose = verbose,
        ...
    )

    # nocov start
    if (clip != "tile") {
        if (verbose) message(paste("Clipping DEM to", clip))
        raster_hand <- clip_it(raster_hand, locations, expand, clip)
    }
    # nocov end

    if (verbose) {
        message(
            paste(
                "Note: units are in meters.\n",
                "Note: The coordinate reference system is:\n",
                prj
            )
        )
    }

    if (neg_to_na) raster_hand[raster_hand < 0] <- NA

    raster_hand
}

#' @title Get HAND Tiles from an AWS S3 Bucket
#'
#' @description
#' This function uses AWS S3 to retrieve HAND rasters from the geotiff folder.
#' It accepts a \code{sf::st_bbox} object as input and returns a single raster
#' object covering that extent.
#'
#' @param bbx a \code{sf::st_bbox} object that is used to select x,y,z tiles.
#' @param z The zoom level to return. The HAND tiles only use `z = 0`.
#' @param prj PROJ.4 string for input bbox
#' @param expand A numeric value of a distance, in meters, used to expand the
#'               bounding box that is used to fetch the tiles. This can
#'               be used for features that fall close to the edge of a tile and
#'               additional area around the feature is desired. Default is NULL.
#' @param verbose Verbose output
#' @param ... Extra configuration parameters to be passed to httr::GET.  Common
#'            usage is to adjust timeout.  This is done as
#'            \code{config=timeout(x)} where \code{x} is a numeric value in
#'            seconds. Multiple configuration functions may be passed as a
#'            vector.
#' @importFrom progress progress_bar
#' @export
#' @keywords internal
download_tiles <- function(locations,
                           z = 0, prj,
                           expand = NULL,
                           verbose = TRUE,
                           ...) {
    # Expand (if needed) and re-project bbx to dd
    bbx        <- proj_expand(locations, prj, expand)
    base_url   <- "https://s3-tilemap.s3.amazonaws.com/geotiff-prod"
    tiles      <- get_hand_xy(bbx)

    hand_urls  <- sprintf("%s/hand/0/%s/%s.tif",
                          base_url,
                          tiles[, 2],
                          tiles[, 1])

    hand_list  <- vector("list", length = nrow(tiles))

    if (verbose) {
        pb  <- progress::progress_bar$new(
            format = "Downloading HAND [:bar] :percent eta: :eta",
            total  = length(hand_urls),
            clear  = FALSE,
            width  = 60
        )
    }

    for (i in seq_along(hand_urls)) {

        if (verbose) pb$tick()

        tmpfile <- tempfile(fileext = ".tif")
        resp <- httr::GET(
            hand_urls[i],
            httr::user_agent(
                "FloodMapping (https://github.com/mikejohnson51/FloodMapping)"
            ),
            httr::write_disk(tmpfile, overwrite = TRUE),
            ...
        )

        # nocov start
        if (!grepl("image/tif", httr::http_type(resp))) {
            stop(
                paste("This url:", hand_urls[i], "did not return a tif"),
                .call = FALSE
            )
        }
        # nocov end

        hand_list[[i]] <- tmpfile
    }

    merge_hand_tiles(hand_list, target_prj = prj, verbose = verbose)
}

#' @title Merge Rasters
#'
#' @description
#' Merge multiple downloaded raster files into a single file.
#' The input `target_prj` describes the projection for the new grid.
#'
#' @param raster_list a list of raster file paths to be mosaiced
#' @param target_prj the target projection of the output raster
#' @param method the method for resampling/reprojecting. Default is 'bilinear'.
#'               Options can be found on the GDAL documentation site
#'               for `gdalwarp`.
#' @param returnRaster if TRUE, return a raster object (default),
#'                     else, return the file path to the object
#' @param verbose Verbose output
#' @export
#' @keywords internal
merge_hand_tiles <- function(raster_list,
                             target_prj,
                             method = "bilinear",
                             return_raster = TRUE,
                             verbose = TRUE) {

    if (verbose) message("Mosaicing & Projecting")

    destfile <- tempfile(fileext = ".tif")
    files    <- unlist(raster_list)

    if (is.null(target_prj)) {
        r <- raster::raster(files[1])
        target_prj <- raster::crs(r)
    }

    sf::gdal_utils(
        util = "warp",
        source = files,
        destination = destfile,
        options = c(
            "-t_srs",
            as.character(target_prj),
            "-r",
            method
        )
    )

    if (return_raster) {
        raster::raster(destfile)
    } else {
        destfile
    }
}

#' @title Get HAND Tile XY Coordinates
#' @param aoi Area of interest
#' @keywords internal
get_hand_xy <- function(aoi) {
    # Extent of HAND/Catchmask Rasters
    ext <- data.frame(xmin = -89.95326,
                      xmax = -83.30852,
                      ymin = 29.48632,
                      ymax = 35.97485)

    if ("bbox" %in% class(aoi)) {
        bb <- aoi
    } else if ("sf" %in% class(aoi)) {
        bb <- sf::st_transform(aoi, 4326) %>%
                   sf::st_bbox()
    } else {
        bb <- aoi %>%
              sf::st_as_sf(
                  coords = c(1, 2),
                  crs = 4326
              ) %>%
              sf::st_bbox()
    }

    if (any(bb$xmin < ext$xmin,
            bb$xmax > ext$xmax,
            bb$ymin < ext$ymin,
            bb$ymax > ext$ymax)) {

        warning(
            paste("HAND tiles are only available for",
                  "the HUC6 regions overlaying Alabama.")
        )

    }

    # Number of columns/rows of HAND/Catchmask Rasters
    xnum   <- floor(280.3281)
    ynum   <- floor(273.7383)

    # Get all Lat/Lon coordinates
    xarray <- seq(ext$xmin, ext$xmax, length.out = xnum)
    yarray <- seq(ext$ymin, ext$ymax, length.out = ynum)

    # Find indices from intersections
    xs <- which.min(abs(xarray - bb$xmin)):which.min(abs(xarray - bb$xmax))
    ys <- which.min(abs(yarray - bb$ymin)):which.min(abs(yarray - bb$ymax))
    ys <- ceiling(ynum - ys)
    # Extend tiles needed to ensure we cover AOI
    ys <- c(ys[1] + 1, ys, ys[length(ys)] - 1)

    # Grid XY coordinates and return
    xy_tiles <- expand.grid(xs, ys)

    xy_tiles
}

#' function to check input type and projection.  All input types convert to a
#' SpatialPointsDataFrame for point elevation and bbx for raster.
#'
#' @keywords internal
loc_check <- function(locations, prj = NULL) {
    loc_wkt <- sf::st_crs(locations)$wkt

    if ("sf" %in% class(locations)) {
        if (!is.null(prj)) {
            locations <- sf::st_transform(locations, crs = prj$wkt)
        }
    } else if (class(locations) == "data.frame") {
        if (is.null(prj)) {
            stop("Please supply a valid WKT string.")
        }
        if (ncol(locations) > 2) {
            df <- data.frame(locations[, 3:ncol(locations)],
                             vector("numeric", nrow(locations)))
            names(df) <- c(names(locations)[3:ncol(locations)],
                           "hand")
        } else {
            df <- data.frame(vector("numeric", nrow(locations)))
            names(df) <- "hand"
        }

        locations <- sf::st_as_sf(
            x = cbind(sf::st_coordinates(locations[, 1:2]), df),
            coords = c("X", "Y"),
            wkt = prj$wkt,
        )
    } else if (attributes(class(locations)) %in% c("raster")) {
        if (
            (is.null(loc_wkt) | nchar(loc_wkt) == 0 | is.na(loc_wkt)) &
            is.null(prj)
        ) {
            stop("Please supply a valid WKT string.")
        }

        if (is.null(loc_wkt) | nchar(loc_wkt) == 0 | is.na(loc_wkt)) {
            if (sum(!is.na(raster::getValues(locations))) == 0) {
                stop("No distinct points, all values NA.")
            } else {
                locations <-
                    raster::rasterToPoints(locations) %>%
                    sf::st_as_sf(coords = c("x", "y"), wkt = prj$wkt)
            }
        }
    }

    locations
}

#' @title Project bounding box and Expand if needed
#'
#' @keywords internal
proj_expand <- function(locations, prj, expand) {

    lll <- grepl("GEOGCRS", prj) |
           grepl("GEODCRS", prj) |
           grepl("GEODETICCRS", prj) |
           grepl("GEOGRAPHICCRS", prj)

    if (any(sf::st_bbox(locations)[c(2, 4)] == 0) & lll & is.null(expand)) {
        # Edge case for lat exactly at the equator - was returning NA
        expand <- 0.01
    } else if (nrow(locations) == 1 & lll & is.null(expand)) {
        # Edge case for single point and lat long
        expand <- 0.01
    } else if (nrow(locations) == 1 & is.null(expand)) {
        # Edge case for single point and projected
        expand <- 1
    }

    bbx <- sf::st_as_sfc(sf::st_bbox(locations))
    if (!is.null(expand)) {
        bbx <- bbx %>%
               sf::st_as_sf() %>%
               sf::st_transform(5070) %>%
               sf::st_buffer(
                   expand,
                   joinStyle = "MITRE",
                   endCapStyle = "SQUARE",
                   mitreLimit = 2
               ) %>%
               sf::st_transform(crs = sf::st_crs(prj)$wkt)
    }

    sf::st_bbox(bbx)
}

#' @title Clip the HAND raster
#' @keywords internal
clip_it <- function(rast, loc, expand, clip) {
    loc_wm <- sf::st_transform(loc, raster::crs(rast))

    if (is.null(clip)) {
        hand <- rast
    } else if (clip == "locations") {
        hand <- raster::mask(raster::crop(rast, loc_wm), loc_wm)
    } else if (clip == "bbox") {
        bbx <- proj_expand(loc_wm, as.character(raster::crs(rast)), expand)
        bbx_sf <- sf::st_transform(
            sf::st_as_sf(sf::st_as_sfc(bbx)),
            raster::crs(rast)
        )
        hand <- raster::mask(raster::crop(rast, bbx_sf), bbx_sf)
    } else {
        hand <- rast
    }

    hand
}

#' @title Estimate download size of DEMs
#' @param locations the locations
#' @param z zoom level if source is aws
#' @keywords internal
estimate_raster_size <- function(locations) {
    locations <- sf::st_bbox(locations) %>%
                 sf::st_as_sfc() %>%
                 sf::st_as_sf() %>%
                 sf::st_transform(sf::st_crs("+init=EPSG:4326"))

    res <- c(0.54905236, 0.27452618, 0.15455633,
             0.07145545, 0.03719130, 0.01901903,
             0.00962056, 0.00483847, 0.00241219,
             0.00120434, 0.00060173, 0.00030075,
             0.00015035, 0.00007517, 0.00003758)

    bb       <- sf::st_bbox(locations)
    num_rows <- (bb$xmax - bb$xmin) / res
    num_cols <- (bb$ymax - bb$ymin) / res

    num_megabytes <- (num_rows * num_cols * 32) / 8388608
    sum(num_megabytes)
}