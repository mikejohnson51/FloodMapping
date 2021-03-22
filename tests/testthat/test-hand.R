# * -------------------------------------------------------------
# * Unit Tests for: get_hand_raster()
# * These are skipped on CRAN and CI environments
# * -------------------------------------------------------------
test_that("get_hand_raster() returns a raster", {
    skip_on_cran()
    skip_on_ci()
    expect_s4_class(
        get_hand_raster(appling_bend, verbose = FALSE),
        "RasterLayer"
    )
})

test_that("get_hand_raster() returns the correct/usable extent", {
    skip_on_cran()
    skip_on_ci()
    expect_true(
        check_extent(
            inside_bb  = sf::st_bbox(appling_bend),
            outside_bb = sf::st_bbox(ab_raster)
        )
    )
})

test_that("get_hand_raster() does not return verbose output", {
    skip_on_cran()
    skip_on_ci()
    expect_silent(
        get_hand_raster(appling_bend, verbose = FALSE)
    )
})

test_that("get_hand_raster() returns with verbose output", {
    skip_on_cran()
    skip_on_ci()
    suppressMessages(expect_message(
        get_hand_raster(appling_bend, verbose = TRUE)
    ))
})

test_that("get_hand_raster() does not contain negative values", {
    skip_on_cran()
    skip_on_ci()
    temp_raster <- get_hand_raster(appling_bend, neg_to_na = TRUE)
    expect_true(
        all(is.na(temp_raster[temp_raster < 0]))
    )
})

# * -------------------------------------------------------------
# * Unit Tests for: get_hand_xy()
# * -------------------------------------------------------------
test_that("get_hand_xy() returns a warning outside boundaries", {
    # Expect some warning is retrieved for `sf` object
    expect_warning(
        get_hand_xy(aoi_outside_boundaries)
    )
    # Expect some warning is retrieved for a `bbox` object
    expect_warning(
        get_hand_xy(sf::st_bbox(aoi_outside_boundaries))
    )
})

test_that("get_hand_xy() returns a specific warning outside boundaries", {
    # Expect a specific warning is retrieved for a `sf` object
    expect_warning(
        get_hand_xy(aoi_outside_boundaries),
        paste("HAND tiles are only available for",
                "the HUC6 regions overlaying Alabama.")
    )

    # Expect a specific warning is retrieved for a `bbox` object
    expect_warning(
        get_hand_xy(sf::st_bbox(aoi_outside_boundaries)),
        paste("HAND tiles are only available for",
                "the HUC6 regions overlaying Alabama.")
    )
})

test_that("get_hand_xy() returns a nonempty data frame of coordinates", {
    # Expect data.frame for `sf` object
    expect_s3_class(
        get_hand_xy(aoi_inside_boundaries),
        "data.frame"
    )

    # Expect data.frame for `bbox` object
    expect_s3_class(
        get_hand_xy(sf::st_bbox(aoi_inside_boundaries)),
        "data.frame"
    )

    # Expect data.frame for `data.frame` object
    expect_s3_class(
        get_hand_xy(
            as.data.frame(sf::st_coordinates(aoi_inside_boundaries))
        ),
        "data.frame"
    )
})

test_that("get_hand_xy() works the same for sf and bbox objects", {
    # Expect get_hand_xy() for `sf` is same as known tiles
    expect_identical(
        get_hand_xy(aoi_inside_boundaries),
        expected_aoi_inside
    )

    # Expect get_hand_xy() for `bbox` is same as known tiles
    expect_identical(
        get_hand_xy(sf::st_bbox(aoi_inside_boundaries)),
        expected_aoi_inside
    )

    # Expect get_hand_xy() for `data.frame` is same as known tiles
    expect_identical(
        get_hand_xy(
            as.data.frame(sf::st_coordinates(aoi_inside_boundaries))
        ),
        expected_aoi_inside
    )
})

# * -------------------------------------------------------------
# * Unit Tests for: proj_expand()
# * -------------------------------------------------------------
test_that("proj_expand() returns a bounding box", {
    # Expect class of `bbox` from `sf`
    expect_s3_class(
        proj_expand(appling_bend, sf::st_crs(appling_bend)$wkt, NULL),
        "bbox"
    )
})

test_that("proj_expand() buffers correctly", {
    # Expect 50m, 100m, 500m, and 1000m to buffer correctly
    for (amt in c(50, 100, 500, 1000)) {
        expect_true(
            check_buffer(
                origin = sf::st_bbox(appling_bend),
                expand = proj_expand(
                    appling_bend,
                    sf::st_crs(appling_bend)$wkt,
                    amt
                ),
                buffer_amt = amt
            )
        )
    }
})

# * -------------------------------------------------------------
# * Unit Tests for: clip_it()
# * -------------------------------------------------------------
test_that("clip_it() returns a raster", {
    expect_s4_class(
        clip_it(ab_raster, appling_bend, NULL, NULL),
        "RasterLayer"
    )
})

# Computationally expensive test...
# need to figure out how to make these better
#> test_that("clip_it() clips to locations", {
#>     tmp_aoi  <- AOI::aoi_get(county = "Etowah", state = "AL") %>%
#>                 AOI::aoi_buffer(-10)
#>
#>     tmp_rast <- get_hand_raster(tmp_aoi, verbose = FALSE) > -Inf
#>
#>     tmp_inside <- clip_it(tmp_rast, tmp_aoi, NULL, "locations") %>%
#>                   raster::boundaries() %>%
#>                   raster::rasterToPoints() %>%
#>                   as.data.frame() %>%
#>                   st_as_sf(
#>                       coords = c("x", "y"),
#>                       crs = sf::st_crs(tmp_rast)$wkt
#>                   ) %>%
#>                   sf::st_combine() %>%
#>                   sf::st_union() %>%
#>                   sf::st_cast("POLYGON") %>%
#>                   sf::st_as_sf() %>%
#>                   sf::st_transform(5070) %>%
#>                   sf::st_buffer(0.0001) %>%
#>                   sf::st_transform(crs = sf::st_crs(tmp_rast)$wkt)
#>
#>     expect_true(AOI::aoi_inside(tmp_inside, tmp_aoi))
#> })

#> test_that("clip_it() clips to bbox", {
#>     tmp_inside <- clip_it(ab_raster, appling_bend, NULL, "bbox") %>%
#>                   sf::st_bbox() %>%
#>                   sf::st_as_sfc() %>%
#>                   sf::st_as_sf()
#>
#>     tmp_outside <- appling_bend %>%
#>                    sf::st_bbox() %>%
#>                    sf::st_as_sfc() %>%
#>                    sf::st_as_sf()
#>
#>     expect_true(AOI::aoi_inside(tmp_inside, tmp_outside))
#> })

# * -------------------------------------------------------------
# * Unit Tests for: loc_check()
# * -------------------------------------------------------------

test_that("loc_check() returns a `sf` object", {
    # `sf`
    expect_s3_class(loc_check(appling_bend), "sf")
    expect_s3_class(loc_check(appling_bend, prj = 5070), "sf")

    # `raster`
    expect_s3_class(loc_check(ab_raster), "sf")
    expect_s3_class(loc_check(ab_raster, prj = 5070), "sf")

    # `bbox`
    expect_s3_class(loc_check(sf::st_bbox(appling_bend)), "sf")
    expect_s3_class(loc_check(sf::st_bbox(appling_bend), prj = 5070), "sf")

    # `data.frame` or `matrix`
    expect_s3_class(
        loc_check(sf::st_coordinates(appling_bend), prj = 4326),
        "sf"
    )
    expect_s3_class(
        loc_check(
            as.data.frame(sf::st_coordinates(appling_bend)),
            prj = 4326
        ),
        "sf"
    )
})

test_that("loc_check() projects to different crs", {
    # `sf`
    expect_true(
        loc_check(appling_bend, prj = 5070) %>%
            sf::st_crs() %>%
            `$`(epsg) %>%
            `==`(5070)
    )

    # `raster`
    expect_true(
        loc_check(ab_raster, prj = 5070) %>%
            sf::st_crs() %>%
            `$`(epsg) %>%
            `==`(5070)
    )

    # `bbox`
    expect_true(
        loc_check(sf::st_bbox(appling_bend), prj = 5070) %>%
            sf::st_crs() %>%
            `$`(epsg) %>%
            `==`(5070)
    )
})

test_that("loc_check() throws correct errors", {
    temp <- appling_bend
    sf::st_crs(temp) <- NA
    expect_error(loc_check(temp))

    sf::st_crs(temp) <- ""
    expect_error(loc_check(temp))

    expect_error(loc_check("test"))
    expect_error(loc_check(123))
})

test_that("loc_check() finds coordinate columns in data.frame", {
    coords <- sf::st_coordinates(appling_bend)
    expect_s3_class(loc_check(coords, prj = 4326), "sf")
    expect_s3_class(loc_check(as.data.frame(coords), prj = 4326), "sf")

    colnames(coords) <- c("x", "y", "L1", "L2")
    expect_s3_class(loc_check(coords, prj = 4326), "sf")
    expect_s3_class(loc_check(as.data.frame(coords), prj = 4326), "sf")

    colnames(coords) <- c("lon", "lat", "L1", "L2")
    expect_s3_class(loc_check(coords, prj = 4326), "sf")
    expect_s3_class(loc_check(as.data.frame(coords), prj = 4326), "sf")

    colnames(coords) <- c("longitude", "latitude", "L1", "L2")
    expect_s3_class(loc_check(coords, prj = 4326), "sf")
    expect_s3_class(loc_check(as.data.frame(coords), prj = 4326), "sf")

    colnames(coords) <- c("unknownx", "unknowny", "L1", "L2")
    expect_error(loc_check(coords, prj = 4326))
    expect_error(loc_check(as.data.frame(coords), prj = 4326))
})

# * -------------------------------------------------------------
# * Unit Tests for: estimate_raster_size()
# * -------------------------------------------------------------

test_that("estimate_raster_size() returns a single element", {
    expect_true(length(estimate_raster_size(appling_bend)) <= 1)
})

test_that("estimate_raster_size() is nonnegative", {
    expect_true(estimate_raster_size(appling_bend) >= 0)
    expect_true(estimate_raster_size(ab_raster) >= 0)
})