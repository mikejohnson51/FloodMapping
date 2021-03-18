# * -------------------------------------------------------------
# * Unit Tests for: get_hand_raster()
# * -------------------------------------------------------------
test_that("get_hand_raster() returns a raster", {
    expect_s4_class(
        ab_raster,
        "RasterLayer"
    )
})

test_that("get_hand_raster() returns the correct/usable extent", {
    expect_true(
        check_extent(
            inside_bb  = sf::st_bbox(appling_bend),
            outside_bb = sf::st_bbox(ab_raster)
        )
    )
})

test_that("get_hand_raster does not return verbose output", {
    expect_silent(
        get_hand_raster(appling_bend, verbose = FALSE)
    )
})

test_that("get_hand_raster() returns with verbose output", {
    expect_message(
        get_hand_raster(appling_bend, verbose = TRUE)
    )
})

test_that("get_hand_raster() does not contain negative values", {
    expect_true(
        all(get_hand_raster(appling_bend, neg_to_na = TRUE)[] >= 0)
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
# * -------------------------------------------------------------``
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