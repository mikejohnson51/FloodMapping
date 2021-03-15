# * -------------------------------------------------------------
# * Unit Tests for: get_hand_raster()
# * -------------------------------------------------------------
# Instantiate once so we don't have repeat requests
ab_raster <- get_hand_raster(appling_bend, verbose = FALSE)

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