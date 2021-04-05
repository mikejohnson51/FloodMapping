# * -------------------------------------------------------------
# * Unit Tests for: get_src()
# * -------------------------------------------------------------
test_that("get_src() returns a data.frame", {
    expect_s3_class(
        get_src(
            comids = single_good_comid,
            hand = ab_raster,
            stage = 0:1,
            progress = FALSE
        ),
        "data.frame"
    )
})

test_that("get_src() throws error if no comids or hand", {
    expect_error(get_src())
    expect_error(get_src(NULL))
    expect_error(get_src(NULL, NULL))
    expect_error(get_src(hand = NULL))
    expect_error(get_src(NA))
    expect_error(get_src(NA, NA))
    expect_error(get_src(hand = NA))
})

test_that("get_src() grabs HAND raster with COMIDs", {
    expect_success({
        tryCatch({
            get_src(
                comids = single_good_comid,
                progress = FALSE,
                stage = 0:1
            )

            succeed(message = "SRC Generated")
        },
        error = function(cond) {
            fail(message = cond)
        })
    })
})

# This test will try to pull the HAND raster for
# `single_good_comid`.
test_that("get_src() grabs COMIDs with HAND raster", {
    expect_success({
        tryCatch({
            get_src(
                comids = NULL,
                hand = get_hand_raster(
                    nhdplusTools::get_nhdplus(comid = single_good_comid),
                    clip = "locations"
                ),
                progress = FALSE,
                stage = 0:1
            )

            succeed(message = "SRC Generated")
        },
        error = function(cond) {
            fail(message = cond)
        })
    })
})

# * -------------------------------------------------------------
# * Unit Tests for: get_catchmask()
# * -------------------------------------------------------------
test_that("get_catchmask() returns a raster", {
    expect_s4_class(
        get_catchmask(appling_bend, ab_raster),
        "RasterLayer"
    )
})

# * -------------------------------------------------------------
# * Unit Tests for: get_roughness()
# * -------------------------------------------------------------
test_that("get_roughness() returns a double", {
    expect_type(
        get_roughness(1, 1, 1, 1, 1),
        "double"
    )
})

test_that("get_roughness() returns error for any non-numeric params", {
    expect_error(get_roughness("1", 1, 1, 1, 1))
    expect_error(get_roughness(1, "1", 1, 1, 1))
    expect_error(get_roughness(1, 1, "1", 1, 1))
    expect_error(get_roughness(1, 1, 1, "1", 1))
    expect_error(get_roughness(1, 1, 1, 1, "1"))
})

# * -------------------------------------------------------------
# * Unit Tests for: find_comids()
# * -------------------------------------------------------------
test_that("find_comids() returns a double vector", {
    expect_type(
        find_comids(appling_bend),
        "double"
    )
})

# * -------------------------------------------------------------
# * Unit Tests for: compute_cellres()
# * -------------------------------------------------------------
# Need a control for compute_cellres()

# * -------------------------------------------------------------
# * Unit Tests for: compute_flat_tub()
# * -------------------------------------------------------------
# Need a control for compute_flat_tub()