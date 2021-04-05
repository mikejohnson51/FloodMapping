test_data <- readRDS(
    system.file(
        "testdata",
        "test-data.rds",
        package = "FloodMapping"
    )
)

aoi_outside_boundaries <- test_data$outside
aoi_inside_boundaries  <- test_data$inside
expected_aoi_inside    <- test_data$expected
appling_bend           <- test_data$appling

ab_raster <- raster::raster(
    system.file(
        "testdata",
        "appling_bend.tif",
        package = "FloodMapping"
    )
)

single_good_comid <- 18523225