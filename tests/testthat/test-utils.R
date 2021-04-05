# * -------------------------------------------------------------
# * Unit Tests for: findHUC6()
# * -------------------------------------------------------------
test_that("findHUC6() finds the correct HUC6", {
    expect_identical(findHUC6(appling_bend)$huc6, "031601")
})

test_that("findHUC6() throws error if CRS is missing for AOI", {
    temp <- appling_bend
    sf::st_crs(temp) <- NA

    expect_error(findHUC6(temp))
})

