aoi_outside_boundaries <- AOI::aoi_get(x = "UCSB")
aoi_inside_boundaries  <- AOI::aoi_get(x = "Mobile River")

# Known tiles for Mobile River, outputted via dput()
expected_aoi_inside <- structure(
    list(
        Var1 = c(
            81L, 82L, 83L, 84L, 85L, 81L, 82L, 83L,
            84L, 85L, 81L, 82L, 83L, 84L, 85L, 81L,
            82L, 83L, 84L, 85L, 81L, 82L, 83L, 84L,
            85L, 81L, 82L, 83L, 84L, 85L, 81L, 82L,
            83L, 84L, 85L, 81L, 82L, 83L, 84L, 85L,
            81L, 82L, 83L, 84L, 85L, 81L, 82L, 83L,
            84L, 85L, 81L, 82L, 83L, 84L, 85L, 81L,
            82L, 83L, 84L, 85L, 81L, 82L, 83L, 84L,
            85L, 81L, 82L, 83L, 84L, 85L, 81L, 82L,
            83L, 84L, 85L, 81L, 82L, 83L, 84L, 85L,
            81L, 82L, 83L, 84L, 85L, 81L, 82L, 83L,
            84L, 85L, 81L, 82L, 83L, 84L, 85L, 81L,
            82L, 83L, 84L, 85L, 81L, 82L, 83L, 84L,
            85L, 81L, 82L, 83L, 84L, 85L, 81L, 82L,
            83L, 84L, 85L
        ),
        Var2 = c(
            224, 224, 224, 224, 224, 223, 223, 223,
            223, 223, 222, 222, 222, 222, 222, 221,
            221, 221, 221, 221, 220, 220, 220, 220,
            220, 219, 219, 219, 219, 219, 218, 218,
            218, 218, 218, 217, 217, 217, 217, 217,
            216, 216, 216, 216, 216, 215, 215, 215,
            215, 215, 214, 214, 214, 214, 214, 213,
            213, 213, 213, 213, 212, 212, 212, 212,
            212, 211, 211, 211, 211, 211, 210, 210,
            210, 210, 210, 209, 209, 209, 209, 209,
            208, 208, 208, 208, 208, 207, 207, 207,
            207, 207, 206, 206, 206, 206, 206, 205,
            205, 205, 205, 205, 204, 204, 204, 204,
            204, 203, 203, 203, 203, 203, 202, 202,
            202, 202, 202
        )
    ),
    out.attrs = list(
        dim = c(5L, 23L),
        dimnames = list(
            Var1 = c(
                "Var1=81",
                "Var1=82",
                "Var1=83",
                "Var1=84",
                "Var1=85"),
            Var2 = c(
                "Var2=224", "Var2=223", "Var2=222", "Var2=221",
                "Var2=220", "Var2=219", "Var2=218", "Var2=217",
                "Var2=216", "Var2=215", "Var2=214", "Var2=213",
                "Var2=212", "Var2=211", "Var2=210", "Var2=209",
                "Var2=208", "Var2=207", "Var2=206", "Var2=205",
                "Var2=204", "Var2=203", "Var2=202"
            )
        )
    ),
    class = "data.frame",
    row.names = c(NA, -115L)
)

# Smaller AOI than Mobile River, for faster tests
appling_bend      <- AOI::aoi_get(x = "Appling Bend")
single_good_comid <- 18523225

# Both object and expected should
# be of class `bbox`, returned from sf::st_bbox().
check_extent <- function(inside_bb, outside_bb) {
    # Check if inside object is
    # contained within outside object.
    all(
        inside_bb$xmin >= outside_bb$xmin,
        inside_bb$xmax <= outside_bb$xmax,
        inside_bb$ymin >= outside_bb$ymin,
        inside_bb$ymax <= outside_bb$ymax
    )
}