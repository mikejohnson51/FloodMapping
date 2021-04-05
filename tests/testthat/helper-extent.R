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