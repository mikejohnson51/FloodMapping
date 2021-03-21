# Approximation, give allowance of (buffer_amt + (buffer_amt / 3)).
# Both origin and expand should be of class `bbox`
check_buffer <- function(origin, expand, buffer_amt) {
    differ <- abs(expand - origin) * 111111
    all(differ <= (buffer_amt + (buffer_amt / 3)))
}