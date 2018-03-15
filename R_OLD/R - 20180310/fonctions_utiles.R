
# Fonction permettant de faire la somme element par element
psum <- function(..., na.rm = FALSE) {

    # Somme
    rowSums(do.call(cbind, list(...)), na.rm = na.rm)

}
