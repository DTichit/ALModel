
# Fonction permettant de faire la somme element par element
psum <- function(..., na.rm = FALSE) {

    # Somme
    rowSums(do.call(cbind, list(...)), na.rm = na.rm)

}


# Fonction qui, prenant 2 vecteurs, supprime les NA
pna.omit <- function(x, y) {

    # Id etant NA chez y
    id_na <- which(is.na(y))

    # MaJ de x
    y[id_na] <- x[id_na]

    # Output
    return(y)
}
