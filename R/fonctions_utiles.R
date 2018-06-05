##' Fonction \code{psum}
##'
##' Cette fonction permet de faire la somme, sur plusieurs vecteurs, element par element, et pouvant supprimer les NAs.
##'
##' @name psum
##' @docType methods
##' @param ... les differnts vecteurs.
##' @param na.rm est un \code{logical} qui, lorsqu'il est a TRUE, permet a la somme de ne pas prendre en compte les NAs.
##' @author Damien Tichit pour Sia Partners
psum <- function(..., na.rm = FALSE) {

    # Somme
    rowSums(do.call(cbind, list(...)), na.rm = na.rm)

}



##' Fonction \code{pna.omit}
##'
##' Cette fonction, prenant deux vecteurs, sort un seul vecteur sans NAs.
##'
##' @name pna.omit
##' @docType methods
##' @param x premier vecteur.
##' @param y second vecteur.
##' @author Damien Tichit pour Sia Partners
pna.omit <- function(x, y) {

    # Id etant NA chez y
    id_na <- which(is.na(y))

    # MaJ de x
    y[id_na] <- x[id_na]

    # Output
    return(y)
}



ch2numFunct<-function(x){
    x %>% stringr::str_replace(",",".") %>% as.numeric
}


if.is_null <- function(x, replace) {

    if(is.null(x))
        out <- replace
    else
        out <- x

    return(out)
}


if.is_na <- function(x, replace) {

    if(is.na(x))
        out <- replace
    else
        out <- x

    return(out)
}


if.is_empty <- function(x, replace) {

    if(length(x) == 0L)
        out <- replace
    else
        out <- x

    return(out)
}




sum_list <- function(list, p) {

    # Calcul de la somme en fonction de la profondeur de la liste
    if(p == 1L)
        res <- do.call(sum, list)
    else if(p == 2L)
        res <- sum(sapply(names(list), function(x) do.call(sum, list[[x]])))
    else if(p == 3L)
        res <- sum(unlist(list))
    else
        stop("Calcul de la somme non codee pour cette profondeur")

    # Output
    return(res)
}


newton_raphson <- function(fun, x0 = 0, delta = 0.01, prec = 0.0001) {

    eps <- 1

    while (eps > prec) {

        x0 <- x0 - fun(x0) * (delta / (fun(x0 + delta) - fun(x0)))

        eps <- abs(fun(x0))
    }

    return(x0)
}


dicho <- function(fun, x0, x1, prec = 0.001) {

    f_x0 <- fun(x0)
    f_x1 <- fun(x1)

    while (eps > prec) {

        f_mean <- fun((x0+x1)/2)

        if(f_mean > 0) {

            x0 <- (x0+x1)/2
            f_x0 <- f_mean

        } else {

            x1 <- (x0+x1)/2
            f_x1 <- f_mean

        }

        eps <- abs(x0-x1)
    }

    return(x0)
}



# microbenchmark::microbenchmark(dicho(fun, -0.5, 0.5),newton_raphson(fun, 0, 0.01))
