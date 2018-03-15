##' Fonction \code{calc_rx}.
##'
##' Cette fonction permet de calculer des probas de rachat pour un age et une table de rachat donnes.
##' Il est possible d'indiquer plusieurs ages differents sous forme de vecteur.
##'
##' @name calc_rx
##' @docType methods
##' @param tab_rachat est un objet de type \code{\link{TabRachat}}.
##' @param age est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include TabRachat-class.R
##'
setGeneric(name = "calc_rx", def = function(tab_rachat, age) {standardGeneric("calc_rx")})
setMethod(
    f = "calc_rx",
    signature = c(tab_rachat = "TabRachat", age = "integer"),
    definition = function(tab_rachat, age){

        # Extraction de donnees
        table <- tab_rachat@table
        col_names <- colnames(table)
        age_table <- .subset2(table, which(col_names == "age"))
        rx_table  <- .subset2(table, which(col_names == "rx"))

        # Age maximum de la table
        age_max <- max(age_table)

        # Ages appliques
        age_ap   <- pmin(age, age_max)

        # Numeros de ligne du lx et du lx+1
        row_rx   <- match(age_ap, age_table)

        # Extraction des rx
        rx   <- rx_table[row_rx]

        # Output
        return(rx)
    }
)
