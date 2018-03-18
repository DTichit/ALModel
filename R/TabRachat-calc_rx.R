##' Fonction \code{calc_rx}.
##'
##' Cette fonction permet de calculer des probas de rachat pour une anciennete et une table de rachat donnes.
##' Il est possible d'indiquer plusieurs ages differents sous forme de vecteur.
##'
##' @name calc_rx
##' @docType methods
##' @param tab_rachat est un objet de type \code{\link{TabRachat}}.
##' @param anc est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include TabRachat-class.R
##'
setGeneric(name = "calc_rx", def = function(tab_rachat, anc) {standardGeneric("calc_rx")})
setMethod(
    f = "calc_rx",
    signature = c(tab_rachat = "TabRachat", anc = "integer"),
    definition = function(tab_rachat, anc){

        # Extraction de donnees
        table <- tab_rachat@table
        col_names <- colnames(table)
        anc_table <- .subset2(table, which(col_names == "anc"))
        rx_table  <- .subset2(table, which(col_names == "rx"))

        # Anciennete maximale de la table
        anc_max <- max(anc_table)

        # Anciennete appliques
        anc_ap   <- pmin(anc, anc_max)

        # Extraction des taux de rachats
        row_rx   <- match(anc_ap, anc_table)

        # Extraction des rx
        rx   <- rx_table[row_rx]

        # Output
        return(rx)
    }
)
