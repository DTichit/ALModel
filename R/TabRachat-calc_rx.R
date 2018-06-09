##' Fonction \code{calc_rx}.
##'
##' Cette fonction permet de calculer des probas de rachat pour une anciennete et une table de rachat donnes.
##' Il est possible d'indiquer plusieurs ages-anciennete differents sous forme de vecteur.
##'
##' @name calc_rx
##' @docType methods
##' @param tab_rachat est un objet de type \code{\link{TabRachat}}.
##' @param age est un \code{integer}.
##' @param anc est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include TabRachat-class.R
##'
setGeneric(name = "calc_rx", def = function(tab_rachat, age, anc) {standardGeneric("calc_rx")})
setMethod(
    f = "calc_rx",
    signature = c(tab_rachat = "TabRachat", age = "integer", anc = "integer"),
    definition = function(tab_rachat, age, anc){

        # Donnees de sorties
        table <- tab_rachat@table
        col_names <- colnames(table)
        age_table <- .subset2(table, which(col_names == "age"))
        anc_table <- .subset2(table, which(col_names == "anc"))
        rx_table  <- .subset2(table, which(col_names == "rx"))

        # Statistique sur la table
        age_min <- min(age_table) ; age_max <- max(age_table)
        anc_min <- min(anc_table) ; anc_max <- max(anc_table)

        # Ages appliques
        age_ap   <- pmax(age_min, pmin(age, age_max))

        # Anciennete appliques
        anc_ap   <- pmax(anc_min, pmin(anc, anc_max))

        # Numeros de ligne du rx
        row_rx   <- match(paste0(age_ap, anc_ap), paste0(age_table, anc_table))

        # Extraction des rx
        rx   <- rx_table[row_rx]

        # Output
        return(rx)
    }
)
