##' Fonction \code{aggregation_immobilier}.
##'
##' Cette fonction permet d'aggreger les model-point pour un portfeuille d'actions.
##'
##' @name aggregation_immobilier
##' @docType methods
##' @param immobilier est un objet de type \code{\link{Immobilier}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R
##'
setGeneric(name = "aggregation_immobilier", def = function(immobilier) {standardGeneric("aggregation_immobilier")})
setMethod(
    f = "aggregation_immobilier",
    signature = c(immobilier = "Immobilier"),
    definition = function(immobilier){

        # Aggregation des donnees
        temp <- colSums(immobilier@ptf[2L:ncol(immobilier@ptf)])

        # Creation du dataframe
        immobilier@ptf <- data.frame(id_mp = paste("im", 1L, sep = "-"), valeur_comptable = temp[["valeur_comptable"]],
                                     valeur_marche = temp[["valeur_marche"]])


        # Output
        return(immobilier)
    }
)
