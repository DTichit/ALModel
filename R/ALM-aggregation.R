##' Fonction \code{aggregation_alm}.
##'
##' Cette fonction permet de faire appel aux différentes fonctions permettant d'aggreger les portefeuilles.
##'
##' @name aggregation_alm
##' @docType methods
##' @param alm est un objet de type \code{\link{ALM}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include ALM-class.R
##'
setGeneric(name = "aggregation_alm", def = function(alm) {standardGeneric("aggregation_alm")})
setMethod(
    f = "aggregation_alm",
    signature = c(alm = "ALM"),
    definition = function(alm){

        # Appel de la fonction permettant d'aggreger les differents PTF d'actifs
        alm@system@actif <- aggregation_actif(alm@system@actif)


        # Appel de la fonction permettant d'aggreger les differents PTF de passifs
        alm@system@passif <- aggregation_passif_1(alm@system@passif)

        # Output
        return(alm)
    }
)
