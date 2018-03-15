##' Fonction \code{calc_loyer}
##'
##' Cette fonction permet de calculer les loyers pour un portefeuille d'immobiliers.
##'
##' @name calc_loyer
##' @docType methods
##' @param immobilier est un objet de type \code{\link{Immobilier}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R
##'
setGeneric(name = "calc_loyer", def = function(immobilier) {standardGeneric("calc_loyer")})
setMethod(
    f = "calc_loyer",
    signature = c(immobilier = "Immobilier"),
    definition = function(immobilier){

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf_immobilier <- names(immobilier@ptf)



        ## ###########################
        ##          Loyers
        ## ###########################

        # Extraction de donnees
        loyer_ptf           <- .subset2(immobilier@ptf, which(name_ptf_immobilier == "loyer"))
        nb_ptf              <- .subset2(immobilier@ptf, which(name_ptf_immobilier == "nombre"))

        # Calcul des loyers
        loyers <- nb_ptf * loyer_ptf



        # Output
        return(list(loyer = loyers))
    }
)
