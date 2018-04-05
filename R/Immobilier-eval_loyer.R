##' Fonction \code{eval_loyer}
##'
##' Cette fonction permet de calculer les loyers pour un portefeuille d'immobiliers.
##'
##' @name eval_loyer
##' @docType methods
##' @param immobilier est un objet de type \code{\link{Immobilier}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R
##'
setGeneric(name = "eval_loyer", def = function(immobilier, loyer) {standardGeneric("eval_loyer")})
setMethod(
    f = "eval_loyer",
    signature = c(immobilier = "Immobilier", loyer = "numeric"),
    definition = function(immobilier, loyer){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf_immobilier <- names(immobilier@ptf)



        ## ###########################
        ##          Loyers
        ## ###########################
        loyers <- .subset2(immobilier@ptf, which(name_ptf_immobilier == "valeur_marche")) * loyer



        # Output
        return(list(loyer = sum(loyers)))
    }
)
