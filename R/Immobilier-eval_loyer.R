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
setGeneric(name = "eval_loyer", def = function(immobilier) {standardGeneric("eval_loyer")})
setMethod(
    f = "eval_loyer",
    signature = c(immobilier = "Immobilier"),
    definition = function(immobilier){

        warning("Fonction a coder : voir l'ESG")

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf_immobilier <- names(immobilier@ptf)



        ## ###########################
        ##          Loyers
        ## ###########################
        loyers <- rep(0, nrow(immobilier@ptf))



        # Output
        return(list(loyer = sum(loyers)))
    }
)
