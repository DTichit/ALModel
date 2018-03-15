##' Fonction \code{proj_1an_immobilier}
##'
##' Cette fonction permet de projeter horizon 1 an un portefeuille d'immobiliers. Elle calcule notamment les loyers.
##'
##' @name proj_1an_immobilier
##' @docType methods
##' @param immobilier est un objet de type \code{\link{Immobilier}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R
##'
setGeneric(name = "proj_1an_immobilier", def = function(immobilier) {standardGeneric("proj_1an_immobilier")})
setMethod(
    f = "proj_1an_immobilier",
    signature = c(immobilier = "Immobilier"),
    definition = function(immobilier){

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf_immobilier <- names(immobilier@ptf)



        ## ######################################################
        ## ######################################################
        ##
        ##              Evaluation des loyers
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        loyer_ptf           <- .subset2(immobilier@ptf, which(name_ptf_immobilier == "loyer"))
        nb_ptf              <- .subset2(immobilier@ptf, which(name_ptf_immobilier == "nombre"))

        # Calcul des loyers
        loyers <- nb_ptf * loyer_ptf



        # Output
        return(list(immobilier = immobilier,
                    flux = list(loyer = sum(loyers))))
    }
)
