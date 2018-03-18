##' Fonction \code{aggregation_passif}.
##'
##' Cette fonction permet de faire appel aux différentes fonctions permettant d'aggreger les portfeuilles passifs d'une compagnie d'assurance.
##'
##' @name aggregation_passif
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "aggregation_passif", def = function(passif) {standardGeneric("aggregation_passif")})
setMethod(
    f = "aggregation_passif",
    signature = c(passif = "Passif"),
    definition = function(passif){

        ## ######################################################
        ## ######################################################
        ##
        ##       Aggregation des differents portefeuilles
        ##
        ## ######################################################
        ## ######################################################


        ## ###########################
        ##          Epargne
        ## ###########################

        passif@ptf_passif@epargne <- aggregation_epargne(passif@ptf_passif@epargne)


        # Output
        return(passif)
    }
)
