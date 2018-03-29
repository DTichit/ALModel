##' Fonction \code{aggregation_passif_2}.
##'
##' Cette fonction permet d'effectuer une premiere aggregation des portfeuilles passifs d'une compagnie d'assurance.
##'
##' @name aggregation_passif_2
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "aggregation_passif_2", def = function(passif) {standardGeneric("aggregation_passif_2")})
setMethod(
    f = "aggregation_passif_2",
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

        passif@ptf_passif@epargne <- aggregation_epargne_2(passif@ptf_passif@epargne)


        # Output
        return(passif)
    }
)
