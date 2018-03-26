##' Fonction \code{aggregation_passif_1}.
##'
##' Cette fonction permet de faire appel aux différentes fonctions permettant d'aggreger les portfeuilles passifs d'une compagnie d'assurance.
##'
##' @name aggregation_passif_1
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "aggregation_passif_1", def = function(passif) {standardGeneric("aggregation_passif_1")})
setMethod(
    f = "aggregation_passif_1",
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

        passif@ptf_passif@epargne <- aggregation_epargne_1(passif@ptf_passif@epargne)


        # Output
        return(passif)
    }
)















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
