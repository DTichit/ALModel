##' Fonction \code{aggregation_actif}.
##'
##' Cette fonction permet de faire appel aux différentes fonctions permettant d'aggreger les portfeuilles financiers
##' d'une compagnie d'assurance.
##'
##' @name aggregation_actif
##' @docType methods
##' @param actif est un objet de type \code{\link{Actif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Actif-class.R
##'
setGeneric(name = "aggregation_actif", def = function(actif) {standardGeneric("aggregation_actif")})
setMethod(
    f = "aggregation_actif",
    signature = c(actif = "Actif"),
    definition = function(actif){

        ## ######################################################
        ## ######################################################
        ##
        ##       Aggregation des differents portefeuilles
        ##
        ## ######################################################
        ## ######################################################



        ## ###########################
        ##          Actions
        ## ###########################

        actif@ptf_actif@action <- aggregation_action(actif@ptf_actif@action)



        ## ###########################
        ##         Obligations
        ## ###########################

        actif@ptf_actif@obligation <- aggregation_obligation(actif@ptf_actif@obligation)



        ## ###########################
        ##         Immobilier
        ## ###########################

        actif@ptf_actif@immobilier <- aggregation_immobilier(actif@ptf_actif@immobilier)


        # Output
        return(actif)
    }
)
