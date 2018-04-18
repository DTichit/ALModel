##' Fonction \code{calcul_pm}
##'
##' Cette fonction permet de determiner le montant total des PM dans les differents portefeuilles de la compagnie d'assurance.
##'
##' @name calcul_pm
##' @docType methods
##' @param ptf_passif est un objet de type \code{\link{PTFPassif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PTFPassif-class.R
##'
setGeneric(name = "calcul_pm", def = function(ptf_passif) {standardGeneric("calcul_pm")})
setMethod(
    f = "calcul_pm",
    signature = c(ptf_passif = "PTFPassif"),
    definition = function(ptf_passif){



        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion des differents passifs
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Epargne
        ## ###########################

        # Somme des PMs
        pm_epargne <- sum(ptf_passif@epargne@ptf$pm)





        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        pm = list(epargne = pm_epargne)


        # Output
        return(pm)
    }
)
