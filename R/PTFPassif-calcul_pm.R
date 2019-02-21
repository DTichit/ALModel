##' Fonction \code{calcul_pm}
##'
##' Cette fonction permet de determiner le montant total des PM dans les differents portefeuilles de la compagnie d'assurance.
##'
##' @name calcul_pm
##' @docType methods
##' @param ptf_passif est un objet de type \code{\link{PTFPassif}}.
##' @param agreg_out est une valeur \code{logical} qui indique si les sorties doivent etre agregees. Par defaut, sa valeur est a TRUE.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PTFPassif-class.R
##'
setGeneric(name = "calcul_pm", def = function(ptf_passif, agreg_out = TRUE) {standardGeneric("calcul_pm")})
setMethod(
    f = "calcul_pm",
    signature = c(ptf_passif = "PTFPassif"),
    definition = function(ptf_passif, agreg_out){



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
        pm_epargne <- sum_cond(x = ptf_passif@epargne@ptf$pm, cond = agreg_out)





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
