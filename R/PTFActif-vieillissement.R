##' Fonction \code{vieillissement_ptf_actif}
##'
##' Cette fonction permet de vieillir les differents composants d'un portfeuille d'actif.
##'
##' @name vieillissement_ptf_actif
##' @docType methods
##' @param ptf_actif est un objet de type \code{\link{PTFActif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PTFActif-class.R
##'
setGeneric(name = "vieillissement_ptf_actif", def = function(ptf_actif) {standardGeneric("vieillissement_ptf_actif")})
setMethod(
    f = "vieillissement_ptf_actif",
    signature = c(ptf_actif = "PTFActif"),
    definition = function(ptf_actif){


        ## ######################################################
        ## ######################################################
        ##
        ##           Gestion des differents actifs
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Obligations
        ## ###########################

        # Appel de la fonction
        res_obligation <- vieillissement_obligation(obligation = ptf_actif@obligation)

        # Mise a jour de l'attribut
        ptf_actif@obligation <- res_obligation[["obligation"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Vente
        vente <- list(obligation = res_obligation[["flux"]][["vente"]])

        # Plus ou moins values
        pmv <- list(obligation = res_obligation[["flux"]][["pmv"]])




        # Output
        return(list(ptf_actif = ptf_actif,
                    flux = list(vente = vente,
                                pmv = pmv)))
    }
)
