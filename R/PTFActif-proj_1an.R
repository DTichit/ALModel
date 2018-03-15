##' Fonction \code{proj_1an_ptf_actif}.
##'
##' Cette fonction permet de projeter horizon 1 an un portfeuille financier : gestion des diffrents actifs.
##'
##' @name proj_1an_ptf_actif
##' @docType methods
##' @param ptf_actif est un objet de type \code{\link{PTFActif}}.
##' @author Damien Tichit pour Sia Partners
##' @include PTFActif-class.R
##'
setGeneric(name = "proj_1an_ptf_actif", def = function(ptf_actif) {standardGeneric("proj_1an_ptf_actif")})
setMethod(
    f = "proj_1an_ptf_actif",
    signature = c(ptf_actif = "PTFActif"),
    definition = function(ptf_actif){



        ## ######################################################
        ## ######################################################
        ##
        ##          Gestion des differents actifs
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Actions
        ## ###########################

        # Appel de la fonction
        res_action <- proj_1an_action(ptf_actif@action)

        # MaJ de l'objet (Pas necessaire pour les actions)
        ptf_actif@action <- res_action[["action"]]




        ## ###########################
        ##        Obligations
        ## ###########################

        # Appel de la fonction
        res_oblig <- proj_1an_obligation(ptf_actif@obligation)

        # MaJ de l'objet
        ptf_actif@obligation <- res_oblig[["obligation"]]




        ## ###########################
        ##      Immobilier
        ## ###########################

        # Appel de la fonction
        res_immo <- proj_1an_immobilier(ptf_actif@immobilier)

        # MaJ de l'objet (Pas necessaire pour les actions)
        ptf_actif@immobilier <- res_immo[["immobilier"]]






        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Produits financiers
        prod_fin <- list(action = sum(res_action[["flux"]][["dividende"]]),
                         obligation = sum(res_oblig[["flux"]][["coupon"]]),
                         immobilier = sum(res_immo[["flux"]][["loyer"]]))

        # Vente
        vente <- list(obligation = sum(res_oblig[["flux"]][["vente"]]))




        # Output
        return(list(ptf_actif = ptf_actif,
                    flux = list(prod_fin = prod_fin,
                                vente = vente)))
    }
)
