##' Fonction \code{gestion_ptf_actif}.
##'
##' Cette fonction permet de gerer les differents ptf actif d'une compagnie d'assurance : recolte des produits financiers, recalcul des VM, vieillissement...
##'
##' @name gestion_ptf_actif
##' @docType methods
##' @param ptf_actif est un objet de type \code{\link{PTFActif}}.
##' @param hyp_actif est un objet de type \code{\link{HypActif}}.
##' @param an est un \code{integer} reprensentant l'annee sur laquelle on travaille.
##' @author Damien Tichit pour Sia Partners
##' @include PTFActif-class.R HypActif-class.R
##'
setGeneric(name = "gestion_ptf_actif", def = function(ptf_actif, hyp_actif, an) {standardGeneric("gestion_ptf_actif")})
setMethod(
    f = "gestion_ptf_actif",
    signature = c(ptf_actif = "PTFActif", hyp_actif = "HypActif", an = "integer"),
    definition = function(ptf_actif, hyp_actif, an){




        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion des differents PTF
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Monetaire
        ## ###########################

        # Appel de la fonction
        res_treso <- gestion_tresorerie(tresorerie = ptf_actif@tresorerie, hyp_actif = hyp_actif, an = an)

        # Mise a jour de l'attribut
        ptf_actif@tresorerie <- res_treso[["tresorerie"]]



        ## ###########################
        ##          Actions
        ## ###########################

        # Appel de la fonction
        res_action <- gestion_action(action = ptf_actif@action, esg_simu = hyp_actif@esg_simu, an = an)

        # Mise a jour de l'attribut
        ptf_actif@action <- res_action[["action"]]



        ## ###########################
        ##          Immobilier
        ## ###########################

        # Appel de la fonction
        res_immo <- gestion_immobilier(immobilier = ptf_actif@immobilier, esg_simu = hyp_actif@esg_simu, an = an)

        # Mise a jour de l'attribut
        ptf_actif@immobilier <- res_immo[["immobilier"]]



        ## ###########################
        ##          Obligations
        ## ###########################

        # Appel de la fonction
        res_oblig <- gestion_obligation(obligation = ptf_actif@obligation, ctz_nom = hyp_actif@esg_simu$ctz_nom, an = an)

        # Mise a jour de l'attribut
        ptf_actif@obligation <- res_oblig[["obligation"]]






        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des resultats
        ##
        ## ######################################################
        ## ######################################################

        # Creation d'une liste contenant les resultats
        flux <- list(prod_fin = list(action = res_action[["dividendes"]],
                                     immobilier = res_immo[["loyers"]],
                                     obligation = res_oblig[["coupons"]],
                                     tresorerie = res_treso[["interets"]]),
                     vente = list(obligation = res_oblig[["vente"]]),
                     var_vnc = list(obligation = res_oblig[["var_vnc"]]))

        # Plus ou moins values latentes
        pmvl <- list(action = res_action[["pmvl"]],
                     immobilier = res_immo[["pmvl"]],
                     obligation = res_oblig[["pmvl"]])





        # Output
        return(list(ptf_actif = ptf_actif,
                    flux = flux,
                    pmvl = pmvl))
    }
)
