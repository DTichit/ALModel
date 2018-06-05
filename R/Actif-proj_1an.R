##' Fonction \code{proj_1an_actif}.
##'
##' Cette fonction permet de projeter horizon 1 an l'actif d'une compagnie d'assurance.
##'
##' @name proj_1an_actif
##' @docType methods
##' @param actif est un objet de type \code{\link{Actif}}.
##' @param an est un \code{integer} reprensentant l'annee sur laquelle on travaille.
##' @author Damien Tichit pour Sia Partners
##' @include Actif-class.R
##'
setGeneric(name = "proj_1an_actif", def = function(actif, an) {standardGeneric("proj_1an_actif")})
setMethod(
    f = "proj_1an_actif",
    signature = c(actif = "Actif", an = "integer"),
    definition = function(actif, an){




        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion du PTF Actif
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        res_gestion <- gestion_ptf_actif(ptf_actif = actif@ptf_actif, hyp_actif = actif@hyp_actif, an = an)

        # Mise a jour de l'attribut
        actif@ptf_actif <- res_gestion[["ptf_actif"]]




        ## ######################################################
        ## ######################################################
        ##
        ##          Revalorisation des PTFs cibles
        ##
        ## ######################################################
        ## ######################################################

        # Revalorisation du portfeuille
        res_revalo_cible <- revalo_ptf_cible(ptf_cible = actif@hyp_actif@ptf_cible, esg = actif@hyp_actif@esg_simu, an = an)

        # Mise a jour de l'objet
        actif@hyp_actif@ptf_cible <- res_revalo_cible[["ptf_cible"]]





        ## ######################################################
        ## ######################################################
        ##
        ##        Evalutation des frais financiers
        ##
        ## ######################################################
        ## ######################################################

        # Calcul des frais financiers
        frais_fin <- eval_frais_fin(ptf_actif = actif@ptf_actif, frais_fin = actif@hyp_actif@frais_fin,
                                    prod_fin = res_gestion[["flux"]][["prod_fin"]])





        ## ######################################################
        ## ######################################################
        ##
        ##              Resultat et Tresorerie
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de differents montants
        frais       <- sum_list(frais_fin[["frais"]], 2L)
        vente       <- sum_list(res_gestion[["flux"]][["vente"]], 1L)
        prod_fin    <- sum_list(res_gestion[["flux"]][["prod_fin"]], 1L)

        # Mouvement sur la tresorerie
        mvt_treso <- prod_fin + vente - frais





        # Output
        return(list(actif = actif,
                    flux = list(prod_fin = res_gestion[["flux"]][["prod_fin"]],
                                vente = res_gestion[["flux"]][["vente"]],
                                frais = frais_fin[["frais"]],
                                var_vnc = res_gestion[["flux"]][["var_vnc"]]),
                    pmvl = res_gestion[["pmvl"]],
                    mvt_solde_treso = mvt_treso))
    }
)
