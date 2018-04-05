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
        ##      Revalorisation des actifs et calcul des PMVL
        ##
        ## ######################################################
        ## ######################################################

        # Revalorisation du portfeuille
        res_revalo_actif <- revalo_ptf_actif(ptf_actif = actif@ptf_actif, hyp_actif = actif@hyp_actif, an = an)

        # Mise a jour de l'objet
        actif@ptf_actif <- res_revalo_actif[["ptf_actif"]]




        ## ######################################################
        ## ######################################################
        ##
        ##          Evaluation des produits financiers
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        prod_fin_ptf <- eval_prod_fin(ptf_actif = actif@ptf_actif, hyp_actif = actif@hyp_actif, an = an)





        ## ######################################################
        ## ######################################################
        ##
        ##        Evalutation des frais financiers
        ##
        ## ######################################################
        ## ######################################################

        # Calcul des frais financiers
        frais_fin <- eval_frais_fin(ptf_actif = actif@ptf_actif, frais_fin = actif@hyp_actif@frais_fin,
                                    prod_fin = prod_fin_ptf[["prod_fin"]])





        ## ######################################################
        ## ######################################################
        ##
        ##          Vieillissement des portefeuilles
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        res_vieillissement <- vieillissement_ptf_actif(ptf_actif = actif@ptf_actif)

        # Mise a jour de l'objet
        actif@ptf_actif <- res_vieillissement[["ptf_actif"]]


        warning("On fait quoi des ventes apres le vieillissement ? Mise a jour de la treso ou PB ?")





        ## ######################################################
        ## ######################################################
        ##
        ##      Mise a jour de la tresorerie apres ces etapes
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des montants a inserer dans la treso
        frais <- sapply(names(frais_fin[["frais"]]), function(x) do.call(sum, frais_fin[["frais"]][[x]]))

        # Calcul du montant a inserer dans la tresorerie
        montant <- frais
        warning("Partie a finaliser")
        warning("Est ce qu'on met les ventes des actifs ?")

        # Mise a jour de l'attribut
        # actif@ptf_actif@tresorerie <- actif@ptf_actif@tresorerie + montant



        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################





        # Output
        return(list(actif = actif,
                    flux = list(prod_fin = prod_fin_ptf[["prod_fin"]],
                                vente = res_vieillissement[["flux"]][["vente"]],
                                pmvr  = res_vieillissement[["flux"]][["pmv"]],
                                frais = frais_fin[["frais"]]),
                    pmvl = res_revalo_actif[["pmvl"]]))
    }
)
