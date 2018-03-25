##' Fonction \code{proj_1an_actif}.
##'
##' Cette fonction permet de projeter horizon 1 an l'actif d'une compagnie d'assurance.
##'
##' @name proj_1an_actif
##' @docType methods
##' @param actif est un objet de type \code{\link{Actif}}.
##' @author Damien Tichit pour Sia Partners
##' @include Actif-class.R
##'
setGeneric(name = "proj_1an_actif", def = function(actif) {standardGeneric("proj_1an_actif")})
setMethod(
    f = "proj_1an_actif",
    signature = c(actif = "Actif"),
    definition = function(actif){


        warning("La tresorerie n'est jamais mise a jour a la suite des differents flux : frais, vieilissement....")

        ## ######################################################
        ## ######################################################
        ##
        ##      Revalorisation des actifs et calcul des PMVL
        ##
        ## ######################################################
        ## ######################################################

        # Revalorisation du portfeuille
        res_revalo_actif <- revalo_ptf_actif(ptf_actif = actif@ptf_actif)

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
        prod_fin_ptf <- eval_prod_fin(ptf_actif = actif@ptf_actif)





        ## ######################################################
        ## ######################################################
        ##
        ##        Evalutation des frais financiers
        ##
        ## ######################################################
        ## ######################################################

        # Calcul des frais financiers
        frais_fin <- eval_frais_fin(ptf_actif = actif@ptf_actif, hyp_actif = actif@hyp_actif)





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

        warning("Les PMV sortant de la fonction 'vieillissement' sont deja comptablisees dans la fonction 'revalo' !")

        # Plusieurs solutions pour palier a ce probleme :
        #       - Mettre le vieillissement en debut de fonction ;
        #       - Sortir de la fonction 'vieillissement' les indices des produits vendus ;
        #       - Dans la fonction 'revalo', ne calculer que les PMVL pour les obligations dont la maturite est >=2 ;
        #       - Ajouter une colonne 'last_vm' dans les PTF afin de calculer les PMVL plus tard.


        warning("On fait quoi des ventes apres le vieillissement ? Mise a jour de la treso ou PB ?")



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
                                pmv   = res_vieillissement[["flux"]][["pmv"]],
                                frais = frais_fin[["frais"]]),
                    pmvl = res_revalo_actif[["pmvl"]]))
    }
)
