##' Fonction \code{proj_1an_actif}.
##'
##' Cette fonction permet de projeter horizon 1 an un portefeuille financier.
##'
##' @name proj_1an_actif
##' @docType methods
##' @param actif est un objet de type \code{Actif}.
##' @param an est un \code{integer}.
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
        ##              Revalorisation des actifs
        ##
        ## ######################################################
        ## ######################################################

        # Revalorisation du portfeuille
        res_revalo_actif <- revalo_ptf_actif(ptf_actif = actif@ptf_actif)

        # Mise a jour de l'objet
        actif@ptf_actif <- res_revalo_actif[["ptf_actif"]]

        # Extraction des PMVL
        pmvl <- res_revalo_actif[["pmvl"]]




        ## ######################################################
        ## ######################################################
        ##
        ##              Revalorisation des actifs
        ##
        ## ######################################################
        ## ######################################################

        # Revalorisation du portfeuille
        res_revalo_actif <- revalo_actif(actif)

        # Mise a jour de l'objet
        actif <- res_revalo_actif[["actif"]]

        # Extraction des PMVL
        pmvl <- res_revalo_actif[["pmvl"]]





        ## ######################################################
        ## ######################################################
        ##
        ##         Projection sur 1 an des differents PTF
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##   Gestion des obligations
        ## ###########################

        # Projection sur une annee des obligations
        proj_oblig <- proj_1an_obligation(actif@ptf_actif@obligation)

        # Mise a jour de l'attribut
        actif@ptf_actif@obligation <- proj_oblig[["obligation"]]


        ## ###########################
        ##    Gestion des actions
        ## ###########################

        # Projection sur une annee des actions
        proj_action <- proj_1an_action(actif@ptf_actif@action)

        # Mise a jour de l'attribut
        actif@ptf_actif@action <- proj_action[["action"]]


        ## ###########################
        ##    Gestion de l'immo
        ## ###########################

        # Projection sur une annee de l'immo
        proj_immo <- proj_1an_immobilier(actif@ptf_actif@immobilier)

        # Mise a jour de l'attribut
        actif@ptf_actif@immobilier <- proj_immo[["immobilier"]]




        # Output
        return(list(actif = actif))
    }
)
