##' Fonction \code{revalo_ptf_actif}
##'
##' Cette fonction permet de projeter de revaloriser le portfeuille financier. Elle calcule egalement les plus ou moins value latentes (PMVL) engendrees.
##'
##' @name revalo_ptf_actif
##' @docType methods
##' @param ptf_actif est un objet de type \code{\link{PTFActif}}.
##' @author Damien Tichit pour Sia Partners
##' @include Actif-class.R
##'
setGeneric(name = "revalo_ptf_actif", def = function(ptf_actif) {standardGeneric("revalo_ptf_actif")})
setMethod(
    f = "revalo_ptf_actif",
    signature = c(ptf_actif = "PTFActif"),
    definition = function(ptf_actif){


        ## ######################################################
        ## ######################################################
        ##
        ##              Revalorisation des actifs
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##   Gestion des obligations
        ## ###########################

        # Revalorisation des obligations
        res_revalo_oblig <- revalo_obligation(ptf_actif@obligation)

        # Mise a jour de l'attribut
        ptf_actif@obligation <- proj_oblig[["obligation"]]



        ## ###########################
        ##    Gestion des actions
        ## ###########################

        # Revalorisation des actions
        res_revalo_action <- revalo_action(ptf_actif@action)

        # Mise a jour de l'attribut
        ptf_actif@action <- res_revalo_action[["action"]]



        ## ###########################
        ##    Gestion de l'immo
        ## ###########################

        # Revalorisation de l'immo
        res_revalo_immo <- revalo_immobilier(ptf_actif@immobilier)

        # Mise a jour de l'attribut
        ptf_actif@immobilier <- res_revalo_immo[["immobilier"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Agregation des PMVL
        ##
        ## ######################################################
        ## ######################################################

        # PMVL par produit
        pmvl <- list(action = res_revalo_action[["pmvl"]],
                     obligation = res_revalo_oblig[["pmvl"]],
                     immobilier = res_revalo_immo[["pmvl"]])



        # Output
        return(list(ptf_actif = ptf_actif,
                    pmvl = pmvl))
    }
)
