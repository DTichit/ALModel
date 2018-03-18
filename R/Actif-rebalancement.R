##' Fonction \code{rebalancement_actif}
##'
##' Cette fonction permet de rebalancer le portfeuille d'actif vers le portfeuile cible.
##'
##' @name rebalancement_actif
##' @docType methods
##' @param actif est un objet de type \code{Actif}.
##' @author Damien Tichit pour Sia Partners
##' @include Actif-class.R
##'
setGeneric(name = "rebalancement_actif", def = function(actif) {standardGeneric("rebalancement_actif")})
setMethod(
    f = "rebalancement_actif",
    signature = c(actif = "Actif"),
    definition = function(actif){

        ## ###########################
        ##   Extraction du PTF cible
        ## ###########################

        # Extraction
        ptf_cible <- actif@hyp_actif@ptf_cible



        ## ######################################################
        ## ######################################################
        ##
        ##                      Allocations
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##     Allocation actuelle
        ## ###########################

        # Allocation
        alloc_ptf <- allocation_ptf_actif(actif@ptf_actif)

        # Allocation totale
        alloc_totale <- alloc_ptf[["vm_actif"]][["total"]]


        ## ###########################
        ##     Allocation cible
        ## ###########################

        # Extraction de donnees
        alloc_cible <- ptf_cible@alloc_cible





        ## ######################################################
        ## ######################################################
        ##
        ##              Rebalancement par actif
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##         Tresorerie
        ## ###########################

        # Mise a jour du solde
        actif@ptf_actif@tresorerie@ptf$solde <- alloc_cible[which(alloc_cible$produit=="tresorerie"), "proportion"] * alloc_totale




        ## ###########################
        ##           Actions
        ## ###########################

        # Proportion action
        prop_action <- alloc_cible[which(alloc_cible$produit=="action"), "proportion"]
        alloc_cible_action <- prop_action * alloc_totale

        # Appel de la fonction
        action_rebal <- rebalancement_action(action = actif@ptf_actif@action, action_cible = ptf_cible@action, alloc_cible = alloc_cible_action)

        # Flux actions
        flux_action <- action_rebal[["flux"]]

        # Mise a jour de l'objet
        actif@ptf_actif@action <- action_rebal[["action"]]




        ## ###########################
        ##         Immobilier
        ## ###########################

        # Proportion immo
        prop_immo <- alloc_cible[which(alloc_cible$produit=="immobilier"), "proportion"]
        alloc_cible_immo <- prop_immo * alloc_totale

        # Appel de la fonction
        immo_rebal <- rebalancement_immobilier(immo = actif@ptf_actif@immobilier, immo_cible = ptf_cible@immobilier, alloc_cible = alloc_cible_immo)

        # Flux immo
        flux_immo <- immo_rebal[["flux"]]

        # Mise a jour de l'objet
        actif@ptf_actif@immobilier <- immo_rebal[["immo"]]




        ## ###########################
        ##         Obligations
        ## ###########################

        # Proportion oblig
        prop_oblig <- alloc_cible[which(alloc_cible$produit=="obligation"), "proportion"]
        alloc_cible_oblig <- prop_oblig * alloc_totale

        # Appel de la fonction
        oblig_rebal <- rebalancement_obligation(oblig = actif@ptf_actif@obligation, oblig_cible = ptf_cible@obligation, alloc_cible = alloc_cible_oblig)

        # Flux obligataires
        flux_oblig <- oblig_rebal[["flux"]]

        # Mise a jour de l'objet
        actif@ptf_actif@obligation <- oblig_rebal[["oblig"]]



        # Output
        return(list(actif = actif,
                    flux = list(achat = list(action = flux_action[["achat"]], obligation = flux_oblig[["achat"]], immobilier = flux_immo[["achat"]]),
                                vente = list(action = flux_action[["vente"]], obligation = flux_oblig[["vente"]], immobilier = flux_immo[["vente"]]))))
    }
)