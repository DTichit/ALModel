##' Fonction \code{rebalancement_actif}
##'
##' Cette fonction permet de rebalancer le portefeuille d'actif vers le portfeuile cible.
##'
##' @name rebalancement_actif
##' @docType methods
##' @param actif est un objet de type \code{Actif}.
##' @param solde_tresorerie est un \code{numeric} represantant le montant du solde de tresorerie. Par defaut, cette valeur est egale a 0.
##' @author Damien Tichit pour Sia Partners
##' @include Actif-class.R
##'
setGeneric(name = "rebalancement_actif", def = function(actif, solde_tresorerie = 0) {standardGeneric("rebalancement_actif")})
setMethod(
    f = "rebalancement_actif",
    signature = c(actif = "Actif"),
    definition = function(actif, solde_tresorerie){


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
        alloc_totale <- alloc_ptf[["vm_actif"]][["total"]] + solde_tresorerie



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
        if(alloc_totale > 0)
            actif@ptf_actif@tresorerie@solde <- alloc_cible[which(alloc_cible$produit=="tresorerie"), "proportion"] * alloc_totale
        else
            actif@ptf_actif@tresorerie@solde <- alloc_totale





        ## ###########################
        ##           Actions
        ## ###########################

        # Proportion action
        prop_action <- alloc_cible[which(alloc_cible$produit=="action"), "proportion"]
        alloc_cible_action <- prop_action * alloc_totale

        # Appel de la fonction
        action_rebal <- rebalancement_action(action = actif@ptf_actif@action, alloc_cible = max(0, alloc_cible_action))

        # Mise a jour de l'objet
        actif@ptf_actif@action <- action_rebal[["action"]]




        ## ###########################
        ##         Immobilier
        ## ###########################

        # Proportion immo
        prop_immo <- alloc_cible[which(alloc_cible$produit=="immobilier"), "proportion"]
        alloc_cible_immo <- prop_immo * alloc_totale

        # Appel de la fonction
        immo_rebal <- rebalancement_immobilier(immo = actif@ptf_actif@immobilier, alloc_cible = max(0, alloc_cible_immo))

        # Mise a jour de l'objet
        actif@ptf_actif@immobilier <- immo_rebal[["immo"]]




        ## ###########################
        ##         Obligations
        ## ###########################

        # Proportion oblig
        prop_oblig <- alloc_cible[which(alloc_cible$produit=="obligation"), "proportion"]
        alloc_cible_oblig <- prop_oblig * alloc_totale

        # Appel de la fonction
        oblig_rebal <- rebalancement_obligation(oblig = actif@ptf_actif@obligation, oblig_cible = ptf_cible@obligation, alloc_cible = max(0, alloc_cible_oblig))

        # Mise a jour de l'objet
        actif@ptf_actif@obligation <- oblig_rebal[["oblig"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Difference d'allocation
        flux <- list(action = action_rebal[["flux"]], obligation = oblig_rebal[["flux"]], immobilier = immo_rebal[["flux"]])

        # PMVR
        pmvr <- list(action = action_rebal[["pmvr"]], obligation = oblig_rebal[["pmvr"]], immobilier = immo_rebal[["pmvr"]])




        # Output
        return(list(actif = actif,
                    flux  = flux,
                    pmvr = pmvr))
    }
)
