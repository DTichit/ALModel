##' Fonction \code{rebalancement_action}
##'
##' Cette fonction permet de rebalancer le portfeuille d'action vers un portfeuile cible.
##' Le montant total cible, en valeur de marche, du portefeuille cible est renseigne dans le parametre \code{alloc_cible}.
##'
##' @name rebalancement_action
##' @docType methods
##' @param action est un objet de type \code{\link{Action}}. Ce parametre represente le ptf actuel de la compagnie.
##' @param alloc_cible est un \code{numeric}. Ce parametre indique l'allocation cible a atteindre (en valeur de marche).
##' @author Damien Tichit pour Sia Partners
##' @include Action-class.R
##'
setGeneric(name = "rebalancement_action", def = function(action, alloc_cible) {standardGeneric("rebalancement_action")})
setMethod(
    f = "rebalancement_action",
    signature = c(action = "Action", alloc_cible = "numeric"),
    definition = function(action, alloc_cible) {


        ## ###########################
        ##   Extraction des donnees
        ## ###########################

        # Extraction du PTF
        ptf <- action@ptf

        # Extraction des donnees du PTF
        names_ptf <- names(ptf)
        vc_ptf <- .subset2(ptf, which(names_ptf == "valeur_comptable"))
        vm_ptf <- .subset2(ptf, which(names_ptf == "valeur_marche"))

        # Ratio entre les VC et les VM
        ratio_vcvm <- vc_ptf/vm_ptf

        # Valeur de marche totale
        vm_totale <- sum(vm_ptf)

        # Difference entre les allocations actuelle et cible
        diff_alloc <- alloc_cible - vm_totale




        ## ###########################
        ## Calcul des nouvelles donnees
        ## ###########################

        # Montant ajoute/supprime de la VM
        vm_mov <- diff_alloc * (vm_ptf / vm_totale)

        # Montant ajoute/supprime de la VC
        if(diff_alloc < 0)
            vc_mov <- (vm_ptf + vm_mov) * ratio_vcvm - vc_ptf
        else
            vc_mov <- diff_alloc * (vm_ptf / vm_totale)




        ## ###########################
        ##          MaJ du PTF
        ## ###########################

        # Mise a jour de la VM
        action@ptf$valeur_marche <- vm_ptf + vm_mov

        # Mise a jour de la VC
        action@ptf$valeur_comptable <- vc_ptf + vc_mov





        ## ###########################
        ##    Parametres de sortie
        ## ###########################

        # Plus ou moins values realisees
        if (diff_alloc < 0)
            pmvr <- sum(abs(vm_mov) - abs(vc_mov))
        else
            pmvr <- 0

        # Flux
        flux <-  alloc_cible - vm_totale






        # Output
        return(list(action = action,
                    pmvr = pmvr,
                    flux = flux))
    }
)
