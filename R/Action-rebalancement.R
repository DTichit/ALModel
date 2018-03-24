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
        ##          MaJ du PTF
        ## ###########################

        # Calcul des nouvelles VM
        new_vm <- vm_ptf + diff_alloc * (vm_ptf / vm_totale)

        # Mise a jour de la VM
        action@ptf$valeur_marche <- new_vm

        # Mise a jour de la VC
        action@ptf$valeur_comptable <- new_vm * ratio_vcvm




        ## ###########################
        ##    Parametres de sortie
        ## ###########################
        if (diff_alloc < 0) {
            achat <- diff_alloc ; vente <- 0
        } else {
            vente <- diff_alloc ; achat <- 0
        }



        # Output
        return(list(action = action,
                    flux = list(achat = achat,
                                vente = vente)))
    }
)
