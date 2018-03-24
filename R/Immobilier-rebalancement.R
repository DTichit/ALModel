##' Fonction \code{rebalancement_immobilier}
##'
##' Cette fonction permet de rebalancer le portfeuille immobilier vers un portfeuile cible.
##' Le montant total, en valeur de marche, du portefeuille cible est renseigne dans le parametre \code{alloc_cible}.
##'
##' @name rebalancement_immobilier
##' @docType methods
##' @param immo est un objet de type \code{\link{Immobilier}}. Ce parametre represente le ptf actuel de la compagnie.
##' @param immo_cible est un objet de type \code{\link{Immobilier}}. Ce parametre represente le ptf cible.
##' @param alloc_cible est un \code{numeric}. Ce parametre indique l'allocation cible a atteindre.
##' @author Damien Tichit pour Sia Partners
##' @include Immobilier-class.R
##'
setGeneric(name = "rebalancement_immobilier", def = function(immo, alloc_cible) {standardGeneric("rebalancement_immobilier")})
setMethod(
    f = "rebalancement_immobilier",
    signature = c(immo = "Immobilier", alloc_cible = "numeric"),
    definition = function(immo, alloc_cible) {


        ## ###########################
        ##   Extraction des donnees
        ## ###########################

        # Extraction du PTF
        ptf <- immo@ptf

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
        immo@ptf$valeur_marche <- new_vm

        # Mise a jour de la VC
        immo@ptf$valeur_comptable <- new_vm * ratio_vcvm




        ## ###########################
        ##    Parametres de sortie
        ## ###########################
        if (diff_alloc < 0) {
            achat <- diff_alloc ; vente <- 0
        } else {
            vente <- diff_alloc ; achat <- 0
        }



        # Output
        return(list(immo = immo,
                    flux = list(achat = achat,
                                vente = vente)))
    }
)
