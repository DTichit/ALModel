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
        ratio_vcvm <- if.is_na(vc_ptf/vm_ptf, 1)

        # Valeur de marche totale
        vm_totale <- sum(vm_ptf)

        # Difference entre les allocations actuelle et cible
        diff_alloc <- alloc_cible - vm_totale




        ## ###########################
        ## Calcul des nouvelles donnees
        ## ###########################

        # Montant ajoute/supprime de la VM
        vm_mov <- diff_alloc

        # Montant ajoute/supprime de la VC
        if(diff_alloc < 0)
            vc_mov <- (vm_ptf + vm_mov) * ratio_vcvm - vc_ptf
        else
            vc_mov <- diff_alloc




        ## ###########################
        ##          MaJ du PTF
        ## ###########################

        # Mise a jour de la VM
        immo@ptf$valeur_marche <- vm_ptf + vm_mov

        # Mise a jour de la VC
        immo@ptf$valeur_comptable <- vc_ptf + vc_mov




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
        return(list(immo = immo,
                    pmvr = pmvr,
                    flux = flux))
    }
)
