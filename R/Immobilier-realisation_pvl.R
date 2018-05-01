##' Fonction \code{realisation_pvl_immobilier}
##'
##' Cette fonction permet de realiser des plus values lattentes.
##'
##' @name realisation_pvl_immobilier
##' @docType methods
##' @param immobilier est un objet de type \code{Immobilier}.
##' @param montant est un \code{numeric} indiquant le montant de plus value devant etre realise.
##' @author Damien Tichit pour Sia Partners
##' @include Immobilier-class.R
##'
setGeneric(name = "realisation_pvl_immobilier", def = function(immobilier, montant) {standardGeneric("realisation_pvl_immobilier")})
setMethod(
    f = "realisation_pvl_immobilier",
    signature = c(immobilier = "Immobilier", montant = "numeric"),
    definition = function(immobilier, montant){



        ## ###########################
        ##    Extraction de donnees
        ## ###########################
        names_ptf   <- names(immobilier@ptf)
        vc_ptf      <- .subset2(immobilier@ptf, which(names_ptf == "valeur_comptable"))
        vm_ptf      <- .subset2(immobilier@ptf, which(names_ptf == "valeur_marche"))



        ## ###########################
        ##      Montant repris
        ## ###########################

        # Calcul des PVL
        pmvl <- vm_ptf - vc_ptf

        # Lignes pour lesquelles on est en PVL
        id_pvl <- which(pmvl > 0)

        # PV realisees
        pvr <- min(montant, if.is_empty(sum(pmvl[id_pvl]), 0))



        ## ###########################
        ##     Realisation des PVL
        ## ###########################

        if(pvr > 0) {

            # Proportion de vente
            proportion_vente <- min(montant / pmvl[id_pvl], 1)

            # Nouvelles donnees
            new_vc <- vc_ptf[id_pvl] * (1 - proportion_vente)
            new_vm <- vm_ptf[id_pvl] * (1 - proportion_vente)

            # Mise a jour de l'attribut
            immobilier@ptf$valeur_comptable[id_pvl] <- new_vc
            immobilier@ptf$valeur_marche[id_pvl]    <- new_vm

            # Vente
            vente <- sum(vm_ptf[id_pvl] * proportion_vente)
        }




        # Output
        return(list(immobilier = immobilier,
                    pvr = pvr,
                    vente = if.is_null(get0("vente"), 0)))
    }
)
