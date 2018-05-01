##' Fonction \code{realisation_pvl_obligation}
##'
##' Cette fonction permet de realiser des plus values lattentes pour un portefeuille obligataire.
##'
##' @name realisation_pvl_obligation
##' @docType methods
##' @param obligation est un objet de type \code{Obligation}.
##' @param montant est un \code{numeric} indiquant le montant de plus value devant etre realise.
##' @author Damien Tichit pour Sia Partners
##' @include Obligation-class.R
##'
setGeneric(name = "realisation_pvl_obligation", def = function(obligation, montant) {standardGeneric("realisation_pvl_obligation")})
setMethod(
    f = "realisation_pvl_obligation",
    signature = c(obligation = "Obligation", montant = "numeric"),
    definition = function(obligation, montant){



        ## ###########################
        ##    Extraction de donnees
        ## ###########################
        names_ptf   <- names(obligation@ptf)
        vc_ptf      <- .subset2(obligation@ptf, which(names_ptf == "valeur_nette_comptable"))
        vm_ptf      <- .subset2(obligation@ptf, which(names_ptf == "valeur_marche"))



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

            # Vente
            vente <- sum(vm_ptf[id_pvl] * proportion_vente)

            # Nouvelles donnees
            new_vc <- vc_ptf[id_pvl] * (1 - proportion_vente)
            new_vm <- vm_ptf[id_pvl] * (1 - proportion_vente)

            # Mise a jour de l'attribut
            obligation@ptf$valeur_nette_comptable[id_pvl]   <- new_vc
            obligation@ptf$valeur_marche[id_pvl]            <- new_vm
            obligation@ptf$nominal[id_pvl]                  <- .subset2(obligation@ptf, which(names_ptf == "nominal")) * (1 - proportion_vente)
            obligation@ptf$valeur_remboursement[id_pvl]     <- .subset2(obligation@ptf, which(names_ptf == "valeur_remboursement")) * (1 - proportion_vente)
        }




        # Output
        return(list(obligation = obligation,
                    pvr = pvr,
                    vente = if.is_null(get0("vente"), 0)))
    }
)
