##' Fonction \code{revalo_obligation}
##'
##' Cette fonction permet de revaloriser les differentes obligations d'un portefeuille obligataire.
##' Elle calcule egalement les plus ou moins value latentes (PMVL) engendrees.
##'
##' @name revalo_obligation
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @param  yield_curve est un \code{numeric} contenant les prix zero-coupon.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "revalo_obligation", def = function(obligation, yield_curve) {standardGeneric("revalo_obligation")})
setMethod(
    f = "revalo_obligation",
    signature = c(obligation = "Obligation", yield_curve = "numeric"),
    definition = function(obligation, yield_curve){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        names_ptf <- names(obligation@ptf)
        vnc_ptf <- .subset2(obligation@ptf, which(names_ptf == "valeur_nette_comptable"))
        vm_ptf      <- .subset2(obligation@ptf, which(names_ptf == "valeur_marche"))
        nominal_ptf  <- .subset2(obligation@ptf, which(names_ptf == "nominal"))
        coupon_ptf  <- .subset2(obligation@ptf, which(names_ptf == "coupon"))
        vr_ptf      <- .subset2(obligation@ptf, which(names_ptf == "valeur_remboursement"))
        mat_ptf <- .subset2(obligation@ptf, which(names_ptf == "maturite"))
        dur_det_ptf <- .subset2(obligation@ptf, which(names_ptf == "duree_detention"))
        spread_ptf <- .subset2(obligation@ptf, which(names_ptf == "spread"))
        tri_ptf <- .subset2(obligation@ptf, which(names_ptf == "tri"))

        # Calcul de la maturite residuelle du PTF
        mat_res_ptf <- mat_ptf - dur_det_ptf




        ## ###########################
        ##  Calcul des nouvelles VNC
        ## ###########################

        # Calcul de la nouvelle VNC
        new_vnc <- vnc_ptf - nominal_ptf * coupon_ptf * exp(-tri_ptf * (mat_res_ptf + 1L)) - vr_ptf * exp(-tri_ptf * (mat_res_ptf + 1L)) + vr_ptf * exp(-tri_ptf * mat_res_ptf)

        # Mise a jour du PTF
        obligation@ptf$valeur_nette_comptable <- new_vnc

        # Variation de la VNC
        var_vnc <- new_vnc - vnc_ptf



        ## ###########################
        ##   Calcul des nouvelles VM
        ## ###########################

        # Calcul de la VM pour les differentes maturites residuelles
        new_vm <- sapply(1L:nrow(obligation@ptf), function(id){

            # Extraction de donnees
            mat_res <- mat_res_ptf[id]
            coupon <- coupon_ptf[id] * nominal_ptf[id]
            actu <- exp(-(yield_curve[1L:mat_res] + spread_ptf[id]) * 1L:mat_res)
            vr <- vr_ptf[id]

            # Actualisation des coupons (Sommation pour eviter de passer par une matrice)
            coupon_act <- coupon * actu

            # Calcul de la nouvelle VM
            vm <- sum(coupon_act) + vr * actu[mat_res]

            # Output
            return(vm)
        })

        # Mise a jour de l'attribut
        obligation@ptf$valeur_marche <- new_vm




        ## ###########################
        ##  Calcul du nouveau nominal
        ## ###########################

        # Calcul du nouveau nominal
        new_nominal <- nominal_ptf * exp(-spread_ptf)

        # Mise a jour du PTF
        obligation@ptf$nominal <- new_nominal




        ## ###########################
        ##  Calcul de la nouvelle VR
        ## ###########################

        # Calcul de la nouvelle VR
        new_vr <- vr_ptf * exp(-spread_ptf)

        # Mise a jour du PTF
        obligation@ptf$valeur_remboursement <- new_vr



        ## ###########################
        ##      Calcul des PMVL
        ## ###########################

        # Determination des PMVL
        pmvl <- new_vm - new_vnc



        # Output
        return(list(obligation = obligation,
                    var_vnc = sum(var_vnc),
                    pmvl = sum(pmvl)))
    }
)
