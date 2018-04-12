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

        warning("Mettre de le spread")

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        ptf_oblig <- obligation@ptf
        names_ptf <- names(ptf_oblig)
        vm_ptf      <- .subset2(ptf_oblig, which(names_ptf == "valeur_marche"))
        coupon_ptf  <- .subset2(ptf_oblig, which(names_ptf == "coupon"))
        nominal_ptf <- .subset2(ptf_oblig, which(names_ptf == "nominal"))
        vr_ptf      <- .subset2(ptf_oblig, which(names_ptf == "valeur_remboursement"))
        maturite_ptf <- .subset2(ptf_oblig, which(names_ptf == "maturite"))
        dur_det_ptf <- .subset2(ptf_oblig, which(names_ptf == "duree_detention"))

        # Calcul de la maturite residuelle du PTF
        mat_res_ptf <- maturite_ptf - dur_det_ptf

        # Ne conserver que les lignes ayant des obligs
        id <- which(nominal_ptf > 0)

        # Maturite residuelles uniques
        uniq_mat_res <- unique(mat_res_ptf[id])



        ## ###########################
        ##   Calcul des nouvelles VM
        ## ###########################

        # Initialisation du vecteur contenant les nouvelles vm
        new_vm <- rep(x = 0, length = nrow(ptf_oblig))

        # Calcul de la VM pour les differentes maturites residuelles
        for(mat_res in uniq_mat_res) {

            # Lignes correspondantes a la mat_res en question
            id_mat_res <- which(mat_res_ptf == mat_res)

            # Extraction de donnees
            coupon <- coupon_ptf[id_mat_res] * nominal_ptf[id_mat_res]
            pzc <- exp(-yield_curve[seq(1L, mat_res)])
            vr <- nominal_ptf[id_mat_res]

            # Actualisation des coupons (Sommation pour eviter de passer par une matrice)
            coupon_act <- sum(coupon) * (pzc^seq(1L, mat_res))

            # Calcul des nouvelles VM
            new_vm[id_mat_res] <- (coupon/sum(coupon)) * sum(coupon_act) + vr * pzc[mat_res]^mat_res

        }



        ## ###########################
        ##      Calcul des PMVL
        ## ###########################

        # Determination des PMVL
        pmvl <- new_vm - vm_ptf



        ## ###########################
        ##   Mise a jour de l'objet
        ## ###########################

        # Mise a jour des VM
        ptf_oblig$valeur_marche <- new_vm

        # Mise a jour de l'attribut
        obligation@ptf <- ptf_oblig



        # Output
        return(list(obligation = obligation,
                    pmvl = sum(pmvl)))
    }
)
