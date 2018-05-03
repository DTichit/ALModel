##' Fonction \code{gestion_obligation}
##'
##' Cette fonction permet de gerer un portfeuille obligation : recolte des coupons, recalcul des VM, VNC, VR, vieillissement...
##'
##' @name gestion_obligation
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @param hyp_actif est un objet de type \code{\link{HypActif}}.
##' @param an est un \code{integer} reprensentant l'annee sur laquelle on travaille.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R HypActif-class.R
##'
setGeneric(name = "gestion_obligation", def = function(obligation, hyp_actif, an) {standardGeneric("gestion_obligation")})
setMethod(
    f = "gestion_obligation",
    signature = c(obligation = "Obligation", hyp_actif = "HypActif", an = "integer"),
    definition = function(obligation, hyp_actif, an){



        ## ######################################################
        ## ######################################################
        ##
        ##   Extraction de la courbe des taux et calcul spread
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de la courbe des taux
        name_ctz <- names(hyp_actif@esg_simu$ctz_nom)
        num <- which(.subset2(hyp_actif@esg_simu$ctz_nom, which(name_ctz == "ProjYr")) == an)
        yield_curve <- .subset2(hyp_actif@esg_simu$ctz_nom, which(name_ctz == "ZeroCoupon"))[num]

        # Calcul du spread : uniquement en 1ere annee
        if(an == 1L)
            obligation@ptf$spread <- calc_spread(obligation = obligation, yield_curve = yield_curve)






        ## ######################################################
        ## ######################################################
        ##
        ##            Vieillissement des obligations
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnnes
        names_ptf <- names(obligation@ptf)

        # Mise a jour de la duree de detention
        obligation@ptf$duree_detention <- .subset2(obligation@ptf, which(names_ptf == "duree_detention")) + 1L






        ## ######################################################
        ## ######################################################
        ##
        ##                  Ecreter Nominal
        ##
        ## ######################################################
        ## ######################################################

        # Extraction du spread
        spread_ptf <- .subset2(obligation@ptf, which(names_ptf == "spread"))


        ## ###########################
        ##  Calcul du nouveau nominal
        ## ###########################

        # Calcul du nouveau nominal
        new_nominal <- .subset2(obligation@ptf, which(names_ptf == "nominal")) * exp(-spread_ptf)

        # Mise a jour du PTF
        obligation@ptf$nominal <- new_nominal






        ## ######################################################
        ## ######################################################
        ##
        ##                  Recolte des coupons
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        nominal_ptf <- .subset2(obligation@ptf, which(names_ptf == "nominal"))
        coupon_ptf  <- .subset2(obligation@ptf, which(names_ptf == "coupon"))



        ## ###########################
        ##      Calcul de coupons
        ## ###########################
        coupons <- coupon_ptf * nominal_ptf







        ## ######################################################
        ## ######################################################
        ##
        ##          Calcul des nouvelles VM, VNC et VR
        ##
        ## ######################################################
        ## ######################################################


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        vnc_ptf     <- .subset2(obligation@ptf, which(names_ptf == "valeur_nette_comptable"))
        vm_ptf      <- .subset2(obligation@ptf, which(names_ptf == "valeur_marche"))
        nominal_ptf <- .subset2(obligation@ptf, which(names_ptf == "nominal"))
        coupon_ptf  <- .subset2(obligation@ptf, which(names_ptf == "coupon"))
        vr_ptf      <- .subset2(obligation@ptf, which(names_ptf == "valeur_remboursement"))
        mat_ptf     <- .subset2(obligation@ptf, which(names_ptf == "maturite"))
        dur_det_ptf <- .subset2(obligation@ptf, which(names_ptf == "duree_detention"))
        tri_ptf     <- .subset2(obligation@ptf, which(names_ptf == "tri"))

        # Calcul de la maturite residuelle du PTF
        mat_res_ptf <- mat_ptf - dur_det_ptf




        ## ###########################
        ##   Calcul des nouvelles VM
        ## ###########################

        # Calcul de la VM pour les differentes maturites residuelles
        new_vm <- sapply(1L:nrow(obligation@ptf), function(id){

            # Extraction de donnees
            mat_res <- mat_res_ptf[id]

            # Calcul de la VM
            if(mat_res > 0) {

                # Cupons et actualisation
                coupon <- coupon_ptf[id] * nominal_ptf[id]
                actu <- exp(-(yield_curve[1L:mat_res] + spread_ptf[id]) * 1L:mat_res)

                # Actualisation des coupons
                coupon_act <- coupon * actu

                # Calcul de la nouvelle VM
                vm <- sum(coupon_act) + vr_ptf[id] * actu[mat_res]
            } else {

                # VM egale a la VR
                vm <- vr_ptf[id] * exp(-spread_ptf[id])
            }

            # Output
            return(vm)
        })

        # Mise a jour de l'attribut
        obligation@ptf$valeur_marche <- new_vm




        ## ###########################
        ##  Calcul des nouvelles VNC
        ## ###########################

        # Calcul de la nouvelle VNC
        # new_vnc <- vnc_ptf - (nominal_ptf * coupon_ptf) * exp(spread_ptf) * exp(-tri_ptf * (mat_res_ptf + 1L)) - new_vr * exp(spread_ptf) * exp(-tri_ptf * (mat_res_ptf + 1L)) + new_vr * exp(-tri_ptf * mat_res_ptf)
        # new_vnc <- exp(-spread_ptf) * (vnc_ptf - (nominal_ptf * coupon_ptf) * exp(spread_ptf) * exp(-tri_ptf * (mat_res_ptf + 1L)) - new_vr * exp(spread_ptf) * exp(-tri_ptf * (mat_res_ptf + 1L)) + new_vr * exp(spread_ptf) * exp(-tri_ptf * mat_res_ptf))
        new_vnc <- sapply(1L:nrow(obligation@ptf), function(id){

            # Extraction de donnees
            mat_res <- mat_res_ptf[id]

            # Calcul de la VNC
            if(mat_res > 0)
                vnc <- sum(coupon_ptf[id] * nominal_ptf[id] * exp(-tri_ptf[id] * 1L:mat_res)) + vr_ptf[id] * exp(-tri_ptf[id] * mat_res)
            else
                vnc <- vr_ptf[id] * exp(-spread_ptf[id])

            # Output
            return(vnc)
        })

        # Mise a jour du PTF
        obligation@ptf$valeur_nette_comptable <- new_vnc

        # Variation de la VNC
        var_vnc <- new_vnc - vnc_ptf




        ## ###########################
        ##  Calcul de la nouvelle VR
        ## ###########################

        # Calcul de la nouvelle VR
        new_vr <- .subset2(obligation@ptf, which(names_ptf == "valeur_remboursement")) * exp(-spread_ptf)

        # Mise a jour du PTF
        obligation@ptf$valeur_remboursement <- new_vr




        ## ###########################
        ##      Calcul des PMVL
        ## ###########################

        # Determination des PMVL
        pmvl <- new_vm - new_vnc








        ## ######################################################
        ## ######################################################
        ##
        ##       Vente des obligations arrivees a maturite
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        maturite_new <- .subset2(obligation@ptf, which(names_ptf == "maturite"))
        dur_det_new  <- .subset2(obligation@ptf, which(names_ptf == "duree_detention"))

        # Determination des oblig a vendre
        ind_oblig_sell <- which(maturite_new == dur_det_new)

        # S'il y a des obligs a vendre :
        if(length(ind_oblig_sell) > 0L) {

            # Extraction de donnees
            vr_ptf <- .subset2(obligation@ptf, which(names_ptf == "valeur_remboursement"))

            # Gain sur les obligations vendues
            vente <- sum(vr_ptf[ind_oblig_sell])

            # Suppression des oblig du PTF
            obligation@ptf <- obligation@ptf[-ind_oblig_sell,]

        }




        # Output
        return(list(obligation = obligation,
                    pmvl = pmvl,
                    coupons = coupons,
                    var_vnc = var_vnc,
                    vente = if.is_null(get0("vente"), 0)))
    }
)