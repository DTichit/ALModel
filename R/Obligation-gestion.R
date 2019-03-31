##' Fonction \code{gestion_obligation}
##'
##' Cette fonction permet de gerer un portfeuille obligation : recolte des coupons, recalcul des VM, VNC, VR, vieillissement...
##'
##' @name gestion_obligation
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @param ctz_nom est un objet de type \code{data.frame} contenant la courbe des taux.
##' @param an est un \code{integer} reprensentant l'annee sur laquelle on travaille.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R HypActif-class.R
##'
setGeneric(name = "gestion_obligation", def = function(obligation, ctz_nom, an) {standardGeneric("gestion_obligation")})
setMethod(
    f = "gestion_obligation",
    signature = c(obligation = "Obligation", ctz_nom = "data.frame", an = "integer"),
    definition = function(obligation, ctz_nom, an){



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
        obligation@ptf$duree_detention <- pmin(.subset2(obligation@ptf, which(names_ptf == "duree_detention")) + 1L, obligation@ptf$maturite)






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
        ## Extraction de la courbe des taux
        ## ###########################
        name_ctz <- names(ctz_nom)
        num <- which(.subset2(ctz_nom, which(name_ctz == "ProjYr")) == (an))
        yield_curve <- .subset2(ctz_nom, which(name_ctz == "ZeroCoupon"))[num]


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        vnc_ptf     <- .subset2(obligation@ptf, which(names_ptf == "valeur_nette_comptable"))
        vm_ptf      <- .subset2(obligation@ptf, which(names_ptf == "valeur_marche"))
        nominal_ptf <- .subset2(obligation@ptf, which(names_ptf == "nominal"))
        coupon_ptf  <- .subset2(obligation@ptf, which(names_ptf == "coupon"))
        remboursement_ptf  <- .subset2(obligation@ptf, which(names_ptf == "remboursement"))
        mat_ptf     <- .subset2(obligation@ptf, which(names_ptf == "maturite"))
        dur_det_ptf <- .subset2(obligation@ptf, which(names_ptf == "duree_detention"))
        tri_ptf     <- .subset2(obligation@ptf, which(names_ptf == "tri"))
        spread_ptf  <- .subset2(obligation@ptf, which(names_ptf == "spread"))

        # Calcul de la maturite residuelle du PTF
        mat_res_ptf <- mat_ptf - dur_det_ptf




        ## ###########################
        ##   Calcul des nouvelles VM
        ## ###########################

        new_vm <- calcul_vm_obligation(nominal = nominal_ptf, coupon = coupon_ptf, mat_res = mat_res_ptf, remboursement = remboursement_ptf, spread = spread_ptf, yield = yield_curve)

        # Mise a jour de l'attribut
        obligation@ptf$valeur_marche <- new_vm




        ## ###########################
        ##  Calcul des nouvelles VNC
        ## ###########################

        # Calcul de la nouvelle VNC
        new_vnc <- exp(-spread_ptf) * (vnc_ptf + exp(spread_ptf - tri_ptf * (mat_res_ptf + 1L)) * (nominal_ptf * remboursement_ptf * (exp(tri_ptf) - 1) - (nominal_ptf * coupon_ptf)))
        new_vnc[which(mat_res_ptf==0L)] <- nominal_ptf[which(mat_res_ptf==0L)] * remboursement_ptf[which(mat_res_ptf==0L)]

        # new_vnc <- sapply(1L:nrow(obligation@ptf), function(id){
        #
        #     # Extraction de donnees
        #     mat_res <- mat_res_ptf[id]
        #
        #     # Calcul de la VNC
        #     if(mat_res > 0)
        #         vnc <- sum(coupon_ptf[id] * nominal_ptf[id] * exp(-tri_ptf[id] * 1L:mat_res)) + vr_ptf[id] * exp(-(tri_ptf[id] * mat_res))
        #     else
        #         vnc <- vr_ptf[id]
        #
        #     # Output
        #     return(vnc)
        # })

        # Mise a jour du PTF
        obligation@ptf$valeur_nette_comptable <- new_vnc

        # Variation de la VNC
        var_vnc <- new_vnc - vnc_ptf




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

        # S'il y a des obligs arrivant a maturite
        if(length(ind_oblig_sell) > 0L) {

            # Extraction de donnees
            nominal_ptf         <- .subset2(obligation@ptf, which(names_ptf == "nominal"))
            remboursement_ptf   <- .subset2(obligation@ptf, which(names_ptf == "remboursement"))

            # Gain sur les obligations remboursees
            vente <- sum(nominal_ptf[ind_oblig_sell] * remboursement_ptf[ind_oblig_sell] )

            # Suppression des oblig du PTF
            obligation@ptf <- obligation@ptf[-ind_oblig_sell,]

        }





        # Output
        return(list(obligation = obligation,
                    pmvl = sum(pmvl),
                    coupons = sum(coupons),
                    var_vnc = sum(var_vnc),
                    vente = if.is_null(get0("vente"), 0)))
    }
)
