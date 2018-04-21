##' Fonction \code{calc_tri}
##'
##' Cette fonction permet de calculer les taux actuariels pour un PTF obligataire.
##'
##' @name calc_tri
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "calc_tri", def = function(obligation) {standardGeneric("calc_tri")})
setMethod(
    f = "calc_tri",
    signature = c(obligation = "Obligation"),
    definition = function(obligation){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################

        # Extraction des donnees du PTF
        name_ptf_oblig  <- names(obligation@ptf)
        nominal_ptf     <- .subset2(obligation@ptf, which(name_ptf_oblig == "nominal"))
        achat_ptf       <- .subset2(obligation@ptf, which(name_ptf_oblig == "valeur_achat"))
        vr_ptf          <- .subset2(obligation@ptf, which(name_ptf_oblig == "valeur_remboursement"))
        # vnc_ptf          <- .subset2(obligation@ptf, which(name_ptf_oblig == "valeur_nette_comptable"))
        coupon_ptf      <- .subset2(obligation@ptf, which(name_ptf_oblig == "coupon"))
        maturite_ptf    <- .subset2(obligation@ptf, which(name_ptf_oblig == "maturite"))
        # dur_det_ptf     <- .subset2(obligation@ptf, which(name_ptf_oblig == "duree_detention"))

        # Calcul de la maturite residuelle du PTF
        # mat_res_ptf <- maturite_ptf - dur_det_ptf





        ## ###########################
        ##      Calcul des TRI
        ## ###########################

        # Calcul des tri
        # tri <- sapply(1L:nrow(obligation@ptf), function(id) {
        #     newton_raphson(function(x)
        #         sum(nominal_ptf[id] * coupon_ptf[id] * exp(-x * (1L:mat_res_ptf[id]))) + vr_ptf[id] * exp(-x * mat_res_ptf[id]) - vnc_ptf[id])
        # })
        tri <- sapply(1L:nrow(obligation@ptf), function(id) {
            newton_raphson(function(x)
                sum(nominal_ptf[id] * coupon_ptf[id] * exp(-x * (1L:maturite_ptf[id]))) + vr_ptf[id] * exp(-x * maturite_ptf[id]) - achat_ptf[id])
        })





        # Output
        return(tri)
    }
)