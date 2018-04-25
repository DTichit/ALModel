##' Fonction \code{eval_coupon}
##'
##' Cette fonction permet de calculer les coupons pour un portfeuille obligataire.
##'
##' @name eval_coupon
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "eval_coupon", def = function(obligation) {standardGeneric("eval_coupon")})
setMethod(
    f = "eval_coupon",
    signature = c(obligation = "Obligation"),
    definition = function(obligation){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf_oblig <- names(obligation@ptf)
        nominal_ptf <- .subset2(obligation@ptf, which(name_ptf_oblig == "nominal"))
        coupon_ptf  <- .subset2(obligation@ptf, which(name_ptf_oblig == "coupon"))
        spread_ptf  <- .subset2(obligation@ptf, which(name_ptf_oblig == "spread"))



        ## ###########################
        ##      Calcul de coupons
        ## ###########################
        coupons <- coupon_ptf * nominal_ptf



        # Output
        return(list(coupon = sum(coupons)))
    }
)
