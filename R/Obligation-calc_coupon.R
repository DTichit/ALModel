##' Fonction \code{calc_coupon}
##'
##' Cette fonction permet de calculer les coupons pour un portfeuille obligataire.
##'
##' @name calc_coupon
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "calc_coupon", def = function(obligation) {standardGeneric("calc_coupon")})
setMethod(
    f = "calc_coupon",
    signature = c(obligation = "Obligation"),
    definition = function(obligation){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf_oblig <- names(obligation@ptf)



        ## ###########################
        ##          Coupons
        ## ###########################

        # Extraction de donnees
        name_ptf_oblig <- names(obligation@ptf)
        nb_ptf      <- .subset2(obligation@ptf, which(name_ptf_oblig == "nombre"))
        nominal_ptf <- .subset2(obligation@ptf, which(name_ptf_oblig == "nominal"))
        tx_coup_ptf <- .subset2(obligation@ptf, which(name_ptf_oblig == "tx_coupon"))

        # Calcul des coupons
        coupons <- nb_ptf * tx_coup_ptf * nominal_ptf




        # Output
        return(list(coupon = coupons))
    }
)
