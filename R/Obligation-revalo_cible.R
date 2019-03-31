##' Fonction \code{revalo_obligation_cible}
##'
##' Cette fonction permet de revaloriser les differentes obligations d'un portefeuille obligataire cible :
##' calcul des nouveaux coupons et mise a jour de la valeur de marche.
##'
##' @name revalo_obligation_cible
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @param  yield_curve est un \code{numeric} contenant les prix zero-coupon.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "revalo_obligation_cible", def = function(obligation, yield_curve) {standardGeneric("revalo_obligation_cible")})
setMethod(
    f = "revalo_obligation_cible",
    signature = c(obligation = "Obligation", yield_curve = "numeric"),
    definition = function(obligation, yield_curve){

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        names_ptf <- names(obligation@ptf)
        maturite_ptf <- .subset2(obligation@ptf, which(names_ptf == "maturite"))




        ## ###########################
        ## Calcul des nouveaux coupons
        ## ###########################

        # Calcul des nouveaux taux coupons
        coupon_ptf <- sapply(1L:nrow(obligation@ptf), function(id){

            # Extraction de donnees
            maturite <- maturite_ptf[id]

            # Extraction de donnees
            zc <- exp(-(yield_curve[1L:maturite]) * 1L:maturite)

            # Calcul du nouveau coupon
            coupon <- (1 - zc[maturite]) / sum(zc)


            # Output
            return(coupon)
        })




        ## ###########################
        ##   Mise a jour de l'objet
        ## ###########################

        # Mise a jour de l'attribut
        obligation@ptf$coupon           <- coupon_ptf



        # Output
        return(list(obligation = obligation))
    }
)
