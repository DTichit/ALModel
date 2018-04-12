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
        va_ptf          <- .subset2(obligation@ptf, which(name_ptf_oblig == "valeur_achat"))
        coupon_ptf      <- .subset2(obligation@ptf, which(name_ptf_oblig == "coupon"))
        maturite_ptf    <- .subset2(obligation@ptf, which(names_ptf == "maturite"))
        dur_det_ptf     <- .subset2(obligation@ptf, which(names_ptf == "duree_detention"))

        # Calcul de la maturite residuelle du PTF
        mat_res_ptf <- maturite_ptf - dur_det_ptf





        ## ###########################
        ##      Calcul des TRI
        ## ###########################

        # Initialisation du vecteur contenant les nouveaux TRI
        tri <- rep(x = NA, length = nrow(obligation@ptf))

        for(mat_res in uniq_mat_res) {

            # Lignes correspondantes a la mat_res en question
            id <- which(mat_res_ptf == mat_res)

            # Recharche du zero
            tri[id] <- newton_raphson(function(x)
                sum(nominal_ptf[id] * coupon_ptf[id] * exp(-x)^1L:mat_res) + nominal_ptf[id] * exp(-x)^mat_res - va_ptf[id])
        }





        ## ###########################
        ##   Mise a jour de l'objet
        ## ###########################

        obligation@ptf$tri <- tri


        # Output
        return(list(obligation = obligation))
    }
)
