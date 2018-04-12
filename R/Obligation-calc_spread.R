##' Fonction \code{calc_spread}
##'
##' Cette fonction permet de calculer les spread pour un PTF obligataire.
##'
##' @name calc_spread
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @param  yield_curve est un vecteur \code{numeric}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "calc_spread", def = function(obligation, yield_curve) {standardGeneric("calc_spread")})
setMethod(
    f = "calc_spread",
    signature = c(obligation = "Obligation", yield_curve = "numeric"),
    definition = function(obligation, yield_curve){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################

        # Extraction des donnees du PTF
        name_ptf_oblig  <- names(obligation@ptf)
        nominal_ptf     <- .subset2(obligation@ptf, which(name_ptf_oblig == "nominal"))
        vr_ptf          <- .subset2(obligation@ptf, which(name_ptf_oblig == "valeur_remboursement"))
        vm_ptf          <- .subset2(obligation@ptf, which(name_ptf_oblig == "valeur_marche"))
        coupon_ptf      <- .subset2(obligation@ptf, which(name_ptf_oblig == "coupon"))
        maturite_ptf    <- .subset2(obligation@ptf, which(names_ptf == "maturite"))
        dur_det_ptf     <- .subset2(obligation@ptf, which(names_ptf == "duree_detention"))

        # Calcul de la maturite residuelle du PTF
        mat_res_ptf <- maturite_ptf - dur_det_ptf




        ## ###########################
        ##      Calcul des TRI
        ## ###########################

        # Initialisation du vecteur contenant les nouveaux TRI
        spread <- rep(x = NA, length = nrow(obligation@ptf))

        for(mat_res in uniq_mat_res) {

            # Lignes correspondantes a la mat_res en question
            id <- which(mat_res_ptf == mat_res)

            # Recherche du zero
            spread[id] <- newton_raphson(fun = function(x)
                sum(nominal_ptf[id] * coupon_ptf[id] * exp(-(yield_curve[1L:mat_res] + x))^(1L:mat_res)) + vr_ptf[id] * exp(-x * mat_res) - vm_ptf[id])
        }





        ## ###########################
        ##   Mise a jour de l'objet
        ## ###########################

        obligation@ptf$spread <- spread


        # Output
        return(list(obligation = obligation))
    }
)
