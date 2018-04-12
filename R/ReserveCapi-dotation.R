##' Fonction \code{dotation_reserve_capi}.
##'
##' Cette fonction permet de doter la Reserve de capitalisation. Sa modelisation reflete sa definition (Article A333.3).
##'
##' En cas de plus value (montant > 0) : dotation totale.
##' En cas de moins value (montant < 0) : reprise dans la reserve de capitalisation, sous reserve qu'il soit encore positif.
##'
##' @name dotation_reserve_capi
##' @docType methods
##' @param reserve_capi est un objet de type \code{\link{ReserveCapi}}.
##' @param pmvr est un \code{numeric} representant le montant les plus ou moins value resalisees a doter.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include ReserveCapi-class.R
##'
setGeneric(name = "dotation_reserve_capi", def = function(reserve_capi, pmvr) {standardGeneric("dotation_reserve_capi")})
setMethod(
    f = "dotation_reserve_capi",
    signature = c(reserve_capi = "ReserveCapi", pmvr = "numeric"),
    definition = function(reserve_capi, pmvr){



        # Dans le cas de plus value realise : On dote tout
        if(pmvr >= 0) {

            # Mise a jour de l'attribut
            reserve_capi@montant <- reserve_capi@montant + pmvr

            # Reste
            reste_pmv <- 0

            # Flux
            flux <- pmvr

        } else {

            # Calcul de la reprise maximale pouvant etre effectue
            reprise <- min(reserve_capi@montant, abs(pmvr))

            # Mise a jour de l'objet
            reserve_capi@montant <- reserve_capi@montant - reprise

            # Reste
            reste_pmv <- abs(pmvr) - reprise

            # Flux
            flux <- -reprise

        }



        # Output
        return(list(reserve_capi = reserve_capi,
                    reste_pmv = reste_pmv,
                    flux = flux))
    }
)
