##' Fonction \code{besoin_revalo_epargne}
##'
##' Cette fonction permet de calculer les besoins en revalorisation :
##' \describe{
##' \item{besoins contractuels}{ : besoins pour assouvir les engagements contractuels, a savoir le tmg.}
##' \item{besoins cibles}{ : besoins pour atteindre une revalorisation egale au taux cible afin de minimiser les rachats conjoncturels.}
##' }
##'
##' @name besoin_revalo_epargne
##' @docType methods
##' @param epargne est un objet de type \code{\link{Epargne}}.
##' @param cible est un \code{numeric} representant un taux cible.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Epargne-class.R
##'
setGeneric(name = "besoin_revalo_epargne", def = function(epargne, cible) {standardGeneric("besoin_revalo_epargne")})
setMethod(
    f = "besoin_revalo_epargne",
    signature = c(epargne = "Epargne", cible = "numeric"),
    definition = function(epargne, cible){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(epargne@ptf)



        ## ######################################################
        ## ######################################################
        ##
        ##              Calcul des differents besoins
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnees
        pm_ptf_epargne   <- .subset2(epargne@ptf, which(name_ptf == "pm"))


        ## ###########################
        ##   Besoin contractuel
        ## ###########################

        # Extraction de donnees
        tmg_ptf_epargne  <- .subset2(epargne@ptf, which(name_ptf == "tmg"))

        # Calcul besoin revalorisation sur les PM
        besoin_tmg <- sum(pm_ptf_epargne * tmg_ptf_epargne)



        ## ###########################
        ##      Besoin cible
        ## ###########################

        # Calcul besoin revalorisation sur les PM
        besoin_cible <- max(sum(pm_ptf_epargne * cible) - besoin_tmg, 0)





        # Output
        return(list(besoin_tmg = besoin_tmg,
                    besoin_cible = besoin_cible))
    }
)
