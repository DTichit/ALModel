##' Fonction \code{besoin_revalo_ptf_passif}
##'
##' Cette fonction permet de calculer les besoins en revalorisation pour les differents PTF passifs d'une compagnie d'assurance :
##' \describe{
##' \item{besoins contractuels}{ : besoins pour assouvir les engagements contractuels.}
##' \item{besoins cibles}{ : besoins pour atteindre une revalorisation cible.}
##' }
##'
##' @name besoin_revalo_ptf_passif
##' @docType methods
##' @param ptf_passif est un objet de type \code{\link{PTFPassif}}.
##' @param cible est une \code{list} contenant des elements relatifs a la politique de revalorisation pour les differents passifs.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PTFPassif-class.R
##'
setGeneric(name = "besoin_revalo_ptf_passif", def = function(ptf_passif, cible) {standardGeneric("besoin_revalo_ptf_passif")})
setMethod(
    f = "besoin_revalo_ptf_passif",
    signature = c(ptf_passif = "PTFPassif", cible = "list"),
    definition = function(ptf_passif, cible){




        ## ######################################################
        ## ######################################################
        ##
        ##         Revalorisation des differents PTF
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Epargne
        ## ###########################

        # Appel de la fonction
        res_epargne <- besoin_revalo_epargne(epargne = ptf_passif@epargne, cible = cible[["epargne"]])






        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Besoin en revalorisation contractuelle
        besoin_contr <- list(epargne = res_epargne[["besoin_tmg"]])

        # Besoin en revalorisation cible
        besoin_cible <- list(epargne = res_epargne[["besoin_cible"]])




        # Output
        return(list(besoin_contr = besoin_contr,
                    besoin_cible = besoin_cible))
    }
)
