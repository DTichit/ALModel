##' Fonction \code{revalo_epargne}
##'
##' Cette fonction permet de revaloriser un portefeuille Epargne. La distribution se fait de la faca suivante :
##' \describe{
##' \item{Revalorisation cible}{ : la revalorisation s'effectue de telle façon a ce que tous les MP aient la meme revalorisation et en se rapprochant du taux cible.}
##' \item{Revalorisation supplementaire}{ : la revalorisation s'effectue au proportionnellement aux PM.}
##' }
##' Les besoins reglementaires sont tout d'abord decompté du montant devant etre obligatoirement distribue.
##'
##' @name revalo_epargne
##' @docType methods
##' @param epargne est un objet de type \code{\link{Epargne}}.
##' @param revalo_cible est un \code{numeric} representant le montant de revalorisation cible a distibuer.
##' @param revalo_supp est un \code{numeric} representant le montant de revalorisation supplementaire a distibuer.
##' @param cible est un \code{numeric} representant un taux cible.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Epargne-class.R
##'
setGeneric(name = "revalo_epargne", def = function(epargne, revalo_cible, revalo_supp, cible) {standardGeneric("revalo_epargne")})
setMethod(
    f = "revalo_epargne",
    signature = c(epargne = "Epargne", revalo_cible = "numeric", revalo_supp = "numeric", cible = "numeric"),
    definition = function(epargne, revalo_cible, revalo_supp, cible){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(epargne@ptf)




        ## ######################################################
        ## ######################################################
        ##
        ##         Calcul des differentes revalorisations
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnees
        pm_ptf_epargne   <- .subset2(epargne@ptf, which(name_ptf == "pm"))


        ## ###########################
        ## Revalorisation contractuelle
        ## ###########################

        # Extraction de donnees
        tmg_ptf_epargne  <- .subset2(epargne@ptf, which(name_ptf == "tmg"))

        # Revalorisation au TMG par model point
        revalo_tmg_mp <- pm_ptf_epargne * tmg_ptf_epargne




        ## ###########################
        ##    Revalorisation cible
        ## ###########################

        # Proportion a distribuer
        besoin_cible <- (cible - tmg_ptf_epargne) * pm_ptf_epargne

        # MP pour lesquels le cible est superieur au TMG
        id_sup <- which(besoin_cible > 0)


        if(length(id_sup) == length(besoin_cible)) {

            # Taux de revalorisation
            tx_revalo_cible <- (sum(revalo_tmg_mp + pm_ptf_epargne) + revalo_cible) / sum(pm_ptf_epargne)

            # Revalorisation cible par model point
            revalo_cible_mp <- (tx_revalo_cible - ( 1 + tmg_ptf_epargne)) * pm_ptf_epargne

        } else {

            # Initialisation du vecteur de revalorisation cible
            revalo_cible_mp <- rep(0, length = length(besoin_cible))

            # Completer le vecteur
            revalo_cible_mp[id_sup] <- revalo_cible * pm_ptf_epargne[id_sup] / sum(pm_ptf_epargne[id_sup])

        }




        ## ###########################
        ## Revalorisation supplementaire
        ## ###########################

        # Revalorisation supplementaire par model point
        revalo_supp_mp <- revalo_supp * pm_ptf_epargne / sum(pm_ptf_epargne)






        ## ######################################################
        ## ######################################################
        ##
        ##                  Mise a jour du PTF
        ##
        ## ######################################################
        ## ######################################################

        # Calcul des nouvelles PM
        new_pm <- pm_ptf_epargne + (revalo_tmg_mp + revalo_cible_mp + revalo_supp_mp)

        # Calcul du taux de revalorisation
        tx_revalo <- new_pm / pm_ptf_epargne

        # Mise a jour des PM
        epargne@ptf$pm <- new_pm
        epargne@ptf$revalo_prec <- tx_revalo - 1





        # Output
        return(list(epargne = epargne))
    }
)