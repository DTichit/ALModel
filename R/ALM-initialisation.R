##' Fonction \code{initialisation_alm}.
##'
##' Cette fonction permet d'initialiser un objet de type \code{\link{ALM}} : chargement des donnees, aggregation, calcul des probas...
##'
##' @name initialisation_alm
##' @docType methods
##' @param alm est un objet de type \code{\link{ALM}}.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{ALM}} : \code{\link{load_alm}}.
##' @seealso Premiere aggregation d'un objet de type \code{\link{ALM}} : \code{\link{aggregation_alm}}.
##' @seealso Seconde aggregation pour les passifs \code{\link{ALM}} : \code{\link{aggregation_passif_2}}.
##' @export
##' @include ALM-class.R ALM-aggregation.R ALM-load.R Passif-aggregation_2.R
##'
setGeneric(name = "initialisation_alm", def = function(alm) {standardGeneric("initialisation_alm")})
setMethod(
    f = "initialisation_alm",
    signature = c(alm = "ALM"),
    definition = function(alm){

        # Premiere aggregation
        alm <- aggregation_alm(alm = alm)

        # Calcul des taux actuariels et spread sur le PTF obligataire
        alm@system@actif@ptf_actif@obligation@ptf$tri <- calc_tri(alm@system@actif@ptf_actif@obligation)

        # Effectuer une premiere simulation permettant de calculer les probas
        system <- calc_be_simu(alm = alm, num_sim = 1L)[["system"]]

        # Mise a jour des tables de probabilites
        alm@system@passif@ptf_passif@epargne@proba <- system@passif@ptf_passif@epargne@proba

        # Mise a jour du booleen evitant un 2nd calcul de probas
        alm@system@passif@hyp_passif@calc_proba <- FALSE

        # Seconde aggregation : Apres le calcul des probas
        alm@system@passif <- aggregation_passif_2(alm@system@passif)


        # Output
        return(alm)
    }
)
