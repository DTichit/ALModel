##' Fonction \code{initialisation_alm}.
##'
##' Cette fonction permet d'initialiser un objet de type \code{\link{ALM}} : chargement des donnees, aggregation, calcul des probas...
##'
##' @name initialisation_alm
##' @docType methods
##' @param alm est un objet de type \code{\link{ALM}}.
##' @param agreg est une valeur \code{logical} qui indique si les sorties relatives au passif doivent etre agregees. Par defaut, sa valeur est a TRUE.
##' @param init_oblig est un \code{logical} indiquant s'il est necessaire d'initialiser le PTF obligataire. Par defaut, sa valeur est egale a \code{TRUE}.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{ALM}} : \code{\link{load_alm}}.
##' @seealso Premiere aggregation d'un objet de type \code{\link{ALM}} : \code{\link{aggregation_alm}}.
##' @seealso Seconde aggregation pour les passifs \code{\link{ALM}} : \code{\link{aggregation_passif_2}}.
##' @export
##' @include ALM-class.R ALM-aggregation.R ALM-load.R Passif-aggregation_2.R
##'
setGeneric(name = "initialisation_alm", def = function(alm, init_oblig = TRUE, agreg_out = TRUE) {standardGeneric("initialisation_alm")})
setMethod(
    f = "initialisation_alm",
    signature = c(alm = "ALM"),
    definition = function(alm, init_oblig, agreg_out){

        # Premiere aggregation
        alm <- aggregation_alm(alm = alm)

        # Initialisation du PTF obligataire : calcul TRI et spread
        if(init_oblig)
            alm@system@actif@ptf_actif@obligation <- initialisation_obligation(obligation = alm@system@actif@ptf_actif@obligation, esg = alm@hyp_alm@esg)

        # Mise a jour du booleen relatif aux sorties
        alm@system@passif@hyp_passif@agreg_out <- agreg_out

        # Effectuer une premiere simulation permettant de calculer les probas
        system <- calc_be_simu(alm = alm, num_sim = 1L)[["system"]]

        # Mise a jour des tables de probabilites
        alm@system@passif@ptf_passif@epargne@proba <- system@passif@ptf_passif@epargne@proba

        # Mise a jour du booleen evitant un 2nd calcul de probas
        alm@system@passif@hyp_passif@calc_proba <- FALSE

        # Seconde aggregation : Apres le calcul des probas
        if(agreg_out)
            alm@system@passif <- aggregation_passif_2(alm@system@passif)


        # Output
        return(alm)
    }
)
