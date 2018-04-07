##' Fonction \code{update_esg}.
##'
##' Cette fonction permet de mettre a jour les donnees de l'ESG dans la classe \code{\link{HypActif}}.
##' Elle met les donnees a jour pour une simulation.
##'
##' @name update_esg
##' @docType methods
##' @param alm est un objet de type \code{\link{ALM}}.
##' @param num_simu est un \code{integer} representant le numero de simulation sur lequel on travaille.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include HypActif-class.R ALM-class.R
##'
setGeneric(name = "update_esg", def = function(alm, num_simu){standardGeneric("update_esg")})
setMethod(
    f = "update_esg",
    signature = c(alm = "ALM", num_simu = "integer"),
    definition = function(alm, num_simu){


        ## ######################################################
        ## ######################################################
        ##
        ##      Mise a jour des donnees sur la partie actif
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de la courbe ZC
        ctz_nom <- alm@hyp_alm@esg@ctz_nom[SimID == num_simu]

        # Extraction des donnees et mise a jour de l'attribut
        alm@system@actif@hyp_actif@esg_simu <- list(ctz_nom        = as.data.frame(ctz_nom),
                                                    eq_dividends   = alm@hyp_alm@esg@eq_dividends[SimID == num_simu, EqDividends],
                                                    eq_index       = alm@hyp_alm@esg@eq_index[SimID == num_simu, EqIndex],
                                                    im_index       = alm@hyp_alm@esg@im_index[SimID == num_simu, ImIndex],
                                                    im_loyer       = alm@hyp_alm@esg@im_loyer[SimID == num_simu, ImLoyer],
                                                    inflation      = alm@hyp_alm@esg@inflation[SimID == num_simu, Inflation])





        ## ######################################################
        ## ######################################################
        ##
        ##      Mise a jour des donnees sur la partie passif
        ##
        ## ######################################################
        ## ######################################################

        # Calcul et mise a jour de l'attribut cible
        alm@system@passif@hyp_passif@cible <- list(epargne = ctz_nom[Maturite == 10L & ProjYr > 0L, ZeroCoupon])






        ## ######################################################
        ## ######################################################
        ##
        ##      Extraction des coefficients d'actualisation
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnees
        coef_actu <- alm@hyp_alm@esg@coef_actu[SimID == num_simu]






        # Output
        return(list(alm = alm,
                    coef_actu = coef_actu))
    }
)
