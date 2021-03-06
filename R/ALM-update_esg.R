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
##' @include ALM-class.R HypActif-class.R
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

        # Extraction de donnees
        ctz_nom <- alm@hyp_alm@esg@ctz_nom[SimID == num_simu]
        inflation <- alm@hyp_alm@esg@inflation[SimID == num_simu, Inflation]

        # Extraction des donnees et mise a jour de l'attribut
        alm@system@actif@hyp_actif@esg_simu <- list(ctz_nom        = as.data.frame(ctz_nom),
                                                    eq_dividends   = alm@hyp_alm@esg@eq_dividends[SimID == num_simu, EqDividends],
                                                    eq_index       = alm@hyp_alm@esg@eq_index[SimID == num_simu, EqIndex],
                                                    im_index       = alm@hyp_alm@esg@im_index[SimID == num_simu, ImIndex],
                                                    im_loyer       = alm@hyp_alm@esg@im_loyer[SimID == num_simu, ImLoyer],
                                                    monetaire      = ctz_nom[Maturite == 1L & ProjYr >= 1L, ZeroCoupon],
                                                    inflation      = inflation)





        ## ######################################################
        ## ######################################################
        ##
        ##      Mise a jour des donnees sur la partie passif
        ##
        ## ######################################################
        ## ######################################################

        # warning("Penser a rebrancher cette partie !")
        # Calcul et mise a jour des differents attributs
        alm@system@passif@hyp_passif@cible <- list(epargne = pmax(ctz_nom[Maturite == 10L, ZeroCoupon], 0))
        # alm@system@passif@hyp_passif@cible <- list(epargne = rep(0, 100L))
        alm@system@passif@hyp_passif@esg_simu <- list(inflation = inflation)






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
