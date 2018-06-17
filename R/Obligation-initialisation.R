##' Fonction \code{initialisation_obligation}.
##'
##' Cette fonction permet d'initialiser un PTF obligataire : calcul du spread et du TRI.
##'
##' @name initialisation_obligation
##' @docType methods
##' @param alm est un objet de type \code{\link{Obligation}}.
##' @param esg est un objet de type \code{\link{ESG}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R ESG-class.R
##'
setGeneric(name = "initialisation_obligation", def = function(obligation, esg) {standardGeneric("initialisation_obligation")})
setMethod(
    f = "initialisation_obligation",
    signature = c(obligation = "Obligation", esg = "ESG"),
    definition = function(obligation, esg){



        ## ###########################
        ##      Calcul du TRI
        ## ###########################

        # Appel de la fonction
        obligation@ptf$tri <- calc_tri(obligation)




        ## ###########################
        ##      Calcul du spread
        ## ###########################

        # Extraction de la courbe des taux pour l'annee 0
        yield_curve_0 <- esg@ctz_nom[SimID == 1L & ProjYr == 0L]$ZeroCoupon

        # Calcul du spread
        if(sum(obligation@ptf$valeur_marche) > 0)
            obligation@ptf$spread <- calc_spread(obligation = obligation, yield_curve = yield_curve_0)
        else
            obligation@ptf$spread <- 0




        # Output
        return(obligation)
    }
)
