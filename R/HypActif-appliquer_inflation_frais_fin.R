##' Fonction \code{appliquer_inflation_frais_fin}
##'
##' Cette fonction permet d'appliquer l'inflation sur les frais financiers.
##'
##' @name appliquer_inflation_frais_fin
##' @docType methods
##' @param hyp_actif est un objet de type \code{\link{HypActif}}.
##' @param an est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @include HypActif-class.R
##'
setGeneric(name = "appliquer_inflation_frais_fin", def = function(hyp_actif, an) {standardGeneric("appliquer_inflation_frais_fin")})
setMethod(
    f = "appliquer_inflation_frais_fin",
    signature = c(hyp_actif = "HypActif", an = "integer"),
    definition = function(hyp_actif, an) {



        ## ###########################
        ##    Extraction inflation
        ## ###########################

        inf <- hyp_actif@esg_simu$inflation[an]





        ## ###########################
        ##      Appliquer inflation
        ## ###########################

        hyp_actif@frais_fin$frais_prod  <- hyp_actif@frais_fin$frais_prod * (1 - inf)
        hyp_actif@frais_fin$frais_vm    <- hyp_actif@frais_fin$frais_vm * (1 - inf)




        # Output
        return(list(hyp_actif = hyp_actif))
    }
)
