##' Fonction \code{calc_prod_fin}.
##'
##' Cette fonction permet de calculer les produits financiers pour un portefeuille financier : dividendes, coupons....
##'
##' @name calc_prod_fin
##' @docType methods
##' @param ptf_actif est un objet de type \code{\link{PTFActif}}.
##' @author Damien Tichit pour Sia Partners
##' @include PTFActif-class.R
##'
setGeneric(name = "calc_prod_fin", def = function(ptf_actif) {standardGeneric("calc_prod_fin")})
setMethod(
    f = "calc_prod_fin",
    signature = c(ptf_actif = "PTFActif"),
    definition = function(ptf_actif){



        ## ######################################################
        ## ######################################################
        ##
        ##  Calcul des produits financiers pour chacun des actifs
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##        Obligations
        ## ###########################

        # Calcul des coupons
        coupon <- calc_coupon(ptf_actif@obligation)



        ## ###########################
        ##          Actions
        ## ###########################

        # Calcul des dividendes
        dividende <- calc_dividende(ptf_actif@action)



        ## ###########################
        ##      Immobilier
        ## ###########################

        # Projection sur une annee de l'immo
        loyer <- calc_loyer(ptf_actif@immobilier)




        # Output
        return(list(actif = actif))
    }
)
