##' Fonction \code{eval_prod_fin}.
##'
##' Cette fonction permet d'evaluer les produits financiers pour les differentes classes d'actifs.
##'
##' @name eval_prod_fin
##' @docType methods
##' @param ptf_actif est un objet de type \code{\link{PTFActif}}.
##' @author Damien Tichit pour Sia Partners
##' @include PTFActif-class.R
##'
setGeneric(name = "eval_prod_fin", def = function(ptf_actif) {standardGeneric("eval_prod_fin")})
setMethod(
    f = "eval_prod_fin",
    signature = c(ptf_actif = "PTFActif"),
    definition = function(ptf_actif){



        ## ######################################################
        ## ######################################################
        ##
        ##          Gestion des differents actifs
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Actions
        ## ###########################

        # Appel de la fonction
        res_action <- eval_dividende(ptf_actif@action)



        ## ###########################
        ##        Obligations
        ## ###########################

        # Appel de la fonction
        res_oblig <- eval_coupon(ptf_actif@obligation)



        ## ###########################
        ##      Immobilier
        ## ###########################

        # Appel de la fonction
        res_immo <- eval_loyer(ptf_actif@immobilier)






        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Produits financiers
        prod_fin <- list(action = res_action[["dividende"]],
                         obligation = res_oblig[["coupon"]],
                         immobilier = res_immo[["loyer"]])




        # Output
        return(list(prod_fin = prod_fin))
    }
)
