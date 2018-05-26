##' Fonction \code{gestion_tresorerie}
##'
##' Cette fonction permet de gerer un portfeuille monetaire : recolte des interets monetaires.
##'
##' @name gestion_tresorerie
##' @docType methods
##' @param tresorerie est un objet de type \code{\link{Tresorerie}}.
##' @param hyp_actif est un objet de type \code{\link{HypActif}}.
##' @param an est un \code{integer} reprensentant l'annee sur laquelle on travaille.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Tresorerie-class.R HypActif-class.R
##'
setGeneric(name = "gestion_tresorerie", def = function(tresorerie, hyp_actif, an) {standardGeneric("gestion_tresorerie")})
setMethod(
    f = "gestion_tresorerie",
    signature = c(tresorerie = "Tresorerie", hyp_actif = "HypActif", an = "integer"),
    definition = function(tresorerie, hyp_actif, an){




        ## ######################################################
        ## ######################################################
        ##
        ##          Calcul des interets monetaires
        ##
        ## ######################################################
        ## ######################################################


        # Taux court
        taux <- hyp_actif@esg_simu$monetaire[an]

        # Calcul des interets
        interets <- tresorerie@solde * taux







        # Output
        return(list(tresorerie = tresorerie,
                    interets = sum(interets)))
    }
)
