##' Fonction \code{aggregation_action}.
##'
##' Cette fonction permet d'aggreger les model-point pour un portfeuille d'actions.
##'
##' @name aggregation_action
##' @docType methods
##' @param action est un objet de type \code{\link{Action}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Action-class.R
##'
setGeneric(name = "aggregation_action", def = function(action) {standardGeneric("aggregation_action")})
setMethod(
    f = "aggregation_action",
    signature = c(action = "Action"),
    definition = function(action){


        # Aggregation des donnees
        temp <- colSums(action@ptf[c("valeur_comptable", "valeur_marche")])


        # Mise a jour du dataframe
        action@ptf <- data.frame(id_mp = paste("ac", 1L, sep = "-"), valeur_comptable = temp[["valeur_comptable"]],
                                 valeur_marche = temp[["valeur_marche"]])

        # Output
        return(action)
    }
)
