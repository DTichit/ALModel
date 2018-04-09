##' Fonction \code{eval_dividende}
##'
##' Cette fonction permet de calculer les dividendes pour un portfeuille d'actions.
##'
##' @name eval_dividende
##' @docType methods
##' @param action est un objet de type \code{\link{Action}}.
##' @param div est un \code{numeric} represente la proportion de l'encours action versee au titre des dividendes.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Action-class.R
##'
setGeneric(name = "eval_dividende", def = function(action, div) {standardGeneric("eval_dividende")})
setMethod(
    f = "eval_dividende",
    signature = c(action = "Action", div = "numeric"),
    definition = function(action, div){



        ## ###########################
        ##          Dividendes
        ## ###########################
        dividendes <- .subset2(action@ptf, which(names(action@ptf) == "valeur_marche")) * div


        # Output
        return(list(dividende = sum(dividendes)))
    }
)
