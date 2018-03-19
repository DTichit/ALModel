##' Fonction \code{eval_dividende}
##'
##' Cette fonction permet de calculer les dividendes pour un portfeuille d'actions.
##'
##' @name eval_dividende
##' @docType methods
##' @param action est un objet de type \code{\link{Action}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Action-class.R
##'
setGeneric(name = "eval_dividende", def = function(action) {standardGeneric("eval_dividende")})
setMethod(
    f = "eval_dividende",
    signature = c(action = "Action"),
    definition = function(action){

        warning("Fonction a coder : voir l'ESG")

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf_action <- names(action@ptf)



        ## ###########################
        ##          Dividendes
        ## ###########################
        dividendes <- rep(0, nrow(action@ptf))


        # Output
        return(list(dividende = sum(dividendes)))
    }
)
