##' Fonction \code{calc_dividende}
##'
##' Cette fonction permet de calculer les dividende pour un portfeuille d'actions.
##'
##' @name calc_dividende
##' @docType methods
##' @param action est un objet de type \code{\link{Action}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Action-class.R
##'
setGeneric(name = "calc_dividende", def = function(action) {standardGeneric("calc_dividende")})
setMethod(
    f = "calc_dividende",
    signature = c(action = "Action"),
    definition = function(action){

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf_action <- names(action@ptf)



        ## ###########################
        ##          Dividendes
        ## ###########################

        # Extraction de donnees
        vm_action <- .subset2(action@ptf, which(name_ptf_action == "valeur_marche"))
        dividende_ptf <- .subset2(action@ptf, which(name_ptf_action == "dividende"))

        # Calcul des dividendes
        dividendes <- dividende_ptf * vm_action

        warning("A voir si le calcul de dividende est correct ! (A priori, non)")


        # Output
        return(list(dividende = dividendes))
    }
)
