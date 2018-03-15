##' Fonction \code{proj_1an_action}
##'
##' Cette fonction permet de projeter horizon 1 an un portefeuille d'actions. Elle calcule notamment les dividendes.
##'
##' @name proj_1an_action
##' @docType methods
##' @param action est un objet de type \code{\link{Action}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Action-class.R
##'
setGeneric(name = "proj_1an_action", def = function(action) {standardGeneric("proj_1an_action")})
setMethod(
    f = "proj_1an_action",
    signature = c(action = "Action"),
    definition = function(action){

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf_action <- names(action@ptf)




        ## ######################################################
        ## ######################################################
        ##
        ##              Evaluation des dividendes
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        vm_action <- .subset2(action@ptf, which(name_ptf_action == "valeur_marche"))
        dividende_ptf <- .subset2(action@ptf, which(name_ptf_action == "dividende"))

        # Calcul des dividendes
        dividendes <- dividende_ptf * vm_action

        warning("A voir si le calcul des dividendes est correct ! (A priori, non)")



        # Output
        return(list(action = action,
                    flux = list(dividende = sum(dividendes))))
    }
)
