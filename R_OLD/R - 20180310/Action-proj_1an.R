##' Fonction \code{proj_1an_action}
##'
##' Cette fonction permet de projeter horizon 1 an un portefeuille d'actions.
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
        ##        Evaluation des gains : dividendes et PMVL
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Dividendes
        ## ###########################

        # Extraction de donnees
        vm_action <- .subset2(action@ptf, which(name_ptf_action == "valeur_marche"))
        dividende_ptf <- .subset2(action@ptf, which(name_ptf_action == "dividende"))
        nb_ptf        <- .subset2(action@ptf, which(name_ptf_action == "nombre"))

        # Calcul des coupons
        dividendes <- dividende_ptf * vm_action_new


        ## ###########################
        ##            PMVL
        ## ###########################

        # Calcul des PMVL
        pmvl_action <- vm_action_new - vm_action_prev




        # Output
        return(list(action = action,
                    gain = list(dividendes = dividendes,
                                pmvl_action = pmvl_action)))
    }
)
