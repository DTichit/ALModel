##' Fonction \code{revalo_action}
##'
##' Cette fonction permet de revaloriser le portefeuille action. Calcule egalement les plus ou moins values latentes.
##'
##' @name revalo_action
##' @docType methods
##' @param action est un objet de type \code{\link{Action}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Action-class.R
##'
setGeneric(name = "revalo_action", def = function(action) {standardGeneric("revalo_action")})
setMethod(
    f = "revalo_action",
    signature = c(action = "Action"),
    definition = function(action){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(action@ptf)



        ## ###########################
        ##   Mise a jour des VM
        ## ###########################

        # Extraction des donnees
        vm_prev <- .subset2(action@ptf, which(name_ptf == "valeur_marche"))

        # Calcul des nouvelles VM
        warning("Revalorisation des actions n'est pas branchee !")
        vm_new  <- vm_prev

        # Mise a jour de l'attribut
        action@ptf$valeur_marche <- vm_new



        ## ###########################
        ##       Calcul des PMVL
        ## ###########################

        # Calcul des PMVL
        pmvl <- vm_new - vm_prev





        # Output
        return(list(action = action,
                    pmvl = pmvl))
    }
)
