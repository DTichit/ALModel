##' Fonction \code{revalo_action}
##'
##' Cette fonction permet de revaloriser le portefeuille action. Calcule egalement les plus ou moins values latentes.
##'
##' @name revalo_action
##' @docType methods
##' @param action est un objet de type \code{\link{Action}}.
##' @param log_rdt est un \code{numeric} representant le log-rendement du PTF.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Action-class.R
##'
setGeneric(name = "revalo_action", def = function(action, log_rdt) {standardGeneric("revalo_action")})
setMethod(
    f = "revalo_action",
    signature = c(action = "Action", log_rdt = "numeric"),
    definition = function(action, log_rdt){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(action@ptf)
        vm_ptf <- .subset2(action@ptf, which(name_ptf == "valeur_marche"))
        vc_ptf <- .subset2(action@ptf, which(name_ptf == "valeur_comptable"))



        ## ###########################
        ##   Mise a jour des VM
        ## ###########################

        # Calcul des nouvelles VM
        vm_new  <- vm_ptf * exp(log_rdt)

        # Mise a jour de l'attribut
        action@ptf$valeur_marche <- vm_new



        ## ###########################
        ##       Calcul des PMVL
        ## ###########################

        # Calcul des PMVL
        pmvl <- vm_new - vc_ptf





        # Output
        return(list(action = action,
                    pmvl = pmvl))
    }
)
