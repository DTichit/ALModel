##' Fonction \code{revalo_obligation}
##'
##' Cette fonction permet de revaloriser le portefeuille obligataire.
##'
##' @name revalo_obligation
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "revalo_obligation", def = function(obligation) {standardGeneric("revalo_obligation")})
setMethod(
    f = "revalo_obligation",
    signature = c(obligation = "Obligation"),
    definition = function(obligation){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(obligation@ptf)



        ## ######################################################
        ## ######################################################
        ##
        ##            Mise a jour des VM
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnees
        vm_prev <- .subset2(obligation@ptf, which(name_ptf == "valeur_marche"))

        # Calcul des nouvelles VM
        warning("Revalorisation des obligations n'est pas branchee !")
        vm_new  <- vm_prev

        # Mise a jour de l'attribut
        obligation@ptf$valeur_marche <- vm_new



        ## ###########################
        ##       Calcul des PMVL
        ## ###########################

        # Calcul des PMVL
        pmvl <- vm_new - vm_prev




        # Output
        return(list(obligation = obligation,
                    pmvl = pmvl))
    }
)
