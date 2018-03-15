##' Fonction \code{revalo_immobilier}
##'
##' Cette fonction permet de revaloriser le portefeuille immobilier.
##'
##' @name revalo_immobilier
##' @docType methods
##' @param immobilier est un objet de type \code{\link{Immobilier}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R
##'
setGeneric(name = "revalo_immobilier", def = function(immobilier) {standardGeneric("revalo_immobilier")})
setMethod(
    f = "revalo_immobilier",
    signature = c(immobilier = "Immobilier"),
    definition = function(immobilier){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(immobilier@ptf)



        ## ######################################################
        ## ######################################################
        ##
        ##            Mise a jour des VM
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnees
        vm_prev <- .subset2(immobilier@ptf, which(name_ptf == "valeur_marche"))

        # Calcul des nouvelles VM
        warning("Revalorisation de l'immobilier n'est pas branchee !")
        vm_new  <- vm_prev

        # Mise a jour de l'attribut
        immobilier@ptf$valeur_marche <- vm_new



        ## ###########################
        ##       Calcul des PMVL
        ## ###########################

        # Calcul des PMVL
        pmvl <- vm_new - vm_prev




        # Output
        return(list(immobilier = immobilier,
                    pmvl = pmvl))
    }
)
