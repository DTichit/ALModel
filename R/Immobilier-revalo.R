##' Fonction \code{revalo_immobilier}
##'
##' Cette fonction permet de revaloriser le portefeuille immobilier.
##'
##' @name revalo_immobilier
##' @docType methods
##' @param immobilier est un objet de type \code{\link{Immobilier}}.
##' @param log_rdt est un \code{numeric} representant le log-rendement du PTF.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R
##'
setGeneric(name = "revalo_immobilier", def = function(immobilier, log_rdt) {standardGeneric("revalo_immobilier")})
setMethod(
    f = "revalo_immobilier",
    signature = c(immobilier = "Immobilier", log_rdt = "numeric"),
    definition = function(immobilier, log_rdt){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(immobilier@ptf)
        vm_ptf <- .subset2(immobilier@ptf, which(name_ptf == "valeur_marche"))
        vc_ptf <- .subset2(immobilier@ptf, which(name_ptf == "valeur_comptable"))



        ## ######################################################
        ## ######################################################
        ##
        ##            Mise a jour des VM
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnees

        # Calcul des nouvelles VM
        vm_new  <- vm_ptf * exp(log_rdt)

        # Mise a jour de l'attribut
        immobilier@ptf$valeur_marche <- vm_new



        ## ###########################
        ##       Calcul des PMVL
        ## ###########################

        # Calcul des PMVL
        pmvl <- vm_new - vc_ptf




        # Output
        return(list(immobilier = immobilier,
                    pmvl = pmvl))
    }
)
