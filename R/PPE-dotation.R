##' Fonction \code{dotation_ppe}.
##'
##' Cette fonction permet de doter la PPE. Le montant est dote sur la 1ere annee de reserve.
##'
##' @name dotation_ppe
##' @docType methods
##' @param ppe est un objet de type \code{\link{PPE}}.
##' @param montant est un \code{numeric} representant le montant a doter.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PPE-class.R
##'
setGeneric(name = "dotation_ppe", def = function(ppe, montant) {standardGeneric("dotation_ppe")})
setMethod(
    f = "dotation_ppe",
    signature = c(ppe = "PPE", montant = "numeric"),
    definition = function(ppe, montant){

        # Test sur le signe du montant
        if(montant < 0)
            warning("PPE : Dotation d'un montant negatif.")

        # Application de la dotation
        ppe@ppe[1L] <- ppe@ppe[1L] + montant

        # Output
        return(list(ppe = ppe,
                    flux = montant))
    }
)
