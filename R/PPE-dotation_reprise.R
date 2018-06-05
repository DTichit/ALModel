##' Fonction \code{dotation_ppe}.
##'
##' Cette fonction permet de reprendre ou doter un montant a la PPE.
##' \describe{
##' \item{dotation}{ : Appel de la fonction \code{\link{dotation_ppe}} ;}
##' \item{reprise}{ : Appel de la fonction \code{\link{reprise_ppe}}.}
##' }
##' @name dotation_reprise_ppe
##' @docType methods
##' @param ppe est un objet de type \code{\link{PPE}}.
##' @param montant est un \code{numeric} representant le montant a doter ou a reprendre.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PPE-class.R PPE-dotation.R PPE-reprise.R
##'
setGeneric(name = "dotation_reprise_ppe", def = function(ppe, montant) {standardGeneric("dotation_reprise_ppe")})
setMethod(
    f = "dotation_reprise_ppe",
    signature = c(ppe = "PPE", montant = "numeric"),
    definition = function(ppe, montant){


        # Test en fonction du signe du montant
        if(montant < 0)
            res <- reprise_ppe(ppe, -montant)
        else
            res <- dotation_ppe(ppe, montant)


        # Mise a jour de l'attribut
        ppe <- res[["ppe"]]


        # Output
        return(list(ppe = ppe,
                    flux = res[["flux"]]))
    }
)
