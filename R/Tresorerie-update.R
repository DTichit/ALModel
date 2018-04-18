##' Fonction \code{update_tresorerie}.
##'
##' Cette fonction permet de mettre a jour la tresorerie : credit et debit.
##'
##' @name update_tresorerie
##' @docType methods
##' @param tresorerie est un objet de type \code{\link{Tresorerie}}
##' @param montant est un \code{numeric}
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Tresorerie-class.R
##'
setGeneric(name = "update_tresorerie", def = function(tresorerie, montant) {standardGeneric("update_tresorerie")})
setMethod(
    f = "update_tresorerie",
    signature = c(tresorerie = "Tresorerie", montant = "numeric"),
    definition = function(tresorerie, montant){


        # Calcul du reste a attribue
        reste <- -min(0, tresorerie@solde + montant)

        # Mise a jour de la tresorerie
        tresorerie@solde <- max(0, tresorerie@solde + montant)


        # Output
        return(list(tresorerie = tresorerie,
                    reste = reste))
    }
)
