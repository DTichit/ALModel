##' Fonction \code{reprise_ppe}.
##'
##' Cette fonction permet de reprendre un montant sur la PPE. Le montant est prioritairement repris sur les plus vieilles dotations.
##'
##' @name reprise_ppe
##' @docType methods
##' @param ppe est un objet de type \code{\link{PPE}}.
##' @param montant est un \code{numeric} representant le montant a reprendre.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PPE-class.R
##'
setGeneric(name = "reprise_ppe", def = function(ppe, montant) {standardGeneric("reprise_ppe")})
setMethod(
    f = "reprise_ppe",
    signature = c(ppe = "PPE", montant = "numeric"),
    definition = function(ppe, montant){


        # Test sur le signe du montant
        if(montant < 0)
            warning("PPE : Dotation d'un montant negatif.")


        ## ###########################
        ##   Montant total de la PPE
        ## ###########################
        sum_ppe <- sum(ppe@ppe)




        ## ###########################
        ##   Gestion de la reprise
        ## ###########################

        if(sum_ppe > montant) {

            ## #######
            # Si montant a reprendre est inferieur a la ppe totale
            ## #######

            # Initialisation de parametres
            reste   <- montant
            ind     <- 8L

            # Mise a jour de l'attribut : reprise decroissante
            while(reste > 0 & ind != 0L) {

                # Si en l'annee ind, la PPB > reste :
                if (ppe@ppe[ind] < reste) {
                    reste <- reste - ppe@ppe[ind]
                    ppe@ppe[ind] <- 0

                } else { # Si en l'annee i, la PPB < reste :
                    ppe@ppe[ind] <- ppe@ppe[ind] - reste
                    reste <- 0
                }

                # Annee precedente
                ind <- ind - 1L
            }


            # Reprise complete du montant demande
            reprise <- montant


        } else {

            ## #######
            # Si le montant a reprendre est superieur a la ppe totale
            ## #######

            # Mise a jour de l'objet PPE
            ppe@ppe <- rep(x = 0, length = 8L)

            # Reprise partielle du montant demande
            reprise <- sum_ppe

        }


        # Output
        return(list(ppe = ppe,
                    reprise = reprise))
    }
)
