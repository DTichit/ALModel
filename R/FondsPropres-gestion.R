##' Fonction \code{gestion_fonds_propres}
##'
##' Cette fonction permet de gerer les fonds propres.
##'
##' @name gestion_fonds_propres
##' @docType methods
##' @param fp est un objet de type \code{FondsPropres}.
##' @param resultat est un \code{numeric} representant le resultat de l'annee.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "gestion_fonds_propres", def = function(fp, resultat) {standardGeneric("gestion_fonds_propres")})
setMethod(
    f = "gestion_fonds_propres",
    signature = c(fp = "FondsPropres", resultat = "numeric"),
    definition = function(fp, resultat){


        ## ###########################
        ##   Mise a jour du resultat
        ## ###########################

        # Mise a jour de l'attribut
        fp@resultat_exercice <- resultat




        ## ###########################
        ##   Mise a jour du report a nouveau
        ## ###########################

        # Mise a jour de l'attribut
        fp@report_a_nouveau <- fp@report_a_nouveau + resultat





        # Output
        return(list(fp = fp))


    }
)
