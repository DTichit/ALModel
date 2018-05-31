##' Fonction \code{calcul_resultat_tech}.
##'
##' Cette fonction permet de calculer le resultat technique d'une compagnie d'assurance.
##'
##' @name calcul_resultat_tech
##' @docType methods
##' @param resultat_tech est une \code{list} contenant le resultat technique : chargements
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include
##'
setGeneric(name = "calcul_resultat_tech", def = function(resultat_tech) {standardGeneric("calcul_resultat_tech")})
setMethod(
    f = "calcul_resultat_tech",
    signature = c(resultat_tech = "list"),
    definition = function(resultat_tech) {


        ## ###########################
        ##     Resultat technique
        ## ###########################

        # Extraction des frais
        frais_prod <- sum_list(resultat_tech[["frais"]], 2L)

        # Extractions des chargements
        chargements <- sum_list(resultat_tech[["chargement"]], 2L)


        # Resultat technique total
        resultat_tech <- chargements - frais_prod



        # Output
        return(resultat_tech)

    }
)
