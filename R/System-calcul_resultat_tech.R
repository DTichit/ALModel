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

        # Classes des differents produits modelises
        name_prod <- names(resultat_tech[["chargement"]])

        # Extraction des chargements
        chgt_prod <- lapply(X = name_prod,
                            FUN = function(x) do.call(sum, resultat_tech[["chargement"]][[x]]))

        # Extraction des frais
        frais_prod <- lapply(X = name_prod,
                            FUN = function(x) do.call(sum, resultat_tech[["frais"]][[x]]))

        # Resultat technique total
        resultat_tech <- do.call(sum, chgt_prod) - do.call(sum, frais_prod)



        # Output
        return(resultat_tech)

    }
)
