##' Fonction \code{calcul_pb}.
##'
##' Cette fonction permet de determiner la PB a distribuer.
##'
##' @name calcul_pb
##' @docType methods
##' @param taux_pb est une \code{list} contenant les deux taux de pb contractuels.
##' @param resultat_fin est une \code{list} contenant les resultats financiers : PMVR, produits financiers.
##' @param resultat_tech est une \code{list} contenant le resultat technique : chargements
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include
##'
setGeneric(name = "calcul_pb", def = function(taux_pb, resultat_fin, resultat_tech) {standardGeneric("calcul_pb")})
setMethod(
    f = "calcul_pb",
    signature = c(taux_pb = "list", resultat_fin = "list", resultat_tech = "list"),
    definition = function(taux_pb, resultat_fin, resultat_tech) {


        ## ###########################
        ##     Resultat financier
        ## ###########################

        # Extraction des PMVR
        pmvr  <- do.call(sum, resultat_fin[["pmvr"]])

        # Extraction des produits financiers
        prod_fin <- do.call(sum, resultat_fin[["prod_fin"]])

        # Resultat financier total
        resultat_fin <- pmvr + prod_fin



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



        ## ###########################
        ##     PB a distribuer
        ## ###########################

        # PB financiere
        pb_fin <- taux_pb[["financier"]] * resultat_fin

        # PB technique
        if(resultat_tech < 0)
            pb_tech <- resultat_tech
        else
            pb_tech <- taux_pb[["technique"]] * resultat_tech



        ## ###########################
        ##  Resultats non distribues
        ## ###########################

        # Difference entre le resultat et la PB
        reste_fin <- resultat_fin - pb_fin
        reste_tech <- resultat_tech - pb_tech





        # Output
        return(list(pb = list(financier = pb_fin,
                              technique = pb_tech),
                    reste = list(financier = reste_fin,
                                 technique = reste_tech)))

    }
)
