##' Fonction \code{calcul_resultat_fin}.
##'
##' Cette fonction permet de calculer le resultat financier d'une compagnie d'assurance.
##'
##' @name calcul_resultat_fin
##' @docType methods
##' @param resultat_fin est une \code{list} contenant les resultats financiers : PMVR, produits financiers et frais.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include
##'
setGeneric(name = "calcul_resultat_fin", def = function(resultat_fin) {standardGeneric("calcul_resultat_fin")})
setMethod(
    f = "calcul_resultat_fin",
    signature = c(resultat_fin = "list"),
    definition = function(resultat_fin) {


        ## ###########################
        ##     Resultat financier
        ## ###########################

        # Extraction des PMVR
        pmvr  <- do.call(sum, resultat_fin[["pmvr"]])

        # Extraction des produits financiers
        prod_fin <- do.call(sum, resultat_fin[["produits"]])

        # Extraction des frais
        frais <- sum_list(resultat_fin[["frais"]], 2L)

        # Variation des VNC
        var_vnc <- sum_list(resultat_fin[["var_vnc"]], 1L)

        # Vente des PVL
        if(!is.null(resultat_fin[["vente_pvl"]][["action"]]))
            vente_pvl <- sum_list(resultat_fin[["vente_pvl"]], 1L)

        # Resultat financier total
        resultat_fin <- pmvr + prod_fin + var_vnc - frais + if.is_null(get0("vente_pvl"), 0)



        # Output
        return(resultat_fin)

    }
)
