##' Fonction \code{revalo_ptf_passif}
##'
##' Cette fonction permet de revaloriser les differents PTF passifs d'une compagnie d'assurance :
##'
##' @name revalo_ptf_passif
##' @docType methods
##' @param ptf_passif est un objet de type \code{\link{PTFPassif}}.
##' @param revalo_cible est une \code{list} contenant les montants de revalorisation cibles a distibuer par produit.
##' @param revalo_supp est une \code{list} contenant les montants de revalorisation supplementaire a distibuer par produit.
##' @param cible est une \code{list} contenant des elements relatifs a la politique de revalorisation pour les differents passifs.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PTFPassif-class.R
##'
setGeneric(name = "revalo_ptf_passif", def = function(ptf_passif, revalo_cible, revalo_supp, cible) {standardGeneric("revalo_ptf_passif")})
setMethod(
    f = "revalo_ptf_passif",
    signature = c(ptf_passif = "PTFPassif", revalo_cible = "list", revalo_supp = "list", cible = "list"),
    definition = function(ptf_passif, revalo_cible, revalo_supp, cible){




        ## ######################################################
        ## ######################################################
        ##
        ##         Revalorisation des differents PTF
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Epargne
        ## ###########################

        # Appel de la fonction
        res_epargne <- revalo_epargne(epargne = ptf_passif@epargne, revalo_cible = revalo_cible[["epargne"]],
                                      revalo_supp = revalo_supp[["epargne"]], cible = cible[["epargne"]])

        # Mise a jour de l'objet
        ptf_passif@epargne <- res_epargne[["epargne"]]








        ## ######################################################
        ## ######################################################
        ##
        ##              Agregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##   Chargements appliques
        ## ###########################

        chgt <- list(epargne = res_epargne[["chargement"]])




        # Output
        return(list(ptf_passif = ptf_passif,
                    chargements_appliques = chgt))
    }
)
