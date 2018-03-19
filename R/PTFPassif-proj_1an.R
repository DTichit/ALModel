##' Fonction \code{proj_1an_ptf_passif}
##'
##' Cette fonction permet de projeter horizon 1 an le portfeuille passif : gestion des differents passifs
##'
##' @name proj_1an_ptf_passif
##' @docType methods
##' @param ptf_passif est un objet de type \code{\link{PTFPassif}}.
##' @param hyp_passif est un objet de type \code{\link{HypPassif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PTFPassif-class.R HypPassif-class.R
##'
setGeneric(name = "proj_1an_ptf_passif", def = function(ptf_passif, hyp_passif) {standardGeneric("proj_1an_ptf_passif")})
setMethod(
    f = "proj_1an_ptf_passif",
    signature = c(ptf_passif = "PTFPassif", hyp_passif = "HypPassif"),
    definition = function(ptf_passif, hyp_passif){



        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion des differents passifs
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Epargne
        ## ###########################

        # Projection sur une annee des contrats epargne
        res_proj_epargne <- proj_1an_epargne(epargne = ptf_passif@epargne, hyp_passif = hyp_passif)

        # Mise a jour de l'attribut
        ptf_passif@epargne <- res_proj_epargne[["epargne"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Prestations
        prestation <- list(epargne = res_proj_epargne[["flux"]][["prestation"]])

        # Revalorisation aux TMG
        revalorisation_tmg <- list(epargne = res_proj_epargne[["flux"]][["revalorisation_tmg"]])

        # Chargements
        chargement <- list(epargne = res_proj_epargne[["flux"]][["chargement"]])

        # Frais
        frais <- list(epargne = res_proj_epargne[["flux"]][["frais"]])


        # Output
        return(list(ptf_passif = ptf_passif,
                    flux = list(prestation = prestation,
                                revalorisation_tmg = revalorisation_tmg,
                                chargement = chargement,
                                frais = frais)))
    }
)
