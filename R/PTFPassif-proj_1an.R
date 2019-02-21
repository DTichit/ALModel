##' Fonction \code{proj_1an_ptf_passif}
##'
##' Cette fonction permet de projeter horizon 1 an le portfeuille passif : gestion des differents passifs
##'
##' @name proj_1an_ptf_passif
##' @docType methods
##' @param ptf_passif est un objet de type \code{\link{PTFPassif}}.
##' @param hyp_passif est un objet de type \code{\link{HypPassif}}.
##' @param an est un objet de type \code{integer}.
##' @param agreg_out est une valeur \code{logical} qui indique si les sorties doivent etre agregees. Par defaut, sa valeur est a TRUE.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PTFPassif-class.R HypPassif-class.R
##'
setGeneric(name = "proj_1an_ptf_passif", def = function(ptf_passif, hyp_passif, an, agreg_out = TRUE) {standardGeneric("proj_1an_ptf_passif")})
setMethod(
    f = "proj_1an_ptf_passif",
    signature = c(ptf_passif = "PTFPassif", hyp_passif = "HypPassif", an = "integer"),
    definition = function(ptf_passif, hyp_passif, an, agreg_out){



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
        res_proj_epargne <- proj_1an_epargne(epargne = ptf_passif@epargne, hyp_passif = hyp_passif, an = an, agreg_out = agreg_out)

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

        # Prime
        prime <- list(epargne = res_proj_epargne[["flux"]][["prime"]])

        # Chargements
        chargement <- list(administration = list(epargne = res_proj_epargne[["flux"]][["chargement"]][["administration"]]),
                           acquisition = list(epargne = res_proj_epargne[["flux"]][["chargement"]][["acquisition"]]))

        # Frais
        frais <- list(epargne = res_proj_epargne[["flux"]][["frais"]])

        # Besoins pour la revalorisation des prestations
        revalo_prest <- list(epargne = res_proj_epargne[["besoin"]][["revalo_prestation"]])


        # Output
        return(list(ptf_passif = ptf_passif,
                    flux = list(prestation = prestation,
                                prime = prime,
                                chargement = chargement,
                                frais = frais),
                    besoin = list(revalo_prest = revalo_prest)))
    }
)
