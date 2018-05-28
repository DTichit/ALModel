##' Fonction \code{gestion_fin_projection_system}
##'
##' Cette fonction permet de gerer la fin de projection de la compagnie d'assurance.
##'
##' @name gestion_fin_projection_system
##' @docType methods
##' @param system est un objet de type \code{\link{System}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include System-class.R
##'
setGeneric(name = "gestion_fin_projection_system", def = function(system) {standardGeneric("gestion_fin_projection_system")})
setMethod(
    f = "gestion_fin_projection_system",
    signature = c(system = "System"),
    definition = function(system){



        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion de la partie actif
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        res_actif <- gestion_fin_projection_actif(actif = system@actif)





        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion de la partie passif
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        res_passif <- gestion_fin_projection_passif(passif = system@passif)

        # Mise a jour de l'attribut
        system@passif <- res_passif[["passif"]]






        ## ######################################################
        ## ######################################################
        ##
        ##              Agregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Montant a verser en fin de projection
        fin_projection_assureurs    <- res_passif[["fin_projection"]][["assureurs"]] + sum_list(res_actif[["pmvr"]], 1L)
        fin_projection_assures      <- res_passif[["fin_projection"]][["assures"]]





        # Output
        return(list(system = system,
                    fin_projection = list(assures = fin_projection_assures,
                                          assureurs = fin_projection_assureurs)))
    }
)
