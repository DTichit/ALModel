##' Fonction \code{proj_1an_system}.
##'
##' Cette fonction permet de projeter a horizon 1 an un objet \code{\link{System}}.
##'
##' @name proj_1an_system
##' @docType methods
##' @param system est un objet de type \code{System}.
##' @param an est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @seealso Projection des passifs : \code{\link{proj_1an_passif}}
##' @seealso Projection des actifs : \code{\link{proj_1an_actif}}
##' @export
##' @include System-class.R Passif-proj_1an.R Actif-proj_1an.R
##'
setGeneric(name = "proj_1an_system", def = function(system, an) {standardGeneric("proj_1an_system")})
setMethod(
    f = "proj_1an_system",
    signature = c(system = "System", an = "integer"),
    definition = function(system, an){



        ## ######################################################
        ## ######################################################
        ##
        ##                  Gestion des actifs :
        ## Revalorisation, calcul des prod fin et vieillissement
        ##
        ## ######################################################
        ## ######################################################

        # Revalorisation du PTF actif
        temp <- proj_1an_actif(actif = system@actif)

        # Mise a jour de l'attribut
        actif <- temp[["actif"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion des passifs :
        ## Evaluation des prestations, revalo des contrats aux TMG
        ##
        ## ######################################################
        ## ######################################################

        # Projection sur une annee des passifs
        temp <- proj_1an_passif(passif = system@passif)

        # Mise a jour de l'attribut
        system@passif <- temp[["passif"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Flux des donnees necessaires au calcul du BEL
        flux <- list()




        # Output
        return(list(system = system))

    }
)
