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
        proj_actif <- proj_1an_actif(actif = system@actif)

        # Mise a jour de l'attribut
        actif <- proj_actif[["actif"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion des passifs :
        ## Evaluation des prestations, revalo des contrats aux TMG
        ##
        ## ######################################################
        ## ######################################################

        # Projection sur une annee des passifs
        proj_passif <- proj_1an_passif(passif = system@passif)

        # Mise a jour de l'attribut
        system@passif <- proj_passif[["passif"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Determination de la PB
        ##
        ## ######################################################
        ## ######################################################

        # Mise en forme des donnees
        result_fin <- list(pmvl = proj_actif[["pmvl"]],
                           prod_fin = proj_actif[["flux"]][["prod_fin"]])
        result_tech <- list(chargement = proj_passif[["flux"]][["chargement"]])

        # Calcul de la PB a distribuer
        res_pb <- calcul_pb(taux_pb = system@taux_pb, resultat_fin = result_fin, resultat_tech = result_tech)

        # Ajout du reste de resultat a la tresorerie
        reste <- do.call(sum, res_pb[["reste"]])
        system@actif@ptf_actif@tresorerie@ptf <- system@actif@ptf_actif@tresorerie@ptf + reste





        ## ######################################################
        ## ######################################################
        ##
        ##              Revalorisation du passif
        ##
        ## ######################################################
        ## ######################################################

        # PB a attribuer
        pb <- do.call(sum, res_pb[["pb"]])

        # Appel de la fonction
        res_revalo <- revalo_passif(passif = system@passif, pb = pb)





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
