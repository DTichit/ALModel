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
        proj_actif <- proj_1an_actif(actif = system@actif, an = an)

        # Mise a jour de l'attribut
        system@actif <- proj_actif[["actif"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion des passifs :
        ## Evaluation des prestations, revalo des contrats aux TMG
        ##
        ## ######################################################
        ## ######################################################

        # Projection sur une annee des passifs
        proj_passif <- proj_1an_passif(passif = system@passif, an = an)

        # Mise a jour de l'attribut
        system@passif <- proj_passif[["passif"]]





        ## ######################################################
        ## ######################################################
        ##
        ##          Gestion de la Reserve de Capitalisation
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des PMV realisees
        pmvr <- proj_actif[["flux"]][["pmvr"]]

        # Appel de la fonction
        res_reserve_capi <- dotation_reserve_capi(system@passif@provision@reserve_capi, pmvr = pmvr[["obligation"]])

        # Mise a jour de la provision
        system@passif@provision@reserve_capi <- res_reserve_capi[["reserve_capi"]]

        # Mise a jour des PMVR
        pmvr[["obligation"]] <- res_reserve_capi[["reste_pmv"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Determination de la PB
        ##
        ## ######################################################
        ## ######################################################

        # Mise en forme des donnees
        result_fin <- list(pmvr = pmvr,
                           prod_fin = proj_actif[["flux"]][["prod_fin"]])
        result_tech <- list(chargement = proj_passif[["flux"]][["chargement"]])
        warning("Resultats a reprendre : ils sont faux !!")

        # Calcul de la PB a distribuer
        res_pb <- calcul_pb(taux_pb = system@taux_pb, resultat_fin = result_fin, resultat_tech = result_tech)

        # Ajout du reste de resultat a la tresorerie
        reste <- do.call(sum, res_pb[["reste"]])
        system@actif@ptf_actif@tresorerie@ptf$solde <- system@actif@ptf_actif@tresorerie@ptf$solde + reste





        ## ######################################################
        ## ######################################################
        ##
        ##  Revalorisation du passif : Distribution de la PB
        ##
        ## ######################################################
        ## ######################################################

        # PB a attribuer
        pb <- do.call(sum, res_pb[["pb"]])

        # Revalorisation deja effectuee
        revalo_prestation <- list(epargne = proj_passif[["besoin"]][["revalo_regl"]][["epargne"]][["prestation"]])

        # Appel de la fonction
        res_revalo <- revalo_passif(passif = system@passif, revalo_prestation = revalo_prestation, pb = pb, an = an)

        # Mise a jour de l'objet
        system@passif <- res_revalo[["passif"]]

        # Mise a jour de la tresorie a la suite des revalorisations contractuelles
        system@actif@ptf_actif@tresorerie@ptf$solde <- system@actif@ptf_actif@tresorerie@ptf$solde - res_revalo$reste_contr






        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Differentes classes de passif modelisees
        name_passif <- names(proj_passif[["flux"]][["prestation"]])


        ## ###########################
        ##   Aggregation des flux
        ## ###########################

        # Prestation par produit
        prestation_prod <- sapply(X = name_passif,
                                  FUN = function(x) do.call(sum, proj_passif[["flux"]][["prestation"]][[x]]),
                                  simplify = FALSE, USE.NAMES = TRUE)

        # Frais par produit
        frais_prod <- sapply(X = name_passif,
                             FUN = function(x) return(do.call(sum, proj_passif[["flux"]][["frais"]][[x]])),
                             simplify = FALSE, USE.NAMES = TRUE)

        # Primes par produit
        prime_prod <- proj_passif[["flux"]][["prime"]]



        ## ###########################
        ##      Flux calcul BEL
        ## ###########################

        # Somme des flux necessaires au calcul du BEL
        flux_bel <- sapply(X = name_passif,
                           FUN = function(x) return(frais_prod[[x]] + prestation_prod[[x]] - prime_prod[[x]]),
                           simplify = FALSE, USE.NAMES = TRUE)


        ## ###########################
        ## Aggregation de l'ensemble des donnees a stocker
        ## ###########################

        # Aggregation des flux : Actif, Passif
        warning("Inserer : Revalorisation")
        stock <- list(flux = list(actif = proj_actif[["flux"]],
                                  passif = proj_passif[["flux"]]),
                      actif = system@actif@ptf_actif,
                      passif = list(epargne = system@passif@ptf_passif@epargne@ptf),
                      revalorisation = res_revalo[["revalorisation"]],
                      provision = system@passif@provision)




        # Output
        return(list(system = system,
                    flux_bel = flux_bel,
                    stock = stock))

    }
)
