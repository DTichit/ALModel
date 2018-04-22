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


        # Mise a jour de la tresorerie
        system@actif@ptf_actif@tresorerie@solde <- system@actif@ptf_actif@tresorerie@solde + proj_actif[["mouvement"]][["treso"]]

        # Mise a jour du resultat
        resultat <- proj_actif[["mouvement"]][["resultat"]]





        ## ######################################################
        ## ######################################################
        ##
        ##                Gestion des passifs :
        ##   Evaluation des prestations, des frais et des chgts
        ##
        ## ######################################################
        ## ######################################################

        # Projection sur une annee des passifs
        proj_passif <- proj_1an_passif(passif = system@passif, an = an)

        # Mise a jour de l'attribut
        system@passif <- proj_passif[["passif"]]


        # Mise a jour de la tresorerie
        system@actif@ptf_actif@tresorerie@solde <- system@actif@ptf_actif@tresorerie@solde + proj_passif[["mouvement"]][["treso"]]

        # Mise a jour du resultat
        resultat <- resultat + proj_passif[["mouvement"]][["resultat"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Re-allocation des actifs
        ##
        ## ######################################################
        ## ######################################################

        # Projection sur une annee des passifs
        res_realloc <- rebalancement_actif(actif = system@actif)

        # Mise a jour de l'attribut
        system@actif <- res_realloc[["actif"]]


        # Credit et debit
        credit <- sum_list(res_realloc$pmvr, 1L)
        debit  <- 0

        # Mise a jour de la tresorerie
        system@actif@ptf_actif@tresorerie@solde <- system@actif@ptf_actif@tresorerie@solde + credit - debit

        # Mise a jour du resultat
        resultat <- resultat + credit - debit






        ## ######################################################
        ## ######################################################
        ##
        ##          Gestion de la Reserve de Capitalisation
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des PMV realisees
        pmvr <- res_realloc[["pmvr"]]

        # Appel de la fonction
        res_reserve_capi <- dotation_reserve_capi(system@passif@provision@reserve_capi, pmvr = pmvr[["obligation"]])

        # Mise a jour de la provision
        system@passif@provision@reserve_capi <- res_reserve_capi[["reserve_capi"]]

        # Mise a jour de la PMVR obligataire apres dotation de la RC
        pmvr[["obligation"]] <- res_reserve_capi[["reste_pmv"]]


        # Credit et debit
        credit <- 0
        debit  <- res_reserve_capi[["flux"]]

        # Mise a jour de la tresorerie
        system@actif@ptf_actif@tresorerie@solde <- system@actif@ptf_actif@tresorerie@solde + credit - debit

        # Mise a jour du resultat
        resultat <- resultat + credit - debit





        ## ######################################################
        ## ######################################################
        ##
        ##      Calcul des resultats techniques et financiers
        ##
        ## ######################################################
        ## ######################################################
        warning("Resultats a reprendre : ils sont probablement faux !!  (Frais financiers ?)")

        # Mise en forme des donnees
        result_fin <- list(pmvr = pmvr,
                           vente = proj_actif[["flux"]][["vente"]],
                           produits = proj_actif[["flux"]][["prod_fin"]],
                           frais = proj_actif[["flux"]][["frais"]])
        result_tech <- list(chargement = proj_passif[["flux"]][["chargement"]],
                            frais = proj_passif[["flux"]][["frais"]])

        # Calcul des resultats
        result_tech <- calcul_resultat_tech(result_tech)
        result_fin  <- calcul_resultat_fin(result_fin)





        ## ######################################################
        ## ######################################################
        ##
        ##  Determination du resultat des actifs en face les FP
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        res_fin_fp <- resultat_fin_fp(passif = system@passif, result_fin = result_fin)[["result_fin_fp"]]






        ## ######################################################
        ## ######################################################
        ##
        ##      Calcul de la PB a distribuer et dotation de la PPE
        ##
        ## ######################################################
        ## ######################################################

        # Calcul de la PB a distribuer
        res_pb <- calcul_pb(taux_pb = system@taux_pb, resultat_fin = (result_fin - res_fin_fp), resultat_tech = result_tech)

        # PB a attribuer
        pb <- sum_list(res_pb[["pb"]], 1L)

        # Dotation du montant de PB sur la PPE
        res_dotation <- dotation_ppe(ppe = system@passif@provision@ppe, montant = pb)
        system@passif@provision@ppe <- res_dotation[["ppe"]]

        # Flux sur la PPE
        flux_ppe <- res_dotation[["dotation"]]


        # Credit et debit
        credit <- 0
        debit  <- res_dotation[["dotation"]]

        # Mise a jour de la tresorerie
        system@actif@ptf_actif@tresorerie@solde <- system@actif@ptf_actif@tresorerie@solde + credit - debit

        # Mise a jour du resultat
        resultat <- resultat + credit - debit






        ## ######################################################
        ## ######################################################
        ##
        ##  Revalorisation du passif : Distribution de la PB
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        res_revalo <- revalo_passif(passif = system@passif, revalo_prestation = proj_passif[["besoin"]][["revalo_prest"]], pb = 0, an = an)

        # Mise a jour de l'objet
        system@passif <- res_revalo[["passif"]]

        # Flux sur la PPE
        flux_ppe <- flux_ppe + res_revalo[["flux_ppe"]]


        # Credit et debit
        credit <- 0
        debit  <- sum_list(res_revalo$revalorisation, 2L) + res_revalo[["flux_ppe"]]

        # Mise a jour de la tresorerie
        system@actif@ptf_actif@tresorerie@solde <- system@actif@ptf_actif@tresorerie@solde + credit - debit

        # Mise a jour du resultat
        resultat <- resultat + credit - debit





        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion des fonds propres
        ##
        ## ######################################################
        ## ######################################################

        # Calcul du resultat

        # Appel de la fonction
        res_gest_fp <- gestion_fonds_propres(fp = system@passif@fonds_propres, resultat = resultat + res_fin_fp)

        # Mise a jour de l'attribut
        system@passif@fonds_propres <- res_gest_fp[["fp"]]





        ## ######################################################
        ## ######################################################
        ##
        ##                  Vieillissement
        ##
        ## ######################################################
        ## ######################################################

        # Vieillissement de la PPE
        system@passif@provision@ppe <- vieillissement_ppe(system@passif@provision@ppe)






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

        # Chargements par produit
        charg_prod <- sapply(X = name_passif,
                             FUN = function(x) return(do.call(sum, proj_passif[["flux"]][["chargement"]][[x]])),
                             simplify = FALSE, USE.NAMES = TRUE)

        # Primes par produit
        prime_prod <- proj_passif[["flux"]][["prime"]]

        # Frais financiers
        frais_fin <- sum_list(proj_actif[["flux"]][["frais"]], 2L)



        ## ###########################
        ##      Flux calcul BEL
        ## ###########################

        # Somme des flux necessaires au calcul du BEL
        # flux_bel <- sapply(X = name_passif,
        #                    FUN = function(x) return(frais_prod[[x]] + prestation_prod[[x]] - prime_prod[[x]] - charg_prod[[x]]),
        #                    simplify = FALSE, USE.NAMES = TRUE)
        flux_bel <- (sum_list(prestation_prod, 1L) + sum_list(frais_prod, 1L) + frais_fin) -
            (sum_list(prime_prod, 1L) + sum_list(charg_prod, 1L))


        ## ###########################
        ## Aggregation de l'ensemble des donnees a stocker
        ## ###########################

        # Aggregation des flux : Actif, Passif
        stock <- list(actif = list(image = system@actif@ptf_actif,
                                   flux = list(prod_fin = proj_actif[["flux"]]$prod_fin,
                                               pmvr = res_realloc[["pmvr"]],
                                               frais = proj_actif[["flux"]]$frais),
                                   resultat_fin_fp = res_fin_fp),
                      passif = list(image = list(epargne = system@passif@ptf_passif@epargne@ptf),
                                    pm_ouverture = proj_passif[["pm_ouverture"]],
                                    flux = proj_passif[["flux"]]),
                      pb = list(pb = pb,
                                revalorisation = list(attribuee = res_revalo[["revalorisation"]],
                                                      besoin = res_revalo[["besoin"]])),
                      fonds_propres = system@passif@fonds_propres,
                      provision = list(image = system@passif@provision,
                                       flux = list(reserve_capi = res_reserve_capi[["flux"]],
                                                   ppe = flux_ppe)))




        # Output
        return(list(system = system,
                    flux_bel = flux_bel,
                    stock = stock))

    }
)
