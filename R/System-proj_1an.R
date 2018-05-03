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





        ## ######################################################
        ## ######################################################
        ##
        ##          Calcul de la PB a distribuer
        ##
        ## ######################################################
        ## ######################################################

        # Mise en forme des donnees
        result_fin <- list(produits = proj_actif[["flux"]][["prod_fin"]],
                           pmvr = pmvr,
                           var_vnc = proj_actif[["flux"]][["var_vnc"]],
                           frais = proj_actif[["flux"]][["frais"]])
        result_tech <- list(frais = proj_passif[["flux"]][["frais"]],
                            chargement = proj_passif[["flux"]][["chargement"]])


        # Calcul des resultats
        # result_tech <- calcul_resultat_tech(result_tech)
        result_tech <- 0
        result_fin  <- calcul_resultat_fin(result_fin)





        ## ######################################################
        ## ######################################################
        ##
        ##  Determination de differents resultats financiers
        ##
        ## ######################################################
        ## ######################################################

        # Resultat financier en face des fonds propres
        quote_part_fp <- calcul_quote_part_fp(passif = system@passif)

        # Resultat financier en face des fonds propres
        res_fin_fp <- result_fin * quote_part_fp

        # Resultat financier lie a la PM
        res_fin_pm <- result_fin  * (1 - quote_part_fp)





        ## ######################################################
        ## ######################################################
        ##
        ##  Revalorisation du passif : Distribution de la PB
        ##
        ## ######################################################
        ## ######################################################

        # Visualisation des PV latentes
        pvl <- extract_pmvl_ptf_actif(system@actif@ptf_actif)[["pvl"]]

        # Appel de la fonction
        res_revalo <- revalo_passif(passif = system@passif, resultat = res_fin_pm + result_tech, pvl = pvl,
                                    revalo_prestation = proj_passif[["besoin"]][["revalo_prest"]], an = an)

        # Mise a jour de l'objet
        system@passif <- res_revalo[["passif"]]




        ## ######################################################
        ## ######################################################
        ##
        ##          Realisation de PVL, le cas echeant
        ##
        ## ######################################################
        ## ######################################################

        # Montant de PVL devant etre realisees
        pvl_a_realiser <- res_revalo[["pvl_a_realiser"]]

        if(pvl_a_realiser > 0) {

            # Appel de la fonction
            res_real_pvl <- realisation_pvl_ptf_actif(ptf_actif = system@actif@ptf_actif, montant = res_revalo[["pvl_a_realiser"]])

            # Mise a jour de l'attribut
            system@actif@ptf_actif <- res_real_pvl[["ptf_actif"]]

            # Somme des PVL realises
            pvl_realisees <- sum_list(res_real_pvl[["pvr"]], 1L)
        }





        ## ######################################################
        ## ######################################################
        ##
        ##           Calcul du resultat de l'exercice
        ##
        ## ######################################################
        ## ######################################################

        # Creation de la liste contenant tous les elements
        resultat <- list(charges_pm = sum_list(proj_passif[["pm_ouverture"]], 1L) - sum_list(res_revalo[["pm_cloture"]],  1L),
                         prime = sum_list(proj_passif$flux$prime, 1L),
                         prestation = sum_list(proj_passif$flux$prestation, 2L),
                         revalo_pm = sum_list(res_revalo$revalorisation$tmg, 1L) + sum_list(res_revalo$revalorisation$pb, 2L),
                         revalo_prest = sum_list(proj_passif[["besoin"]][["revalo_prest"]], 1L),
                         frais = sum_list(proj_passif$flux$frais, 2L),
                         chgt = sum_list(proj_passif$flux$chargement, 2L),
                         resultat_fin = result_fin - res_fin_fp + res_realloc[["pmvr"]]$obligation + if.is_null(get0("pvl_realisees"), 0L),
                         charges_rc = res_reserve_capi[["flux"]],
                         charges_ppe = res_revalo[["flux_ppe"]],
                         res_fin_fp = res_fin_fp)

        # Calcul du resultat de l'exercice
        res_resultat <- calcul_resultat(resultat)






        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion des fonds propres
        ##
        ## ######################################################
        ## ######################################################

        message(paste(an, res_revalo[["besoin_emprunt"]]))

        # Appel de la fonction
        res_gest_fp <- gestion_fonds_propres(fp = system@passif@fonds_propres, resultat = res_resultat[["resultat"]], emprunt = res_revalo[["besoin_emprunt"]])

        # Mise a jour de l'attribut
        system@passif@fonds_propres <- res_gest_fp[["fp"]]

        # Mise a jour de le tresorerie apres l'emprunt
        system@actif@ptf_actif@tresorerie@solde <- system@actif@ptf_actif@tresorerie@solde + res_revalo[["besoin_emprunt"]]





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
                                               frais = proj_actif[["flux"]]$frais,
                                               var_vnc = proj_actif[["flux"]][["var_vnc"]],
                                               vente_pvl = if.is_null(get0("pvl_realisees"), 0L)),
                                   resultat_fin_fp = res_fin_fp),
                      passif = list(image = list(epargne = system@passif@ptf_passif@epargne@ptf),
                                    pm_ouverture = proj_passif[["pm_ouverture"]],
                                    flux = proj_passif[["flux"]]),
                      pb = list(revalorisation = list(attribuee = res_revalo[["revalorisation"]],
                                                      prestation = sum_list(proj_passif[["besoin"]][["revalo_prest"]], 1L),
                                                      besoin_cible = res_revalo$besoin_cible)),
                      fonds_propres = list(image = system@passif@fonds_propres,
                                           emprunt = res_revalo[["besoin_emprunt"]]),
                      provision = list(image = system@passif@provision,
                                       flux = list(reserve_capi = res_reserve_capi[["flux"]],
                                                   ppe = res_revalo[["flux_ppe"]])))




        # Output
        return(list(system = system,
                    flux_bel = flux_bel,
                    stock = stock))

    }
)
