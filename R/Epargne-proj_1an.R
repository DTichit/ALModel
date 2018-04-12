##' Fonction \code{proj_1an_epargne}.
##'
##' Cette fonction permet de projeter horizon 1 an un portefeuille de contrats epragnes.
##'
##' @name proj_1an_epargne
##' @docType methods
##' @param epargne est un objet de type \code{\link{Epargne}}.
##' @param hyp_passif est un objet de type \code{\link{HypPassif}}.
##' @param an est un objet de type \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Epargne-class.R HypPassif-class.R
##'
setGeneric(name = "proj_1an_epargne", def = function(epargne, hyp_passif, an) {standardGeneric("proj_1an_epargne")})
setMethod(
    f = "proj_1an_epargne",
    signature = c(epargne = "Epargne", hyp_passif = "HypPassif", an = "integer"),
    definition = function(epargne, hyp_passif, an){

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(epargne@ptf)



        ## ######################################################
        ## ######################################################
        ##
        ##           Mise en memoire des PM a l'ouverture
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        pm_ptf_epargne   <- .subset2(epargne@ptf, which(name_ptf == "pm"))

        # PM a l'ouverture
        pm_ouverture <- sum(pm_ptf_epargne)



        ## ######################################################
        ## ######################################################
        ##
        ##               Evaluation des prestations
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        nb_contr_ptf_epargne <- .subset2(epargne@ptf, which(name_ptf == "nb_contr"))
        revalo_prec_ptf_epargne   <- .subset2(epargne@ptf, which(name_ptf == "revalo_prec"))


        ## ###########################
        ## Gestion des deces
        ## ###########################

        # Extraction des taux de deces par model point
        tx_deces_pm     <- .subset2(epargne@proba@deces_pm, an + 1L)
        tx_deces_contr  <- .subset2(epargne@proba@deces_contr, an + 1L)

        # Calcul des prestations relatives aux deces
        deces <- tx_deces_pm * pm_ptf_epargne


        ## ###########################
        ## Gestion des rachats structurels
        ## ###########################

        # Extraction des taux de rachats par model point
        tx_rachat_tot_pm  <- .subset2(epargne@proba@rachat_tot_pm, an + 1L)
        tx_rachat_tot_contr  <- .subset2(epargne@proba@rachat_tot_contr, an + 1L)
        tx_rachat_part <- .subset2(epargne@proba@rachat_part, an + 1L)

        # Calcul des prestations relatives aux rachats
        rachat_tot  <- tx_rachat_tot_pm * pm_ptf_epargne
        rachat_part <- tx_rachat_part * pm_ptf_epargne


        ## ###########################
        ## Gestion des rachats conjocturels
        ## ###########################

        # Calcul des taux de rachats
        tx_rachat_conj <- calc_rachat_conj(rachat_conj = hyp_passif@rachat_conj, tx_cible = hyp_passif@cible$epargne[an], tx_serv = revalo_prec_ptf_epargne)

        # Calcul des prestations
        rachat_conj <- tx_rachat_conj * pm_ptf_epargne







        ## ######################################################
        ## ######################################################
        ##
        ##            Evaluation des primes versees
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnees
        prime <- .subset2(epargne@ptf, which(name_ptf == "prime"))







        ## ######################################################
        ## ######################################################
        ##
        ##               Evaluation des chargements
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des chargements
        tx_chgt_gestion <- .subset2(epargne@ptf, which(name_ptf == "chgt_gestion"))
        tx_chgt_rachats <- .subset2(epargne@ptf, which(name_ptf == "chgt_rachats"))
        tx_chgt_deces   <- .subset2(epargne@ptf, which(name_ptf == "chgt_deces"))

        # Extraction des donnees
        pm_ptf_epargne   <- .subset2(epargne@ptf, which(name_ptf == "pm"))

        # Calcul des differents chargements
        chgt_gestion <- pm_ptf_epargne * tx_chgt_gestion
        chgt_rachats <- (rachat_tot + rachat_part + rachat_conj) * tx_chgt_rachats
        chgt_deces   <- deces * tx_chgt_deces

        # Aggregation des chargements
        chgt <- chgt_gestion + chgt_rachats + chgt_deces







        ## ######################################################
        ## ######################################################
        ##
        ##                Evaluation des frais
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des frais
        frais_gestion <- .subset2(epargne@ptf, which(name_ptf == "frais_uni_gestion"))
        frais_rachats <- .subset2(epargne@ptf, which(name_ptf == "frais_uni_rachats"))
        frais_deces   <- .subset2(epargne@ptf, which(name_ptf == "frais_uni_deces"))

        # Extraction de l'inflation
        inf <- hyp_passif@esg_simu$inflation[an]

        # Extraction des donnees
        nb_contr_ptf_epargne <- .subset2(epargne@ptf, which(name_ptf == "nb_contr"))

        # Calcul des differents frais
        frais_gestion <- nb_contr_ptf_epargne * frais_gestion * (1 - inf)
        frais_rachats <- nb_contr_ptf_epargne * tx_rachat_tot_contr * frais_rachats * (1 - inf)
        frais_deces   <- nb_contr_ptf_epargne * tx_deces_contr * frais_deces * (1 - inf)

        # Aggregation des frais
        frais <- frais_gestion + frais_rachats + frais_deces




        ## ######################################################
        ## ######################################################
        ##
        ##       Mise a jour des MP - Vieillissement du PTF
        ##
        ## ######################################################
        ## ######################################################

        # Prestations
        prestations <- deces + (rachat_tot + rachat_part + rachat_conj)

        # Calcul des nouvelles PM
        new_pm <- pm_ptf_epargne - prestations - chgt + prime
        new_nb_contr <- nb_contr_ptf_epargne * (1 - tx_deces_contr - tx_rachat_tot_contr)

        # Mise a jour de l'objet
        epargne@ptf$nb_contr <- new_nb_contr
        epargne@ptf$pm       <- new_pm

        # Appliquer l'inflation aux frais
        epargne@ptf$frais_uni_gestion   <- .subset2(epargne@ptf, which(name_ptf == "frais_uni_gestion")) * (1 - inf)
        epargne@ptf$frais_uni_rachats   <- .subset2(epargne@ptf, which(name_ptf == "frais_uni_rachats")) * (1 - inf)
        epargne@ptf$frais_uni_deces     <- .subset2(epargne@ptf, which(name_ptf == "frais_uni_deces")) * (1 - inf)

        # Vieillissement du portfeuille : seulement dans la simulation calculant les probas
        if(hyp_passif@calc_proba){
            epargne@ptf$age <- .subset2(epargne@ptf, which(name_ptf == "age")) + 1L
            epargne@ptf$anc <- .subset2(epargne@ptf, which(name_ptf == "anc")) + 1L
        }



        ## ######################################################
        ## ######################################################
        ##
        ##        Besoin en revalorisation des prestations
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        tmg_ptf_epargne  <- .subset2(epargne@ptf, which(name_ptf == "tmg"))

        # Calcul besoin revalorisation sur les prestations (en milieu d'annee)
        revalo_tmg_prest <- prestations * (tmg_ptf_epargne * 0.5)





        # Output
        return(list(epargne = epargne,
                    pm_ouverture = pm_ouverture,
                    flux = list(prestation = list(deces = sum(deces),
                                                  rachat_tot = sum(rachat_tot),
                                                  rachat_part = sum(rachat_part),
                                                  rachat_conj = sum(rachat_conj)),
                                prime = sum(prime),
                                chargement = list(gestion = sum(chgt_gestion),
                                                  rachats = sum(chgt_rachats),
                                                  deces = sum(chgt_deces)),
                                frais = list(gestion = sum(frais_gestion),
                                             rachats = sum(frais_rachats),
                                             deces = sum(frais_deces))),
                    besoin = list(revalo_prestation = sum(revalo_tmg_prest))))
    }
)
