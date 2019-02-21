##' Fonction \code{proj_1an_epargne}.
##'
##' Cette fonction permet de projeter horizon 1 an un portefeuille de contrats epragnes.
##'
##' @name proj_1an_epargne
##' @docType methods
##' @param epargne est un objet de type \code{\link{Epargne}}.
##' @param hyp_passif est un objet de type \code{\link{HypPassif}}.
##' @param an est un objet de type \code{integer}.
##' @param agreg_out est une valeur \code{logical} qui indique si les sorties doivent etre agregees. Par defaut, sa valeur est a TRUE.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Epargne-class.R HypPassif-class.R
##'
setGeneric(name = "proj_1an_epargne", def = function(epargne, hyp_passif, an, agreg_out = TRUE) {standardGeneric("proj_1an_epargne")})
setMethod(
    f = "proj_1an_epargne",
    signature = c(epargne = "Epargne", hyp_passif = "HypPassif", an = "integer"),
    definition = function(epargne, hyp_passif, an, agreg_out){

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
        pm_ouverture <- pm_ptf_epargne



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
        ## Gestion des rachats
        ## ###########################

        # Calcul des taux de rachats
        tx_rachat_conj <- calc_rachat_conj(rachat_conj = hyp_passif@rachat_conj, tx_cible = hyp_passif@cible$epargne[an], tx_serv = revalo_prec_ptf_epargne)

        # Repartition des taux (partiel / total)
        tx_rachat_conj_partiel <- tx_rachat_conj * hyp_passif@rachat_conj@repartition$partiel
        tx_rachat_conj_total   <- tx_rachat_conj * hyp_passif@rachat_conj@repartition$total

        # Extraction des taux de rachats structurels par model point
        tx_rachat_struct_tot_pm     <- .subset2(epargne@proba@rachat_tot_pm, an + 1L)
        tx_rachat_struct_tot_contrat<- .subset2(epargne@proba@rachat_tot_contr, an + 1L)
        tx_rachat_struct_part       <- .subset2(epargne@proba@rachat_part, an + 1L)

        # Calcul des taux de rachats appliques
        taux_rachat_tot_pm      <- pmax(tx_rachat_struct_tot_pm + tx_rachat_conj_total, 0)
        taux_rachat_tot_contrat <- pmax(tx_rachat_struct_tot_contrat + tx_rachat_conj_total, 0)
        taux_rachat_partiel     <- pmax(tx_rachat_struct_part + tx_rachat_conj_partiel, 0)


        # Calcul des prestations relatives aux rachats
        rachat_tot  <- taux_rachat_tot_pm * pm_ptf_epargne
        rachat_part <- taux_rachat_partiel * pm_ptf_epargne







        ## ######################################################
        ## ######################################################
        ##
        ##          Evaluation des primes versees deduites
        ##               des chargements d'acquisition
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnees
        tx_chgt_acquisition <- .subset2(epargne@ptf, which(name_ptf == "chgt_acquisition"))
        prime <- .subset2(epargne@ptf, which(name_ptf == "prime"))

        # Calcul des chargements d'acquisisition
        chgt_acquisition <- tx_chgt_acquisition * prime

        # Montant des primes ajoutees aux PM
        primes_ajoutees <- prime - chgt_acquisition







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
        frais_gestion <- nb_contr_ptf_epargne * frais_gestion * (1 + inf)
        frais_rachats <- (nb_contr_ptf_epargne * taux_rachat_tot_contrat) * frais_rachats * (1 + inf)
        frais_deces   <- (nb_contr_ptf_epargne * tx_deces_contr) * frais_deces * (1 + inf)

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
        prestations <- deces + (rachat_tot + rachat_part)

        # Calcul des nouvelles PM
        new_pm <- pmax(pm_ptf_epargne - prestations, 0) + primes_ajoutees
        new_nb_contr <- nb_contr_ptf_epargne * (1 - pmin(tx_deces_contr + taux_rachat_tot_contrat, 1))

        # Mise a jour de l'objet
        epargne@ptf$nb_contr <- new_nb_contr
        epargne@ptf$pm       <- new_pm

        # Appliquer l'inflation aux frais
        epargne@ptf$frais_uni_gestion   <- .subset2(epargne@ptf, which(name_ptf == "frais_uni_gestion")) * (1 + inf)
        epargne@ptf$frais_uni_rachats   <- .subset2(epargne@ptf, which(name_ptf == "frais_uni_rachats")) * (1 + inf)
        epargne@ptf$frais_uni_deces     <- .subset2(epargne@ptf, which(name_ptf == "frais_uni_deces")) * (1 + inf)

        # Vieillissement du portfeuille : seulement dans la simulation calculant les probas
        if(hyp_passif@calc_proba){
            epargne@ptf$age <- .subset2(epargne@ptf, which(name_ptf == "age")) + 1L
            epargne@ptf$anc <- .subset2(epargne@ptf, which(name_ptf == "anc")) + 1L
        }









        ## ######################################################
        ## ######################################################
        ##
        ##      Calculs des chargements sur les nouvelles PM
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnees
        tx_chgt_administration  <- .subset2(epargne@ptf, which(name_ptf == "chgt_administration"))
        pm_ptf_epargne          <- .subset2(epargne@ptf, which(name_ptf == "pm"))

        # Calcul des chargements
        chgt_administration <- pm_ptf_epargne * tx_chgt_administration

        # Somme des chargements
        chgt <- chgt_administration + chgt_acquisition




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
                    pm_ouverture = sum_cond(x = pm_ouverture, cond = agreg_out),
                    flux = list(prestation = list(deces = sum_cond(x = deces, cond = agreg_out),
                                                  rachat_tot = sum_cond(x = rachat_tot, cond = agreg_out),
                                                  rachat_part = sum_cond(x = rachat_part, cond = agreg_out)),
                                prime = sum_cond(x = prime, cond = agreg_out),
                                chargement = list(administration = sum_cond(x = chgt_administration, cond = agreg_out),
                                                  acquisition = sum_cond(x = chgt_acquisition, cond = agreg_out)),
                                frais = list(gestion = sum_cond(x = frais_gestion, cond = agreg_out),
                                             rachats = sum_cond(x = frais_rachats, cond = agreg_out),
                                             deces = sum_cond(x = frais_deces, cond = agreg_out))),
                    besoin = list(revalo_prestation = sum_cond(x = revalo_tmg_prest, cond = agreg_out))))
    }
)
