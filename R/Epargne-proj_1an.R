##' Fonction \code{proj_1an_epargne}.
##'
##' Cette fonction permet de projeter horizon 1 an un portefeuille de contrats epragnes.
##'
##' @name proj_1an_epargne
##' @docType methods
##' @param epargne est un objet de type \code{\link{Epargne}}.
##' @param hyp_passif est un objet de type \code{\link{HypPassif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Epargne-class.R HypPassif-class.R
##'
setGeneric(name = "proj_1an_epargne", def = function(epargne, hyp_passif) {standardGeneric("proj_1an_epargne")})
setMethod(
    f = "proj_1an_epargne",
    signature = c(epargne = "Epargne", hyp_passif = "HypPassif"),
    definition = function(epargne, hyp_passif){

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(epargne@ptf)



        ## ######################################################
        ## ######################################################
        ##
        ##               Evaluation des prestations
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        nb_contr_ptf_epargne <- .subset2(epargne@ptf, which(name_ptf == "nb_contr"))
        sexe_ptf_epargne <- .subset2(epargne@ptf, which(name_ptf == "sexe"))
        age_ptf_epargne  <- .subset2(epargne@ptf, which(name_ptf == "age"))
        anc_ptf_epargne  <- .subset2(epargne@ptf, which(name_ptf == "anc"))
        pm_ptf_epargne   <- .subset2(epargne@ptf, which(name_ptf == "pm"))


        ## ###########################
        ## Gestion des deces
        ## ###########################

        # Selection des contrats par sexe
        contrats_h <- which(sexe_ptf_epargne == "H")
        contrats_f <- which(sexe_ptf_epargne == "F")

        # Calcul des taux de deces par model point
        tx_deces <- rep(x = NA, length = nrow(epargne@ptf))
        tx_deces[contrats_h] <- calc_qx(tab_morta = hyp_passif@tab_morta_h, age = age_ptf_epargne[contrats_h])
        tx_deces[contrats_f] <- calc_qx(tab_morta = hyp_passif@tab_morta_f, age = age_ptf_epargne[contrats_f])

        # Calcul des prestations relatives aux deces
        deces <- tx_deces * pm_ptf_epargne


        ## ###########################
        ## Gestion des rachats structurels
        ## ###########################

        # Calcul des taux de rachats par model point
        tx_rachat_tot  <- calc_rx(tab_rachat = hyp_passif@tab_rachat_tot, anc = anc_ptf_epargne)
        tx_rachat_part <- calc_rx(tab_rachat = hyp_passif@tab_rachat_part, anc = anc_ptf_epargne)

        # Calcul des prestations relatives aux rachats
        rachat_tot  <- tx_rachat_tot * pm_ptf_epargne
        rachat_part <- tx_rachat_part * pm_ptf_epargne


        warning("Les rachats dynamiques sont a inserer !")







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
        chgt_rachats <- (rachat_tot + rachat_part) * tx_chgt_rachats
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

        # Extraction des donnees
        nb_contr_ptf_epargne <- .subset2(epargne@ptf, which(name_ptf == "nb_contr"))

        # Calcul des differents frais
        frais_gestion <- nb_contr_ptf_epargne * frais_gestion
        frais_rachats <- nb_contr_ptf_epargne * (tx_rachat_tot + tx_rachat_part) * frais_rachats
        frais_deces   <- nb_contr_ptf_epargne * tx_deces * frais_deces

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
        new_pm <- pm_ptf_epargne - prestations - chgt
        new_nb_contr <- nb_contr_ptf_epargne * (1 - tx_deces - tx_rachat_tot)

        # Mise a jour de l'objet
        epargne@ptf$nb_contr <- new_nb_contr
        epargne@ptf$pm       <- new_pm

        # Vieillissement du portfeuille
        epargne@ptf$age <- age_ptf_epargne + 1L
        epargne@ptf$anc <- anc_ptf_epargne + 1L




        ## ######################################################
        ## ######################################################
        ##
        ##              Revalorisation des contrats
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        pm_ptf_epargne   <- .subset2(epargne@ptf, which(name_ptf == "pm"))
        tmg_ptf_epargne  <- .subset2(epargne@ptf, which(name_ptf == "tmg"))

        # Calcul besoin revalorisation sur les PM restantes
        revalo_tmg_pm <- pm_ptf_epargne * tmg_ptf_epargne

        # Calcul besoin revalorisation sur les prestations (en milieu d'annee)
        revalo_tmg_prest <- prestations * (tmg_ptf_epargne^0.5)
        warning("Revalorisation des prestations a revoir")



        warning("A voir si le calcul de la revalorisation est correcte")



        # Output
        return(list(epargne = epargne,
                    flux = list(prestation = list(deces = sum(deces),
                                                  rachat_tot = sum(rachat_tot),
                                                  rachat_part = sum(rachat_part)),
                                chargement = list(gestion = sum(chgt_gestion),
                                                  rachats = sum(chgt_rachats),
                                                  deces = sum(chgt_deces)),
                                frais = list(gestion = sum(frais_gestion),
                                             rachats = sum(frais_rachats),
                                             deces = sum(frais_deces))),
                    besoin = list(revalo_tmg = list(pm = sum(revalo_tmg_pm),
                                                    prestation = sum(revalo_tmg_prest)))))
    }
)
