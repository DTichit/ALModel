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


        ## ######################################################
        ## ######################################################
        ##
        ##              Revalorisation des contrats
        ##
        ## ######################################################
        ## ######################################################

        #################################################################
        #################################################################
        #################################################################
        #               A FAIRE ! : Mettre PB, et TMG
        #################################################################
        #################################################################
        #################################################################






        ## ######################################################
        ## ######################################################
        ##
        ##               Evaluation des prestations
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        name_ptf <- names(epargne@ptf)
        nb_contr_ptf_epargne <- .subset2(epargne@ptf, which(name_ptf == "nb_contr"))
        sexe_ptf_epargne <- .subset2(epargne@ptf, which(name_ptf == "sexe"))
        age_ptf_epargne  <- .subset2(epargne@ptf, which(name_ptf == "age"))
        pm_ptf_epargne   <- .subset2(epargne@ptf, which(name_ptf == "pm"))
        chgt_ptf_epargne <- .subset2(epargne@ptf, which(name_ptf == "chgt"))
        prime_ptf_epargne <- .subset2(epargne@ptf, which(name_ptf == "prime"))


        ## ###########################
        ## Gestion des deces
        ## ###########################

        # Selection des contrats par sexe
        contrats_h <- which(sexe_ptf_epargne == "H")
        contrats_f <- which(sexe_ptf_epargne == "F")

        # Calcul des taux de deces par model point
        tx_deces <- rep(x = NA, length = nrow(epargne@ptf))
        tx_deces[contrats_h] <- calc_qx(tab_morta = hyp_passif@tab_morta_h, age = age_ptf_epargne[contrats_h])
        tx_deces[contrats_f] <- calc_qx(tab_morta = hyp_passif@tab_morta_h, age = age_ptf_epargne[contrats_f])

        # Calcul des prestations relatives aux deces
        deces <- tx_deces * pm_ptf_epargne


        ## ###########################
        ## Gestion des rachats structurels
        ## ###########################

        # Calcul des taux de rachats par model point
        tx_rachat_tot  <- calc_rx(tab_rachat = hyp_passif@tab_rachat_tot, age = age_ptf_epargne)
        tx_rachat_part <- calc_rx(tab_rachat = hyp_passif@tab_rachat_part, age = age_ptf_epargne)

        # Calcul des prestations relatives aux rachats
        rachat_tot  <- tx_rachat_part * pm_ptf_epargne
        rachat_part <- tx_rachat_part * pm_ptf_epargne







        ## ######################################################
        ## ######################################################
        ##
        ##               Evaluation des chargements
        ##
        ## ######################################################
        ## ######################################################

        # Calcul des chargements
        chgt <- chgt_ptf_epargne * pm_ptf_epargne





        ## ######################################################
        ## ######################################################
        ##
        ##       Mise a jour des MP - Vieillissement du PTF
        ##
        ## ######################################################
        ## ######################################################

        # Calcul des nouvelles PM
        new_pm <- pm_ptf_epargne - (deces + rachat_tot + rachat_part + chgt) + prime_ptf_epargne
        new_nb_contr <- nb_contr_ptf_epargne * (1 - tx_deces - tx_rachat_tot)

        # Mise a jour de l'objet
        epargne@ptf$nb_contr <- new_nb_contr
        epargne@ptf$pm       <- new_pm

        # Vieillissement du portfeuille
        epargne@ptf$age <- age_ptf_epargne + 1L




        # Output
        return(list(epargne = epargne,
                    prestations = list(deces = deces,
                                       rachat_tot = rachat_tot,
                                       rachat_part = rachat_part),
                    chargements = chgt))
    }
)
