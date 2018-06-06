##' Fonction \code{bilan_simu_output}.
##'
##' Cette fonction permet de sortir les bilans (FRENCH GAAP) sur les differentes annees de projection pour une simulation donnee.
##'
##' @name bilan_simu_output
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param num_simu est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "bilan_simu_output", def = function(output, num_simu, digits = 2L) {standardGeneric("bilan_simu_output")})
setMethod(
    f = "bilan_simu_output",
    signature = c(output = "Output", num_simu = "integer"),
    definition = function(output, num_simu, digits){



        ## ###########################
        ## Nombre d'annees de projection
        ## ###########################

        # Extraction de la donnee
        nb_annees_proj <- length(output@stock[[1L]])






        ## ######################################################
        ## ######################################################
        ##
        ##              Construction des bilans
        ##
        ## ######################################################
        ## ######################################################

        bilan <- sapply(1L:nb_annees_proj, function(x) {

            # Extraction de donnees
            stock <- output@stock[[num_simu]][[x]]


            ## ###########################
            ##           ACTIF
            ## ###########################

            # VMs actifs
            vm_obligation  <- sum(stock$actif@obligation@ptf$valeur_marche)
            vm_action      <- sum(stock$actif@action@ptf$valeur_marche)
            vm_immobilier  <- sum(stock$actif@immobilier@ptf$valeur_marche)
            vm_tresorerie  <- sum(stock$actif@tresorerie@solde)
            vm_totale      <- vm_obligation + vm_action + vm_immobilier + vm_tresorerie

            # VNC actif
            vnc_obligation  <- sum(stock$actif@obligation@ptf$valeur_nette_comptable)
            vnc_action      <- sum(stock$actif@action@ptf$valeur_comptable)
            vnc_immobilier  <- sum(stock$actif@immobilier@ptf$valeur_comptable)
            vnc_tresorerie  <- sum(stock$actif@tresorerie@solde)
            vnc_totale      <- vnc_obligation + vnc_action + vnc_immobilier + vnc_tresorerie

            # PMVL
            pmvl <- vm_totale - vnc_totale





            ## ###########################
            ##           PASSIF
            ## ###########################

            # Capitaux propres
            capitaux_propres <- stock$fonds_propres@capitaux_propres

            # Resultat de l'exercice
            res <- stock$fonds_propres@resultat_exercice

            # Report a nouveau
            ran <- stock$fonds_propres@report_a_nouveau

            # Emprunt
            dette <- stock$fonds_propres@dette


            # PPE
            ppe <- sum(stock$provision@ppe@ppe)

            # PPE
            pre <- sum(stock$provision@pre@montant)

            # RC
            rc <- stock$provision@reserve_capi@montant

            # PM de cloture
            pm_c <- sum(stock$passif$epargne$pm)


            passif_total <- capitaux_propres + ran + res + ppe + pm_c + rc + dette + pre


            c(PMVL = pmvl,
              Obligation = vnc_obligation, Action = vnc_action, Immobilier = vnc_immobilier, Tresorerie = vnc_tresorerie,
              Total_Actif = vnc_totale,

              Capitaux_propres = capitaux_propres, Dette = dette,
              Report_a_nouveau = ran,
              Resultat_de_l_exercice = res,
              Reserve_capitalisation = rc, PRE = pre, PPE = ppe,
              PM_Cloture = pm_c,
              Total_Passif = passif_total)

        })

        # Arrondir les donnees
        bilan <- round(bilan, digits = digits)

        # Renommage des colonnes
        colnames(bilan) <- 1L:nb_annees_proj



        # Output
        return(bilan)
    }
)
