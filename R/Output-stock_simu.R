##' Fonction \code{stock_simu}.
##'
##' Cette fonction permet de sortir les elements composant le bilan sur les differentes annees de projection et pour une simulation donnee.
##'
##' @name stock_simu
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param num_simu est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "stock_simu", def = function(output, num_simu, digits = 2L) {standardGeneric("stock_simu")})
setMethod(
    f = "stock_simu",
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
        ##              Construction du data frame
        ##
        ## ######################################################
        ## ######################################################

        stock <- sapply(1L:nb_annees_proj, function(x) {

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


            c(VM_Obligation = vm_obligation, VM_Action = vm_action, VM_Immobilier = vm_immobilier, VM_Tresorerie = vm_tresorerie,
              VNC_Obligation = vnc_obligation, VNC_Action = vnc_action, VNC_Immobilier = vnc_immobilier, VNC_Tresorerie = vnc_tresorerie,
              Total_Actif = vnc_totale,

              Capitaux_propres = capitaux_propres, Dette = dette,
              Report_a_nouveau = ran,
              Resultat_de_l_exercice = res,
              Reserve_capitalisation = rc, PRE = pre, PPE = ppe,
              PM_Cloture = pm_c,
              Total_Passif = passif_total)

        })

        # Arrondir les donnees
        stock <- round(stock, digits = digits)

        # Renommage des colonnes
        colnames(stock) <-  1L:nb_annees_proj

        # Passage en format long
        stock <- melt(stock)

        # Renommage des colonnes
        colnames(stock) <-  c("Element", "annee", "Montant")



        # Output
        return(stock)
    }
)
