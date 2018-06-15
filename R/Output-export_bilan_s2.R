##' Fonction \code{export_bilan_s2}.
##'
##' Cette fonction permet d'exporter le bilan S2 dans un fichier pdf.
##'
##' @name export_bilan_s2
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param file est un \code{character}. Par defaut, sa valeur est 'Bilan_S2.pdf'.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "export_bilan_s2", def = function(output, file = "Bilan_S2.pdf", width = 7, height = 7) {standardGeneric("export_bilan_s2")})
setMethod(
    f = "export_bilan_s2",
    signature = c(output = "Output"),
    definition = function(output, file, width, height){



        ## ######################################################
        ## ######################################################
        ##
        ##                  SOLVABILITE 2
        ##
        ## ######################################################
        ## ######################################################


        ## ###########################
        ##          ACTIFS
        ## ###########################

        # VMs actifs
        vm_obligation  <- sum(output@system@actif@ptf_actif@obligation@ptf$valeur_marche)
        vm_action      <- sum(output@system@actif@ptf_actif@action@ptf$valeur_marche)
        vm_immobilier  <- sum(output@system@actif@ptf_actif@immobilier@ptf$valeur_marche)
        vm_tresorerie  <- sum(output@system@actif@ptf_actif@tresorerie@solde)

        # Somme
        vm_totale      <- vm_obligation + vm_action + vm_immobilier + vm_tresorerie


        ## ###########################
        ##          PASSIFS
        ## ###########################

        # Extraction de donnees
        bel <- mean(output@be)
        nav <- mean(output@nav)

        # Somme
        passif_total <- bel + nav



        ## ###########################
        ##         BILAN S2
        ## ###########################

        # Data
        actif <- c(Action = vm_action, Obligation = vm_obligation,
                   Immobilier = vm_immobilier, Monetaire = vm_tresorerie)

        passif <-  c(NAV = nav, BE = bel)


        actif <- data.frame(Type = "Actif", Element = names(actif), Montant = round(actif, 2L))
        passif <- data.frame(Type = "Passif", Element = names(passif), Montant = round(passif, 2L))
        passif$Element <- factor(passif$Element, levels = passif$Element[order(passif$Element, decreasing = TRUE)])




        ## ###########################
        ##    Construction du Bilan
        ## ###########################

        # Enregistrer le graphique
        pdf(file = file, width = width, height = height)

        # Appel de la fonciton
        plot_bilan(actif = actif, passif = passif, title = "Bilan S2")

        # Fermeture du fichier
        dev.off()

    }
)
