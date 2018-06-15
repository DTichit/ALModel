##' Fonction \code{export_bilan_s1}.
##'
##' Cette fonction permet d'exporter le bilan S1 dans un fichier pdf.
##'
##' @name export_bilan_s1
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param file est un \code{character}. Par defaut, sa valeur est 'Bilan_S1.pdf'.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "export_bilan_s1", def = function(output, file = "Bilan_S1.pdf", width = 7, height = 7) {standardGeneric("export_bilan_s1")})
setMethod(
    f = "export_bilan_s1",
    signature = c(output = "Output"),
    definition = function(output, file, width, height){



        ## ###########################
        ##          ACTIFS
        ## ###########################

        # VNCs actifs
        vnc_obligation <- sum(output@system@actif@ptf_actif@obligation@ptf$valeur_nette_comptable)
        vnc_action     <- sum(output@system@actif@ptf_actif@action@ptf$valeur_comptable)
        vnc_immobilier <- sum(output@system@actif@ptf_actif@immobilier@ptf$valeur_comptable)
        vnc_tresorerie <- sum(output@system@actif@ptf_actif@tresorerie@solde)

        # Total
        vnc_totale      <- vnc_obligation + vnc_action + vnc_immobilier + vnc_tresorerie




        ## ###########################
        ##          PASSIFS
        ## ###########################

        # Fonds propres
        fp <- calcul_fonds_propres(output@system@passif@fonds_propres)[["total"]]

        # Provisions techniques
        pt <- calcul_pt(output@system@passif)[["pt"]]
        pm <- sum_list(pt$pm, 1L)
        rc <- pt[["reserve_capi"]]
        ppe <- pt[["ppe"]]
        pre <- pt[["pre"]]

        # Total
        passif_total <- fp + pm + rc + ppe + pre




        ## ###########################
        ##         BILAN S1
        ## ###########################

        actif <- round(c(Action = vnc_action, Obligation = vnc_obligation,
                   Immobilier = vnc_immobilier, Monetaire = vnc_tresorerie), 2L)

        passif <-  c(Fonds_Propres = fp, PM = pm, Reserve_Capitalisation = rc, PPE = ppe, PRE = pre)
        passif[which(passif == 0)] <- NA


        actif <- data.frame(Type = "Actif", Element = names(actif), Montant = round(actif, 2L))
        passif <- data.frame(Type = "Passif", Element = names(passif), Montant = round(passif, 2L))
        passif$Element <- factor(passif$Element, levels = passif$Element[order(passif$Element, decreasing = FALSE)])




        ## ###########################
        ##    Construction du Bilan
        ## ###########################

        # Enregistrer le graphique
        pdf(file = file, width = width, height = height)

        # Appel de la fonciton
        plot_bilan(actif = actif, passif = passif, title = "Bilan S1")

        # Fermeture du fichier
        dev.off()




    }
)
