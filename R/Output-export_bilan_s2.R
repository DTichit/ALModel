##' Fonction \code{export_bilan_s2}.
##'
##' Cette fonction permet d'exporter plusieurs bilans dans des fichiers excel.
##'
##' @name export_bilan_s2
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param num_simu est un vecteur d'\code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "export_bilan_s2", def = function(output, file = "Bilans_S2.xlsx") {standardGeneric("export_bilan_s2")})
setMethod(
    f = "export_bilan_s2",
    signature = c(output = "Output"),
    definition = function(output, file){



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

        bilan <- plot_bilan(actif = actif, passif = passif, title = "Bilan S2")




        ## ######################################################
        ## ######################################################
        ##
        ##                  FRENCH GAAP
        ##
        ## ######################################################
        ## ######################################################


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
        passif_total <- fp + pt + pm + rc + ppe + pre



        ## ###########################
        ##         BILAN S1
        ## ###########################

        actif <- round(c(Action = vnc_action, Obligation = vnc_obligation,
                   Immobilier = vnc_immobilier, Monetaire = vnc_tresorerie), 2L)

        passif <-  c(Fonds_Propres = fp, PM = pm, Reserve_Capitalisation = rc, PPE = ppe, PRE = pre)
        passif[which(passif == 0)] <- NA







        startRow <- 4L ; startColumn <- 1L


        ## ###########################
        ##    Chargement du package
        ## ###########################

        if(!require(xlsx)){
            install.packages("xlsx")
            require(xlsx)
        }



        # Creation du ficher excel
        wb <- createWorkbook(type="xlsx")

        TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE, heightInPoints=13)  +
            Font(wb, isBold=TRUE, heightInPoints=13, color = "#FEFEFE") +
            Fill(foregroundColor = "#690F3C", backgroundColor="#690F3C")

        TABLE_COLNAMES_STYLE <- CellStyle(wb)  +
            Font(wb, isBold=TRUE, heightInPoints=13, color = "#FEFEFE") +
            Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
            Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_MEDIUM", "BORDER_MEDIUM")) +
            Fill(foregroundColor = "#690F3C", backgroundColor="#690F3C")

        SIMULATION_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
            Font(wb, isBold=TRUE, heightInPoints=14)

        TITLE_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
            Font(wb, isBold=TRUE, heightInPoints=12) +
            Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_MEDIUM", "BORDER_MEDIUM"))

        HAUT_PASSIF_STYLE <- CellStyle(wb) +
            Border(color="black", position="TOP", pen="BORDER_MEDIUM")




        for(x in num_simu) {


            # Construction du bilan
            bilan <- bilan_simu_output(output = output, num_simu = x, digits = digits)
            bilan <- as.data.frame(bilan)


            # Creation d'une page
            name_sheet <- paste("Bilan", "simu", x, sep = "_")
            sheet <- createSheet(wb, sheetName = name_sheet)



            ## ###########################
            ##      Numero de simulation
            ## ###########################

            rows <- createRow(sheet, rowIndex = 1L)
            cells <- createCell(rows, colIndex = 1L)
            setCellValue(cells[[1,1]], paste("Simulation :", x))
            setCellStyle(cells[[1,1]], SIMULATION_STYLE)



            ## ###########################
            ##      Mise en forme
            ## ###########################

            # Modifier hauteurs lignes
            rows  <- createRow(sheet, rowIndex=startRow:(nrow(bilan) + startRow))
            setRowHeight(rows, multiplier=1.25)

            # Modifier largeur colonnes
            setColumnWidth(sheet, colIndex = startColumn, colWidth = 25L)
            setColumnWidth(sheet, colIndex=(startColumn + 1L):(ncol(bilan) + startColumn), colWidth = 15L)

            # Lignes de totaux
            num_actif <- which(rownames(bilan) == "Total_Actif")
            num_passif <- which(rownames(bilan) == "Total_Passif")
            decalage <- 1L

            # Ecriture des donnees
            addDataFrame(bilan[1L:num_actif,], sheet, startRow=startRow, startColumn=startColumn,
                         colnamesStyle = TABLE_COLNAMES_STYLE,
                         rownamesStyle = TABLE_ROWNAMES_STYLE)
            addDataFrame(bilan[(num_actif+decalage):num_passif,], sheet, startRow=startRow+num_actif+2L, startColumn=startColumn,
                         col.names = FALSE, rownamesStyle = TABLE_ROWNAMES_STYLE)

            # Bordure haut du passif
            rows  <- getRows(sheet, rowIndex = startRow + num_actif + decalage + 1L)
            cells <- getCells(rows, colIndex = (startColumn + 1L):(ncol(bilan)+startColumn))
            sapply(1L:length(cells), function(x) setCellStyle(cells[[x]], cellStyle = HAUT_PASSIF_STYLE))


            # Augmenter taille PASSIF et ACTIF
            rows  <- getRows(sheet, rowIndex = startRow + c(num_actif, num_passif + decalage))
            cells <- getCells(rows, colIndex = (startColumn + 1L):(ncol(bilan)+startColumn))
            sapply(1L:length(cells), function(x) setCellStyle(cells[[x]], cellStyle = TITLE_STYLE))


        }


        # Ecriture du fichier
        saveWorkbook(wb, file)

    }
)
