##' Fonction \code{export_bilan}.
##'
##' Cette fonction permet d'exporter plusieurs bilans dans des fichiers excel.
##'
##' @name export_bilan
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param num_simu est un vecteur d'\code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "export_bilan", def = function(output, num_simu, digits = 2L, file = "Bilans_FRENCH_GAAP.xlsx") {standardGeneric("export_bilan")})
setMethod(
    f = "export_bilan",
    signature = c(output = "Output", num_simu = "integer"),
    definition = function(output, num_simu, digits, file){



        ## ###########################
        ##          Parametres
        ## ###########################

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
