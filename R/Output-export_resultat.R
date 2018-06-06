##' Fonction \code{export_resultat}.
##'
##' Cette fonction permet d'exporter plusieurs comptes de resultat dans des fichiers excel.
##'
##' @name export_resultat
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param num_simu est un vecteur d'\code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "export_resultat", def = function(output, num_simu, digits = 2L, file = "Resultats.xlsx") {standardGeneric("export_resultat")})
setMethod(
    f = "export_resultat",
    signature = c(output = "Output", num_simu = "integer"),
    definition = function(output, num_simu, digits, file){



        ## ###########################
        ##          Parametres
        ## ###########################

        startRow <- 3L ; startColumn <- 1L


        ## ###########################
        ##    Chargement du package
        ## ###########################

        if(!require(xlsx)){
            install.packages("xlsx")
            require(xlsx)
        }



        # Creation du ficher excel
        wb <- createWorkbook(type="xlsx")

        TABLE_ROWNAMES_STYLE <- CellStyle(wb) +
            Font(wb, isBold=FALSE, heightInPoints=12, color = "#FEFEFE") +
            Fill(foregroundColor = "#690F3C", backgroundColor="#690F3C")

        BOLD_STYLE <-  TABLE_ROWNAMES_STYLE +
            Font(wb, isBold=TRUE, heightInPoints=12, color = "#FEFEFE")

        TABLE_COLNAMES_STYLE <- CellStyle(wb)  +
            Font(wb, isBold=TRUE, heightInPoints=13, color = "#FEFEFE") +
            Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
            Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_MEDIUM", "BORDER_MEDIUM")) +
            Fill(foregroundColor = "#690F3C", backgroundColor="#690F3C")

        SIMULATION_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
            Font(wb, isBold=TRUE, heightInPoints=14)

        MARGE_STYLE <- CellStyle(wb) +
            Font(wb, isBold=TRUE, heightInPoints=12) +
            Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_MEDIUM", "BORDER_MEDIUM"))

        RESULTAT_STYLE <- CellStyle(wb) +
            Font(wb, isBold=TRUE, heightInPoints=13) +
            Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_MEDIUM", "BORDER_MEDIUM"))




        for(x in num_simu) {


            # Construction du CdR
            resultat <- resultat_simu_output(output = output, num_simu = x, digits = digits)
            resultat <- as.data.frame(resultat)


            # Creation d'une page
            name_sheet <- paste("Resultat", "simu", x, sep = "_")
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
            rows  <- createRow(sheet, rowIndex=startRow:(nrow(resultat) + startRow))
            setRowHeight(rows, multiplier=1.25)

            # Modifier largeur colonnes
            setColumnWidth(sheet, colIndex = startColumn, colWidth = 35L)
            setColumnWidth(sheet, colIndex=(startColumn + 1L):(ncol(resultat) + startColumn), colWidth = 15L)

            # Lignes de totaux
            num_souscription <- which(rownames(resultat) == "Marge_Souscription")
            num_gestion <- which(rownames(resultat) == "Marge_de_Gestion")
            num_financiere <- which(rownames(resultat) == "Marge_Financiere")
            num_resultat <- which(rownames(resultat) == "Resultat")

            # Ecriture des donnees
            addDataFrame(resultat, sheet, startRow=startRow, startColumn=startColumn,
                         colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE)

            # Augmenter taille MARGES
            rows  <- getRows(sheet, rowIndex = startRow + c(num_souscription, num_gestion, num_financiere))
            cells <- getCells(rows, colIndex = (startColumn + 1L):(ncol(resultat)+startColumn))
            sapply(1L:length(cells), function(x) setCellStyle(cells[[x]], cellStyle = MARGE_STYLE))

            # Augmenter taille RESULTAT
            rows  <- getRows(sheet, rowIndex = startRow + num_resultat)
            cells <- getCells(rows, colIndex = (startColumn + 1L):(ncol(resultat)+startColumn))
            sapply(1L:length(cells), function(x) setCellStyle(cells[[x]], cellStyle = RESULTAT_STYLE))

            # Mettre en gras les marges
            rows  <- getRows(sheet, rowIndex = startRow + c(num_souscription, num_gestion, num_financiere, num_resultat))
            cells <- getCells(rows, colIndex = 1L)
            sapply(1L:length(cells), function(x) setCellStyle(cells[[x]], cellStyle = BOLD_STYLE))


        }


        # Ecriture du fichier
        saveWorkbook(wb, file)

    }
)
