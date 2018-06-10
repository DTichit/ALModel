##' Fonction \code{export_all}.
##'
##' Cette fonction permet differents elements a la suite du run : Bilans FRENCH-GAAP, Compte des resultats...
##'
##' @name export_all
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param num_simu est un vecteur d'\code{integer}.
##' @param detail est un \code{logical}. Lorsqu'il est egal a \code{TRUE}, tous les bilans et comptes de resultats seront sortis. Par defaut, sa valeur est \code{FALSE}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "export_all", def = function(output, digits = 2L, directory = "Output", by = 50L, detail = FALSE) {standardGeneric("export_all")})
setMethod(
    f = "export_all",
    signature = c(output = "Output"),
    definition = function(output, digits, directory, by, detail){


        ## ###########################
        ##    Gestion du dossier
        ## ###########################

        # Message
        message("Creation de l'architecture")

        # Construction du chemin
        if(dirname(directory) == ".")
            dir <- paste(getwd(), directory, sep = "/")
        else
            dir <- directory

        # Creation du dossier
        if(! dir.exists(dir))
            dir.create(dir)
        else
            message("Dossier deja existant : tous les fichiers vont etre supprimes")

        # Dossier des CdR
        dir_bilan_fg <- paste(dir, "01_Bilans_FRENCH_GAAP", sep = "/")
        if(! dir.exists(dir_bilan_fg))
            dir.create(dir_bilan_fg)
        else
            for(file in list.files(path = dir_bilan_fg))
                file.remove(paste(dir_bilan_fg, file, sep = "/"))

        # Dossier des CdR
        dir_cdr <- paste(dir, "02_Compte_de_resultats", sep = "/")
        if(! dir.exists(dir_cdr))
            dir.create(dir_cdr)
        else
            for(file in list.files(path = dir_cdr))
                file.remove(paste(dir_cdr, file, sep = "/"))


        # Message
        message(paste("Dossier de sortie :", dir))







        ## ######################################################
        ## ######################################################
        ##
        ##              Ecriture des fichiers
        ##
        ## ######################################################
        ## ######################################################

        # Nombre de simulations
        nb_sim <- length(output@be)

        # Intervalle
        int <- seq(from = 0L, to = nb_sim, by = by)
        if(nb_sim %% by != 0)
            int <- c(int, nb_sim)

        # Test sur la valeur de by
        if(by > 100L)
            stop("Valeur de 'by' trop elevee ! (Indiquez une valeur inferieure a 100)")




        ## ###########################
        ##          Bilans
        ## ###########################
        if(detail){

            # Message
            message("Construction des Bilans")

            # Barre de progression
            barre <- txtProgressBar(min = 0L, max = (length(int) - 1L), style = 3L)

            for(i in 1L:(length(int) - 1L)) {

                # Simulations a traiter
                sim <- (int[i]+1L):(int[i+1L])

                # Construction de la liste des bilans
                list_bilan <- lapply(X = sim, FUN = function(x) bilan_simu_output(output = output, num_simu = x, digits = digits))

                # File
                file <- paste(dir_bilan_fg, paste0("Bilans_FRENCH_GAAP", "_", (int[i]+1L), "-", (int[i+1L]), ".xlsx"), sep = "/")

                # Export resultat
                export_bilan(list_bilans = list_bilan, digits = digits, file = file)

                # Avancement de la barre de progression
                setTxtProgressBar(barre, i)
            }

            # Fermeture de la barre de progression
            close(barre)
        }




        ## ###########################
        ##        Bilan Moyen
        ## ###########################

        # Message
        message("Construction du bilan moyen.")

        # Construction du bilan
        bilan_moyen <- bilan_moyen(output = output, digits = digits)

        # Export vers un fichier excel
        file <- paste(directory, paste0("Bilan_moyen_FRENCH_GAAP.xlsx"), sep = "/")
        export_bilan(list_bilans = list(bilan_moyen), digits = digits, file = file)





        ## ###########################
        ##      Compte des resultats
        ## ###########################

        if(detail){

            # Message
            message("Construction des comptes de resultat")

            # Barre de progression
            barre <- txtProgressBar(min = 0L, max = (length(int) - 1L), style = 3L)

            for(i in 1L:(length(int) - 1L)) {

                # Simulations a traiter
                sim <- (int[i]+1L):(int[i+1L])

                # Construction de la liste des resultats
                list_res <- lapply(X = sim, FUN = function(x) resultat_simu_output(output = output, num_simu = x, digits = digits))

                # File
                file <- paste(dir_cdr, paste0("Resultats", "_", (int[i]+1L), "-", (int[i+1L]),".xlsx"), sep = "/")

                # Export resultat
                export_resultat(list_resultats = list_res, digits = digits, file = file)

                # Avancement de la barre de progression
                setTxtProgressBar(barre, i)
            }

            # Fermeture de la barre de progression
            close(barre)
        }




        ## ###########################
        ##  Compte de resultat moyen
        ## ###########################

        # Message
        message("Construction du compte de resultat moyen.")

        # Construction du resultat
        resultat_moyen <- resultat_moyen(output = output, digits = digits)

        # Export vers un fichier excel
        file <- paste(directory, paste0("Resultat_moyen.xlsx"), sep = "/")
        export_resultat(list_resultats = list(resultat_moyen), digits = digits, file = file)


    }
)
