##' Fonction \code{resultat_simu_output}.
##'
##' Cette fonction permet de sortir les comptes de resultat sur les differentes annees de projection pour une simulation donnee.
##'
##' @name resultat_simu_output
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param num_simu est un \code{integer}.
##' @param digits est un \code{integer} indiquant le nombre de chiffres significatifs que l'on souhaite avoir dans le CdR. Par defaut, sa valeur est egale a 2.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "resultat_simu_output", def = function(output, num_simu, digits = 2L) {standardGeneric("resultat_simu_output")})
setMethod(
    f = "resultat_simu_output",
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
        ##              Construction des resultats
        ##
        ## ######################################################
        ## ######################################################

        resultat <- sapply(1L:nb_annees_proj, function(x) {

            # Extraction des elements composant le resultat de l'annee
            resultat_proj <- output@stock[[num_simu]][[x]]$flux


            ## ###########################
            ##    Marge de souscription
            ## ###########################

            # Extraction des donnees
            charges_pm <- sum_list(resultat_proj[["charges_pm"]], 1L)
            prime <- sum_list(resultat_proj[["prime"]], 1L)
            chgt_acquisition <- sum_list(resultat_proj[["chgt"]][["acquisition"]], 1L)
            chgt_administration <- sum_list(resultat_proj[["chgt"]][["administration"]], 1L)
            prestation <- sum_list(resultat_proj[["prestation"]], 2L)
            revalo_pm <- sum_list(resultat_proj[["revalo_pm"]][["tmg"]], 1L) + sum_list(resultat_proj[["revalo_pm"]][["pb"]], 2L)

            # Calcul de la marge de souscription
            mg_souscription <- charges_pm + prime - chgt_acquisition - prestation - chgt_administration + revalo_pm




            ## ###########################
            ##      Marge de gestion
            ## ###########################

            # Extraction des donnees
            frais <- sum_list(resultat_proj[["frais"]], 2L)

            # Calcul de la marge de gestion
            mg_gestion <- chgt_administration + chgt_acquisition - frais



            ## ###########################
            ##      Marge financiere
            ## ###########################

            # Extraction des donnees
            resultat_fin <- calcul_resultat_fin(resultat_proj[["resultat_fin"]])
            revalo_prest <- sum_list(resultat_proj[["revalo_prest"]], 1L)

            # Calcul de la marge financiere
            mg_financiere <- resultat_fin - resultat_proj[["charges_rc"]] - resultat_proj[["charges_ppe"]] - resultat_proj[["charges_pre"]] - revalo_pm - revalo_prest



            ## ###########################
            ##          IS et PS
            ## ###########################

            # Extraction de donnees
            ps <- resultat_proj[["participation_salaries"]]
            is <- resultat_proj[["impots_societes"]]


            # Resultat
            res <- mg_souscription + mg_gestion + mg_financiere - is - ps


            c(Charges_PM = charges_pm,
              Primes = prime - chgt_acquisition,
              Prestations = prestation,
              TMG_et_PB_incorpores_a_la_PM_nets_chgt = revalo_pm - chgt_administration,
              Marge_Souscription = mg_souscription,

              Chargements = chgt_administration + chgt_acquisition,
              Frais = frais,
              Marge_de_Gestion = mg_gestion,

              Revenus_financiers = resultat_fin,
              Charges_PPE = resultat_proj$charges_ppe,
              Charges_RC = resultat_proj$charges_rc,
              Charges_PRE = resultat_proj$charges_pre,
              TMG_et_PB_dont_prestations =  -(revalo_pm + revalo_prest),
              Marge_Financiere = mg_financiere,

              Impots_societes = is,
              Participation_salaries = ps,
              Resultat = output@stock[[num_simu]][[x]]$fonds_propres@resultat_exercice
            )

        })

        # Arrondir les donnees
        resultat <- round(resultat, digits = digits)

        # Renommage des colonnes
        colnames(resultat) <- 1L:nb_annees_proj


        # Output
        return(resultat)
    }
)
