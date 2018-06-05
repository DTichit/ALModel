##' Fonction \code{calcul_resultat}.
##'
##' Cette fonction permet de calculer le resultat de l'exercice en cours pour une compagnie d'assurance.
##'
##' @name calcul_resultat
##' @docType methods
##' @param resultat est une \code{list} contenant les differents elements du resultat.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include
##'
setGeneric(name = "calcul_resultat", def = function(resultat) {standardGeneric("calcul_resultat")})
setMethod(
    f = "calcul_resultat",
    signature = c(resultat = "list"),
    definition = function(resultat) {


        ## ######################################################
        ## ######################################################
        ##
        ##             Calcul des differentes marges
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##    Marge de souscription
        ## ###########################

        # Extraction des donnees
        charges_pm <- sum_list(resultat[["charges_pm"]], 1L)
        prime <- sum_list(resultat[["prime"]], 1L)
        chgt_acquisition <- sum_list(resultat[["chgt"]][["acquisition"]], 1L)
        chgt_administration <- sum_list(resultat[["chgt"]][["administration"]], 1L)
        prestation <- sum_list(resultat[["prestation"]], 2L)
        revalo_pm <- sum_list(resultat[["revalo_pm"]][["tmg"]], 1L) + sum_list(resultat[["revalo_pm"]][["pb"]], 2L)

        # Calcul de la marge de souscription
        mg_souscription <- charges_pm + (prime - chgt_acquisition) - prestation - chgt_administration + revalo_pm




        ## ###########################
        ##      Marge de gestion
        ## ###########################

        # Extraction des donnees
        frais <- sum_list(resultat[["frais"]], 2L)

        # Calcul de la marge de gestion
        mg_gestion <- chgt_administration + chgt_acquisition - frais



        ## ###########################
        ##      Marge financiere
        ## ###########################

        # Extraction des donnees
        resultat_fin <- calcul_resultat_fin(resultat[["resultat_fin"]])
        revalo_prest <- sum_list(resultat[["revalo_prest"]], 1L)

        # Calcul de la marge financiere
        mg_financiere <- resultat_fin - resultat[["charges_rc"]] - resultat[["charges_ppe"]] - resultat[["charges_pre"]] - revalo_pm - revalo_prest



        ## ###########################
        ##    Resultat financier FP
        ## ###########################

        # res_fin_fp <- resultat[["res_fin_fp"]]






        ## ######################################################
        ## ######################################################
        ##
        ##                  Calcul du resultat
        ##
        ## ######################################################
        ## ######################################################

        # Calcul du resultat
        resultat <- mg_souscription + mg_gestion + mg_financiere









        # Output
        return(list(resultat = resultat,
                    marges = list(souscription = mg_souscription,
                                  gestion = mg_gestion,
                                  financiere = mg_financiere
                                  #, res_fin_fp = res_fin_fp
                                  )))
    }
)
