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

        mg_souscription <- resultat[["charges_pm"]] + resultat[["prime"]] - resultat[["chgt"]][["acquisition"]] -
            resultat[["prestation"]] - resultat[["chgt"]][["administration"]] + resultat[["revalo_pm"]]



        ## ###########################
        ##      Marge de gestion
        ## ###########################

        mg_gestion <- resultat[["chgt"]][["administration"]] + resultat[["chgt"]][["acquisition"]] - resultat[["frais"]]



        ## ###########################
        ##      Marge financiere
        ## ###########################

        mg_financiere <- resultat[["resultat_fin"]] - resultat[["charges_rc"]] - resultat[["charges_ppe"]] - resultat[["charges_pre"]] - resultat[["revalo_pm"]]



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
