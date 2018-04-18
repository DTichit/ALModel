##' Fonction \code{gestion_capitaux_propres}
##'
##' Cette fonction permet de gerer les capitaux propres : ajout d'une part du resultat fin, retirer les dividendes.
##'
##' @name gestion_capitaux_propres
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @param result_fin est un \code{numeric} representant le resultat financier total.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "gestion_capitaux_propres", def = function(passif, result_fin) {standardGeneric("gestion_capitaux_propres")})
setMethod(
    f = "gestion_capitaux_propres",
    signature = c(passif = "Passif", result_fin = "numeric"),
    definition = function(passif, result_fin){


        ## ###########################
        ##      Calcul des PMs
        ## ###########################

        # Determination des PMs totales
        pm <- calcul_pm(passif@ptf_passif)

        # PM totales
        pm <- sum_list(pm, 1L)




        ## ###########################
        ##          Quote-Part
        ## ###########################

        # Quote-Part capitaux propres
        qp_cp <- passif@cap_pro / (passif@cap_pro + pm)




        ## ###########################
        ## Mise a jour des capitaux propres
        ## ###########################

        # Resultat financiers des actifs en face des capitaux propres
        result_fin_cp <- qp_cp * result_fin

        # Ajout d'un part des resultats financiers
        passif@cap_pro <- passif@cap_pro + result_fin_cp




        ## ###########################
        ##          Dividendes
        ## ###########################

        # Calcul des dividendes verses
        dividendes <- passif@cap_pro * passif@hyp_passif@dividende

        # Retirer une partie des capitaux propres
        passif@cap_pro <- passif@cap_pro - dividendes




        # Output
        return(list(passif = passif,
                    result_fin_cp = result_fin_cp,
                    dividendes = dividendes))


    }
)
