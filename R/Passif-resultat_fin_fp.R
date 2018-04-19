##' Fonction \code{resultat_fin_fp}
##'
##' Cette fonction permet de determiner le resultat financier des actifs en face les fonds propres.
##'
##' @name resultat_fin_fp
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @param result_fin est un \code{numeric} representant le resultat financier total.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "resultat_fin_fp", def = function(passif, result_fin) {standardGeneric("resultat_fin_fp")})
setMethod(
    f = "resultat_fin_fp",
    signature = c(passif = "Passif", result_fin = "numeric"),
    definition = function(passif, result_fin){


        ## ###########################
        ##      Extraction des PMs
        ## ###########################

        # Determination des PMs totales
        pm <- calcul_pm(passif@ptf_passif)

        # PM totales
        pm <- sum_list(pm, 1L)




        ## ###########################
        ##          Quote-Part
        ## ###########################

        # Total des fonds propres
        fp <- passif@fonds_propres@capital_social + passif@fonds_propres@report_a_nouveau

        warning("Est ce qu'on prend le report a nouveau ?")

        # Quote-Part capitaux propres
        qp_cp <- fp / (fp + pm)




        ## ###########################
        ## Calcul du resulat
        ## ###########################

        # Resultat financiers des actifs en face des capitaux propres
        result_fin_fp <- qp_cp * result_fin

        warning("On fait quoi de ce resultat ?")




        # Output
        return(list(result_fin_fp = result_fin_fp))


    }
)
