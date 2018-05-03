##' Fonction \code{calcul_quote_part_fp}
##'
##' Cette fonction permet de determiner la proportion que represente les fonds propres sur le passif d'une compagnie d'assurance.
##'
##' @name calcul_quote_part_fp
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @param result_fin est un \code{numeric} representant le resultat financier total.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "calcul_quote_part_fp", def = function(passif) {standardGeneric("calcul_quote_part_fp")})
setMethod(
    f = "calcul_quote_part_fp",
    signature = c(passif = "Passif"),
    definition = function(passif){


        ## ###########################
        ##      Extraction des PT
        ## ###########################

        # Determination des PMs totales
        pm <- calcul_pm(passif@ptf_passif)

        # PM totales
        pm <- sum_list(pm, 1L)

        # PPE totale
        ppe <- sum(passif@provision@ppe@ppe)




        ## ###########################
        ##          Quote-Part
        ## ###########################

        # Total des fonds propres
        fp <- passif@fonds_propres@capital_social + passif@fonds_propres@report_a_nouveau

        # Quote-Part capitaux propres
        qp_cp <- fp / (fp + pm + ppe)





        # Output
        return(qp_cp)


    }
)
