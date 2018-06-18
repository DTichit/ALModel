##' Fonction \code{calcul_quote_part_fp}
##'
##' Cette fonction permet de determiner la proportion que represente les fonds propres sur le passif d'une compagnie d'assurance.
##'
##' @name calcul_quote_part_fp
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
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
        pt <- calcul_pt(passif)[["total"]]




        ## ###########################
        ## Extraction des Fonds prpres
        ## ###########################

        # Total des fonds propres
        fp <- calcul_fonds_propres(fp = passif@fonds_propres)[["total"]]




        ## ###########################
        ##          Quote-Part
        ## ###########################

        # Quote-Part capitaux propres
        qp_cp <- fp / (fp + pt)





        # Output
        return(qp_cp)


    }
)
