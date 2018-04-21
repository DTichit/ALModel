##' Fonction \code{calcul_pt}
##'
##' Cette fonction permet de determiner le montant total des provisions techniques pour une compagnie d'assurance : PM et PPE
##'
##' @name calcul_pt
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PTFPassif-class.R
##'
setGeneric(name = "calcul_pt", def = function(ptf_passif) {standardGeneric("calcul_pt")})
setMethod(
    f = "calcul_pt",
    signature = c(ptf_passif = "PTFPassif"),
    definition = function(ptf_passif){



        ## ######################################################
        ## ######################################################
        ##
        ##           Extraction des differentes provisions
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##   Provisions mathematiques
        ## ###########################

        pm <- calcul_pm(passif@ptf_passif)


        ## ###########################
        ##              PPE
        ## ###########################

        ppe <- sum(passif@provision@ppe@ppe)





        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        pt_tot <- sum_list(pm, 1L) + ppe



        # Output
        return(list(total = pt_tot,
                    pt = list(pm = pm,
                              ppe = ppe)))
    }
)
