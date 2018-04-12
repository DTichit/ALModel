##' Fonction \code{proj_1an_passif}
##'
##' Cette fonction permet de projeter horizon 1 an le passif d'une compagnie d'assurance.
##'
##' @name proj_1an_passif
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @param an est un objet de type \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "proj_1an_passif", def = function(passif, an) {standardGeneric("proj_1an_passif")})
setMethod(
    f = "proj_1an_passif",
    signature = c(passif = "Passif", an = "integer"),
    definition = function(passif, an){



        ## ######################################################
        ## ######################################################
        ##
        ##           Calcul des probas, le cas echeant
        ##
        ## ######################################################
        ## ######################################################

        if(passif@hyp_passif@calc_proba) {

            # Appel de la fonction
            res_calc_proba <- calc_proba_ptf_passif(ptf_passif = passif@ptf_passif, hyp_passif = passif@hyp_passif, an = an)

            # Mise a jour de l'attribut
            passif@ptf_passif <- res_calc_proba[["ptf_passif"]]

        }



        ## ######################################################
        ## ######################################################
        ##
        ## Gestion du portfeuille : Prestation, revalorisation aux TMG...
        ##
        ## ######################################################
        ## ######################################################

        # Projection sur une annee du portfeuille
        res_proj_ptf <- proj_1an_ptf_passif(ptf_passif = passif@ptf_passif, hyp_passif = passif@hyp_passif, an = an)

        # Mise a jour de l'attribut
        passif@ptf_passif <- res_proj_ptf[["ptf_passif"]]





        # Output
        return(list(passif = passif,
                    pm_ouverture = res_proj_ptf[["pm_ouverture"]],
                    flux = res_proj_ptf[["flux"]],
                    besoin = res_proj_ptf[["besoin"]]))
    }
)
