##' Fonction \code{calc_be}.
##'
##' Cette fonction est une fonction centrale du package. Elle permet en effet de calculer un BEL.
##'
##' C'est sur cette fonction que s'effectue les boucles sur le nombre de simulations ainsi que sur les annees.
##'
##' @name calc_be
##' @docType methods
##' @param alm est un objet de type \code{ALM} contenant l'ensemble des donnees.
##' @author Damien Tichit pour Sia Partners
##' @seealso Projection sur une annee d'un \code{\link{System}} : \code{\link{proj_1an_system}}.
##' @export
##' @include System-class.R System-proj_1an.R ALM-class.R
##'
setGeneric(name = "calc_be", def = function(alm) {standardGeneric("calc_be")})
setMethod(
    f = "calc_be",
    signature = c(alm = "ALM"),
    definition = function(alm){

        # Extraction des donnees
        hyp_alm <- alm@hyp_alm
        system  <- alm@system

        # Boucle sur le nombre de simulation
        for (sim in 1L:(hyp_alm@nb_simu)) {

            # Boucle sur le nombre d'annees
            for (an in 1L:(hyp_alm@an_proj)) {

                # Projection sur une annee
                res_proj <- proj_1an_system(system = system, an = an)

                # Mise a jour de l'attribut
                system <- res_proj[["system"]]

            }
        }


        # Output
        return(NULL)
    }
)
