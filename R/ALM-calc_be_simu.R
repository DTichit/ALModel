##' Fonction \code{calc_be_simu}.
##'
##' Cette fonction est une fonction centrale du package. Elle permet en effet de calculer un BEL pour une simulation donnee.
##'
##' C'est sur cette fonction que s'effectue les boucles sur le nombre de simulations ainsi que sur les annees.
##'
##' @name calc_be_simu
##' @docType methods
##' @param alm est un objet de type \code{ALM} contenant l'ensemble des donnees.
##' @author Damien Tichit pour Sia Partners
##' @seealso Projection sur une annee d'un \code{\link{System}} : \code{\link{proj_1an_system}}.
##' @export
##' @include System-class.R System-proj_1an.R ALM-class.R
##'
setGeneric(name = "calc_be_simu", def = function(alm){standardGeneric("calc_be_simu")})
setMethod(
    f = "calc_be_simu",
    signature = c(alm = "ALM"),
    definition = function(alm){


        ## ###########################
        ##      Initialisation
        ## ###########################

        # Extraction des donnees
        hyp_alm <- alm@hyp_alm
        system  <- alm@system

        # Initialisation de la liste contenant les flux par annee
        flux_be_simu    <- list()
        flux_simu       <- list()




        ## ###########################
        ## Boucle sur les annnees
        ## ###########################

        for (an in 1L:(hyp_alm@an_proj)) {

            # Projection sur une annee
            res_proj <- proj_1an_system(system = system, an = an)

            # Mise a jour de l'attribut
            system <- res_proj[["system"]]

            # Mise en memoire des flux
            flux_be_simu[[an]]  <- res_proj[["flux_bel"]]
            flux_simu[[an]]     <- res_proj[["flux"]]
        }





        ## ###########################
        ## Aggregation des flux par produit
        ## ###########################

        flux <- sapply(X = names(flux_be_simu[[1L]]),
                       FUN = function(x) {sapply(X = 1L:(hyp_alm@an_proj), FUN = function(y) return(flux_be_simu[[y]][[x]]))},
                       simplify = FALSE, USE.NAMES = TRUE)




        # Output
        return(list(flux = flux,
                    system = system))
    }
)
