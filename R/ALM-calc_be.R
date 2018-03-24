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


        ## ###########################
        ##      Initialisation
        ## ###########################

        # Extraction des donnees
        hyp_alm <- alm@hyp_alm

        # Initialisation de la liste contenant les flux par annee
        flux_be_simu    <- list()
        flux_simu       <- list()



        ## ###########################
        ## Boucle sur les simulations
        ## ###########################

        flux_be <- lapply(1L:(hyp_alm@nb_simu), function(sim) {

            # Remise a jour de l'objet
            system  <- alm@system

            # Boucle sur le nombre d'annees
            for (an in 1L:(hyp_alm@an_proj)) {

                # Projection sur une annee
                res_proj <- proj_1an_system(system = system, an = an)

                # Mise a jour de l'attribut
                system <- res_proj[["system"]]

                # Mise en memoire des flux
                flux_be_simu[[an]]  <- res_proj[["flux_bel"]]
                flux_simu[[an]]     <- res_proj[["flux"]]
            }

            # Aggregation des flux par produit
            temp <- sapply(X = names(flux_be_simu[[1L]]),
                           FUN = function(x) {sapply(X = 1L:(hyp_alm@an_proj), FUN = function(y) return(flux_be_simu[[y]][[x]]))},
                           simplify = FALSE, USE.NAMES = TRUE)

            # Output
            return(temp)
        })


        ## ###########################
        ##   Actualisation des flux
        ## ###########################

        # Recuperation des taux sans risque
        tsr <- hyp_alm@tsr[1L:hyp_alm@an_proj]

        # Actualisation
        flux_actu <- sapply(X = names(flux_be[[1L]]), simplify = FALSE, USE.NAMES = TRUE ,
                            FUN = function(x) {

                                sapply(X = 1L:(hyp_alm@an_proj), FUN = function(y) {

                                    # Actualisation des flux
                                    flux_actu_simu <- flux_be[[y]][[x]] * ((1 + tsr)^(-(1L:(hyp_alm@an_proj))))

                                    # Somme des flux
                                    return(sum(flux_actu_simu))})
                            })



        ## ###########################
        ##      Calcul du BEL
        ## ###########################

        # Moyenne sur les simulations
        be <- sapply(X = names(flux_be[[1L]]), function(x) mean(flux_actu[[x]]))



        # Output
        return(list(be = list(be = be,
                              flux_actu = flux_actu),
                    flux = 0L))
    }
)
