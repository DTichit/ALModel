##' Fonction \code{calc_be_simu}.
##'
##' Cette fonction est une fonction centrale du package. Elle permet en effet de calculer un BEL pour une simulation donnee.
##'
##' C'est sur cette fonction que s'effectue les boucles sur le nombre de simulations ainsi que sur les annees.
##'
##' @name calc_be_simu
##' @docType methods
##' @param alm est un objet de type \code{ALM} contenant l'ensemble des donnees.
##' @param num_sim est un \code{integer} representant le numero de simulation sur lequel on travaille.
##' @author Damien Tichit pour Sia Partners
##' @seealso Projection sur une annee d'un \code{\link{System}} : \code{\link{proj_1an_system}}.
##' @export
##' @include System-class.R System-proj_1an.R ALM-class.R
##'
setGeneric(name = "calc_be_simu", def = function(alm, num_sim){standardGeneric("calc_be_simu")})
setMethod(
    f = "calc_be_simu",
    signature = c(alm = "ALM", num_sim = "integer"),
    definition = function(alm, num_sim){


        ## ###########################
        ## Mise a jour des donnees ESG
        ## ###########################

        # Appel de la fonction
        res_esg <- update_esg(alm = alm, num_sim = num_sim)

        # Mise a jour de l'objet ALM
        alm <- res_esg[["alm"]]


        ## ###########################
        ##      Initialisation
        ## ###########################

        # Extraction des donnees
        an_proj <- alm@hyp_alm@an_proj
        system  <- alm@system

        # Initialisation de la liste contenant les flux par annee
        flux_be_simu    <- list()
        stock_simu      <- list()




        ## ###########################
        ## Boucle sur les annnees
        ## ###########################

        for (an in 1L:(an_proj)) {

            # Projection sur une annee
            res_proj <- proj_1an_system(system = system, an = an)

            # Mise a jour de l'attribut
            system <- res_proj[["system"]]

            # Mise en memoire des flux
            flux_be_simu[[an]]  <- res_proj[["flux_bel"]]
            stock_simu[[an]]    <- res_proj[["stock"]]
        }





        ## ###########################
        ## Aggregation des flux par produit
        ## ###########################

        flux <- sapply(X = names(flux_be_simu[[1L]]),
                       FUN = function(x) {sapply(X = 1L:(an_proj), FUN = function(y) return(flux_be_simu[[y]][[x]]))},
                       simplify = FALSE, USE.NAMES = TRUE)




        ## ###########################
        ##   Actualisation des flux
        ## ###########################

        # Recuperation des taux sans risque
        coef_actu <- res_esg[["coef_actu"]][ProjYr <= an_proj, CoefActu]

        # Actualisation
        flux_actu <- sapply(X = names(flux), simplify = FALSE, USE.NAMES = TRUE ,
                            FUN = function(x) return(sum(flux[[x]] * coef_actu)))

        warning("Demander comment est calcule le coef actu afin de savoir comment il fonctionne pour actualiser ???")





        # Output
        return(list(out = list(flux_actu = flux_actu,
                               stock = stock_simu),
                    system = system))
    }
)
