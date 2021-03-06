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
        flux_bel_simu   <- list()
        flux_nav_simu   <- list()
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
            flux_bel_simu[[an]]  <- res_proj[["flux"]][["bel"]]
            flux_nav_simu[[an]] <- res_proj[["flux"]][["nav"]]
            stock_simu[[an]]    <- res_proj[["stock"]]
        }



        ## ###########################
        ##  Gestion fin de projection
        ## ###########################

        # Appel de la fonction
        res_fin_proj <- gestion_fin_projection_system(system = system)

        # Mise a jour de l'attribut
        system <- res_fin_proj[["system"]]




        ## ###########################
        ## Aggregation des flux par produit
        ## ###########################

        # Extraction des donnees de projection
        flux_bel <- unlist(flux_bel_simu)
        flux_nav <- unlist(flux_nav_simu)

        # Ajout des donnees de fin de projection
        flux_bel[an_proj] <- flux_bel[an_proj] + res_fin_proj[["fin_projection"]][["assures"]]
        flux_nav[an_proj] <- flux_nav[an_proj] + res_fin_proj[["fin_projection"]][["assureurs"]]





        ## ###########################
        ##   Actualisation des flux
        ## ###########################

        # Recuperation des taux sans risque
        coef_actu <- res_esg[["coef_actu"]][ProjYr <= an_proj, CoefActu][1L:length(flux_bel)]

        # Actualisation
        flux_bel_actu <- sum(flux_bel * coef_actu)
        flux_nav_actu <- sum(flux_nav * coef_actu)





        # Output
        return(list(out = list(flux_actu = list(bel = flux_bel_actu,
                                                nav = flux_nav_actu),
                               stock = stock_simu,
                               fin_projection = res_fin_proj[["fin_projection"]]),
                    system = system))
    }
)
