##' Fonction \code{calc_be}.
##'
##' Cette fonction est une fonction centrale du package. Elle permet en effet de calculer un BEL.
##'
##' Il est possible paralleliser les calculs afin d'accelerer le calcul d'un best-estimate.
##'
##' @name calc_be
##' @docType methods
##' @param alm est un objet de type \code{\link{ALM}} contenant l'ensemble des donnees.
##' @param parallel est une valeur \code{logical}. Lorsque cet argument est a \code{TRUE}, les calculs sont parallelises.
##' @param nb_core est une valeur \code{integer} qui indique le nombre de coeurs utilises lorsque les calculs sont parallelises.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include ALM-class.R
##'
setGeneric(name = "calc_be", def = function(alm, parallel = FALSE, nb_core = 1L){standardGeneric("calc_be")})
setMethod(
    f = "calc_be",
    signature = c(alm = "ALM"),
    definition = function(alm, parallel, nb_core){


        ## ###########################
        ##        Message
        ## ###########################

        start_time <- Sys.time()

        message(paste("Lancement des", alm@hyp_alm@nb_simu, "simulations :", start_time))



        ## ###########################
        ##      Initialisation
        ## ###########################

        # Extraction des donnees
        hyp_alm <- alm@hyp_alm

        # Initialisation de la liste contenant les flux par annee
        flux_bel_simu   <- list()
        flux_nav_simu   <- list()
        stock           <- list()



        ## ###########################
        ## Boucle sur les simulations
        ## ###########################

        if(parallel){

            # Initialisation de la parallelisation
            cluster <- makePSOCKcluster(nb_core)
            registerDoParallel(cluster)

            flux_be <- foreach(i=1L:(hyp_alm@nb_simu), .packages = c("SiALM")) %dopar% {

                # Remise a jour de l'objet
                system  <- alm@system

                # Appel de la fonction pour calculer les flux sur 1 simulation
                be_simu <- calc_be_simu(alm, num_sim = i)[["out"]]

                return(be_simu)
            }


            # Stoppe la parallelisation
            stopCluster(cluster)



        } else {


            # Barre de progression
            barre <- txtProgressBar(min = 0L, max = hyp_alm@nb_simu, style = 3L)

            flux_be <- lapply(1L:(hyp_alm@nb_simu), function(sim) {

                # Remise a jour de l'objet
                system  <- alm@system

                # Appel de la fonction pour calculer les flux sur 1 simulation
                be_simu <- calc_be_simu(alm, num_sim = sim)[["out"]]

                # Avancement de la barre de progression
                setTxtProgressBar(barre, sim)

                # Output
                return(be_simu)
            })

            # Fermeture de la barre de progression
            close(barre)
        }


        message("Fin des simulations")


        ## ###########################
        ##      Calcul du BEL
        ## ###########################


        message("Calcul du BEL et de la NAV")

        # Extraction des flux actualises
        flux_nav_actu <-  sapply(1L:(hyp_alm@nb_simu), function(y) return(flux_be[[y]][["flux_actu"]][["nav"]]))
        flux_bel_actu <-  sapply(1L:(hyp_alm@nb_simu), function(y) return(flux_be[[y]][["flux_actu"]][["bel"]]))

        # Moyenne sur les simulations
        nav <- mean(flux_nav_actu)
        be  <- mean(flux_bel_actu)



        ## ###########################
        ##   Extraction des donnees sauvegardees
        ## ###########################
        stock <- sapply(X = 1L:(hyp_alm@nb_simu), simplify = FALSE, USE.NAMES = TRUE,
                        FUN = function(x) return(flux_be[[x]][["stock"]]))



        ## ###########################
        ## Creation de l'objet 'Output'
        ## ###########################

        output <- new("Output", system = alm@system, stock = stock, be = flux_bel_actu, nav = flux_nav_actu)



        diff_time <- round(difftime(Sys.time(), start_time, units = "min"), digits = 2L)

        message(paste("Calcul du BE effectue en :", diff_time, "minutes."))




        # Output
        return(list(be = be,
                    nav = nav,
                    output = output))
    }
)
