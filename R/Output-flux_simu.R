##' Fonction \code{flux_simu}.
##'
##' Cette fonction permet de sortir les flux composant le compte de resultat sur les differentes annees de projection et pour une simulation donnee.
##'
##' @name flux_simu
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param num_simu est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "flux_simu", def = function(output, num_simu, digits = 2L) {standardGeneric("flux_simu")})
setMethod(
    f = "flux_simu",
    signature = c(output = "Output", num_simu = "integer"),
    definition = function(output, num_simu, digits){



        ## ###########################
        ## Nombre d'annees de projection
        ## ###########################

        # Extraction de la donnee
        nb_annees_proj <- length(output@stock[[num_simu]])






        ## ######################################################
        ## ######################################################
        ##
        ##              Construction du data frame
        ##
        ## ######################################################
        ## ######################################################

        flux <- sapply(1L:nb_annees_proj, function(x) {

            # Output
            return(unlist(output@stock[[num_simu]][[x]]$flux))

        })

        # Arrondir les donnees
        flux <- round(flux, digits = digits)

        # Renommage des colonnes
        colnames(flux) <-  1L:nb_annees_proj

        # Passage en format long
        flux <- melt(flux)

        # Renommage des colonnes
        colnames(flux) <-  c("Element", "annee", "Montant")



        # Output
        return(flux)
    }
)
