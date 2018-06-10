##' Fonction \code{bilan_moyen}.
##'
##' Cette fonction permet de sortir le bilan moyen (FRENCH GAAP) sur les differentes simulations presentes dans l'objet \code{\link{Output}}
##'
##' @name bilan_moyen
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "bilan_moyen", def = function(output, digits = 2L) {standardGeneric("bilan_moyen")})
setMethod(
    f = "bilan_moyen",
    signature = c(output = "Output"),
    definition = function(output, digits){



        ## ###########################
        ##  Hypotheses
        ## ###########################

        # Extraction de la donnee
        nb_simu <- length(output@stock)
        nb_annees_proj <- length(output@stock[[1L]])






        ## ######################################################
        ## ######################################################
        ##
        ##                  Bilan moyen
        ##
        ## ######################################################
        ## ######################################################

        # Construction des bilans
        bilans <- lapply(1L:nb_simu, function(x) bilan_simu_output(output = output, num_simu = x, digits = digits))

        # Initialisation du bilan moyen
        bilan_mean <- bilans[[1L]]

        # Calcul des moyennes
        for(i in 1L:nrow(bilan_mean))
            for(j in 1L:ncol(bilan_mean))
                bilan_mean[i,j] <- mean(sapply(1L:nb_simu, FUN = function(x) bilans[[x]][i,j]))



        # Output
        return(bilan_mean)
    }
)
