##' Fonction \code{resultat_moyen}.
##'
##' Cette fonction permet de sortir le compte de resultat moyen sur les differentes simulations presentes dans l'objet \code{\link{Output}}
##'
##' @name resultat_moyen
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "resultat_moyen", def = function(output, digits = 2L) {standardGeneric("resultat_moyen")})
setMethod(
    f = "resultat_moyen",
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

        # Construction des comptes de resultats
        cdr <- lapply(1L:nb_simu, function(x) resultat_simu_output(output = output, num_simu = x, digits = digits))

        # Initialisation du bilan moyen
        cdr_mean <- cdr[[1L]]

        # Calcul des moyennes
        for(i in 1L:nrow(cdr_mean))
            for(j in 1L:ncol(cdr_mean))
                cdr_mean[i,j] <- mean(sapply(1L:nb_simu, FUN = function(x) cdr[[x]][i,j]))



        # Output
        return(cdr_mean)
    }
)
