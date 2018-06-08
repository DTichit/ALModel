##' Fonction \code{plot_bilan}.
##'
##' Cette fonction permet d'exporter plusieurs bilans dans des fichiers excel.
##'
##' @name plot_bilan
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param num_simu est un vecteur d'\code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "plot_bilan", def = function(actif, passif, title = "") {standardGeneric("plot_bilan")})
setMethod(
    f = "plot_bilan",
    signature = c(actif = "data.frame", passif = "data.frame"),
    definition = function(actif, passif, title){



        ## ###########################
        ##   Extraction de donnees
        ## ###########################

        # Total
        actif_total     <- sum(actif$Montant)
        passif_total    <- sum(passif$Montant)




        ## ######################################################
        ## ######################################################
        ##
        ##           Construction des elements du bilan
        ##
        ## ######################################################
        ## ######################################################


        plot_actif <- gg_bilan(actif, lim = max(actif_total, passif_total)) +
            labs(title = paste("Actif =", actif_total))

        plot_passif <- gg_bilan(passif, lim = max(actif_total, passif_total)) +
            labs(title = paste("Passif =", passif_total))

        bilan <- gridExtra::grid.arrange(plot_actif, plot_passif, nrow=1, ncol=2)



        # Output
        return(bilan)
    }
)
