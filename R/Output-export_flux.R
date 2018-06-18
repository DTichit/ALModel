##' Fonction \code{export_flux_output}.
##'
##' Cette fonction permet de sortir les elements composant le bilan.
##'
##' @name export_flux_output
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param num_simu est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "export_flux_output", def = function(output, digits = 2L) {standardGeneric("export_flux_output")})
setMethod(
    f = "export_flux_output",
    signature = c(output = "Output"),
    definition = function(output, digits){


        nb_simu <- length(output@stock)

        temp <- lapply(1L:nb_simu, function(x) {

            return(flux_simu(output = output, num_simu = x, digits = digits))
        })



        res <- NULL
        for(x in 1L:nb_simu)
            res <- rbind(res, data.frame(num_simu = x, temp[[x]]))


        return(res)
    }
)
