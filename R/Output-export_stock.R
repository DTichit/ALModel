##' Fonction \code{export_stock_output}.
##'
##' Cette fonction permet de sortir les elements composant le bilan.
##'
##' @name export_stock_output
##' @docType methods
##' @param output est un objet de type \code{\link{Output}}.
##' @param num_simu est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Output-class.R
##'
setGeneric(name = "export_stock_output", def = function(output, digits = 2L) {standardGeneric("export_stock_output")})
setMethod(
    f = "export_stock_output",
    signature = c(output = "Output"),
    definition = function(output, digits){


        nb_simu <- length(output@stock)

        temp <- lapply(1L:nb_simu, function(x) {

            return(stock_simu(output = output, num_simu = x))
        })



        res <- NULL
        for(x in 1L:nb_simu)
            res <- rbind(res, data.frame(num_simu = x, temp[[x]]))


        return(res)
    }
)
