##' Fonction \code{load_alm}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{ALM}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{ALM}} necessite la creation de deux autres objets : \code{\link{System}} et \code{\link{HypALM}}.
##'
##' @name load_alm
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaire
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{System}} : \code{\link{load_system}}.
##' @seealso Construction d'un objet de type \code{\link{HypALM}} : \code{\link{load_hyp_alm}}.
##' @export
##' @include System-class.R HypALM-class.R System-load.R HypALM-load.R ALM-class.R
##'
setGeneric(name = "load_alm", def = function(address) {standardGeneric("load_alm")})
setMethod(
    f = "load_alm",
    signature = c(address = "character"),
    definition = function(address){

        # Creation des attributs
        system  <- load_system(paste(address, "01_System", sep = "/"))
        hyp_alm <- load_hyp_alm(paste(address, "02_Hypotheses", sep = "/"))

        # Creation de l'objet
        system <- new("ALM",
                      system = system, hyp_alm = hyp_alm)

        # Output
        return(system)
    }
)
