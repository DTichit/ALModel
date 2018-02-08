##' Fonction \code{load_ptf_passif}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{PTFPassif}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{PTFPassif}} necessite la creation d'un autre objet : \code{\link{Epargne}}.
##'
##' @name load_ptf_passif
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{Epargne}} : \code{\link{load_epargne}}.
##' @export
##' @include Epargne-class.R Epargne-load.R PTFPassif-class.R
##'
setGeneric(name = "load_ptf_passif", def = function(address) {standardGeneric("load_ptf_passif")})
setMethod(
    f = "load_ptf_passif",
    signature = c(address = "character"),
    definition = function(address){

        # Creation des attributs
        epargne     <- load_epargne(paste(address, "Epargne.csv", sep = "/"))

        # Creation de l'objet
        ptf_passif <- new("PTFPassif",  epargne = epargne)

        # Output
        return(ptf_passif)
    }
)
