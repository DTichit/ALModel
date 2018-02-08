##' Fonction \code{load_actif}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Actif}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{Actif}} necessite la creation d'e deux autres objets'un autre objet : \code{\link{PTFActif}}.
##'
##' @name load_actif
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{PTFActif}} : \code{\link{load_ptf_actif}}.
##' @export
##' @include PTFActif-class.R PTFActif-load.R Actif-class.R
##'
setGeneric(name = "load_actif", def = function(address) {standardGeneric("load_actif")})
setMethod(
    f = "load_actif",
    signature = c(address = "character"),
    definition = function(address){

        # Creation des attributs
        ptf_actif      <- load_ptf_actif(paste(address, "01_Portefeuilles", sep = "/"))

        # Creation de l'objet
        actif <- new("Actif", ptf_actif = ptf_actif)

        # Output
        return(actif)
    }
)
