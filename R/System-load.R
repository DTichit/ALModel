##' Fonction \code{load_system}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{System}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{System}} necessite la creation de deux autres objets : \code{\link{Actif}} et \code{\link{Passif}}.
##'
##' @name load_system
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{Actif}} : \code{\link{load_actif}}.
##' @seealso Construction d'un objet de type \code{\link{Passif}} : \code{\link{load_passif}}.
##' @export
##' @include System-class.R Actif-class.R Actif-load.R Passif-load.R Passif-class.R
##'
setGeneric(name = "load_system", def = function(address) {standardGeneric("load_system")})
setMethod(
    f = "load_system",
    signature = c(address = "character"),
    definition = function(address){

        # Creation des attributs
        actif   <- load_actif(paste(address, "01_Actifs", sep = "/"))
        passif  <- load_passif(paste(address, "02_Passifs", sep = "/"))
        ppe     <- load_ppe(paste(address, "03_PPE", "PPE.csv", sep = "/"))

        # Creation de l'objet
        system <- new("System",
                      actif = actif, passif = passif, ppe = ppe)

        # Output
        return(system)
    }
)
