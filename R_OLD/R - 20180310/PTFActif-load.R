##' Fonction \code{load_ptf_actif}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Actif}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{Actif}} necessite la creation de trois autres objets : \code{\link{Action}}, \code{\link{Obligation}}, \code{\link{Immobilier}} et \code{\link{Tresorerie}}.
##'
##' @name load_ptf_actif
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{Action}} : \code{\link{load_action}}.
##' @seealso Construction d'un objet de type \code{\link{Obligation}} : \code{\link{load_obligation}}.
##' @seealso Construction d'un objet de type \code{\link{Tresorerie}} : \code{\link{load_tresorerie}}.
##' @seealso Construction d'un objet de type \code{\link{Immobilier}} : \code{\link{load_immobilier}}.
##' @export
##' @include Actif-class.R Action-class.R Action-load.R Obligation-class.R Obligation-load.R Tresorerie-class.R Tresorerie-load.R Immobilier-class.R Immobilier-load.R
##'
setGeneric(name = "load_ptf_actif", def = function(address) {standardGeneric("load_ptf_actif")})
setMethod(
    f = "load_ptf_actif",
    signature = c(address = "character"),
    definition = function(address){

        # Creation des attributs
        action      <- load_action(paste(address, "Actions.csv", sep = "/"))
        obligation  <- load_obligation(paste(address, "Obligations.csv", sep = "/"))
        tresorerie  <- load_tresorerie(paste(address, "Tresorerie.csv", sep = "/"))
        immobilier  <- load_immobilier(paste(address, "Immobilier.csv", sep = "/"))

        # Creation de l'objet
        ptf_actif <- new("PTFActif",
                         action = action, obligation = obligation, tresorerie = tresorerie, immobilier = immobilier)

        # Output
        return(ptf_actif)
    }
)
