##' Fonction \code{load_hyp_actif}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{HypActif}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{HypActif}} necessite la creation d'un autre objet : \code{\link{PTFCible}}.
##'
##' @name load_hyp_actif
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{PTFCible}} : \code{\link{load_ptf_cible}}.
##' @export
##' @include HypActif-class.R PTFCible-class.R PTFCible-load.R
##'
setGeneric(name = "load_hyp_actif", def = function(address) {standardGeneric("load_hyp_actif")})
setMethod(
    f = "load_hyp_actif",
    signature = c(address = "character"),
    definition = function(address){

        # Creation des attributs
        ptf_cible      <- load_ptf_cible(paste(address, "01_PortefeuillesCibles", sep = "/"))

        # Lecture de fichiers
        frais_fin <- read.csv2(paste(address, "frais_financiers.csv", sep = "/"))

        # Creation de l'objet
        hyp_actif <- new("HypActif", ptf_cible = ptf_cible, esg_simu = list(), frais_fin = frais_fin)

        # Output
        return(hyp_actif)
    }
)
