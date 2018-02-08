##' Fonction \code{load_hyp_alm}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{HypALM}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{HypALM}} necessite plusieurs parametres presents dans un fichier nomme \code{Hypotheses_ALM.csv}.
##'
##' @name load_hyp_alm
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include HypALM-class.R
##'
setGeneric(name = "load_hyp_alm", def = function(address) {standardGeneric("load_hyp_alm")})
setMethod(
    f = "load_hyp_alm",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(paste(address, "Hypotheses_ALM.csv", sep = "/"), header = TRUE, colClasses = c("integer", "integer"))

        # Lecture des donnes
        nb_simu <- temp[1L, "nb_simu"]
        an_proj <- temp[1L, "an_proj"]

        # Creation de l'objet
        hyp_alm <- new("HypALM", nb_simu = nb_simu, an_proj = an_proj)

        # Output
        return(hyp_alm)
    }
)
