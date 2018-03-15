##' Fonction \code{load_tab_morta}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{TabMorta}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{TabMorta}} necessite des donnees presentes dans les fichiers nommes \code{TM-F.csv} et \code{TM-H.csv}.
##'
##' @name load_tab_morta
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include TabMorta-class.R
##'
setGeneric(name = "load_tab_morta", def = function(address) {standardGeneric("load_tab_morta")})
setMethod(
    f = "load_tab_morta",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE, colClasses = c("integer", "integer"))

        # Creation de l'objet
        tab_morta <- new("TabMorta", table = temp)

        # Output
        return(tab_morta)
    }
)
