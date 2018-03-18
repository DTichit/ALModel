##' Fonction \code{load_tab_rachat}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{TabRachat}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{TabRachat}} necessite des donnees presentes dans un fichier nomme \code{Rachat.csv}.
##'
##' @name load_tab_rachat
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include TabRachat-class.R
##'
setGeneric(name = "load_tab_rachat", def = function(address) {standardGeneric("load_tab_rachat")})
setMethod(
    f = "load_tab_rachat",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE, colClasses = c("integer", "numeric"))

        # Creation de l'objet
        tab_rachat <- new("TabRachat", table = temp)

        # Output
        return(tab_rachat)
    }
)
