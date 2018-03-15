##' Fonction \code{load_action}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Action}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{Action}} necessite des donnees presentes dans un fichier nomme \code{Actions.csv}.
##'
##' @name load_action
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Action-class.R
##'
setGeneric(name = "load_action", def = function(address) {standardGeneric("load_action")})
setMethod(
    f = "load_action",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE,
                          colClasses = c("numeric", "numeric", "numeric"))

        # Creation du dataframe
        df <- data.frame(id_mp = paste("ac", 1L:nrow(temp), sep = "-"), temp, cible = FALSE)

        # Creation de l'objet
        action <- new("Action", ptf = df)

        # Output
        return(action)
    }
)
