##' Fonction \code{load_ppe}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{PPE}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{PPE}} necessite des donnees presentes dans les fichiers nommes \code{PPE.csv} et \code{TM-H.csv}.
##'
##' @name load_ppe
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PPE-class.R
##'
setGeneric(name = "load_ppe", def = function(address) {standardGeneric("load_ppe")})
setMethod(
    f = "load_ppe",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE, colClasses = c("numeric"))

        # Creation de l'objet
        ppe <- new("PPE", ppe = temp[,"ppe"])

        # Output
        return(ppe)
    }
)
