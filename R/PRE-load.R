##' Fonction \code{load_pre}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{PRE}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{PRE}} necessite des donnees presentes dans les fichiers nommes \code{pre.csv}.
##'
##' @name load_pre
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PRE-class.R
##'
setGeneric(name = "load_pre", def = function(address) {standardGeneric("load_pre")})
setMethod(
    f = "load_pre",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE, colClasses = c("numeric"))

        # Creation de l'objet
        pre <- new("PRE", montant = temp[,"montant"])

        # Output
        return(pre)
    }
)
