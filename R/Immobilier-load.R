##' Fonction \code{load_immobilier}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Immobilier}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{Immobilier}} necessite des donnees presentes dans un fichier nomme \code{Immobilier.csv}.
##'
##' @name load_immobilier
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R
##'
setGeneric(name = "load_immobilier", def = function(address) {standardGeneric("load_immobilier")})
setMethod(
    f = "load_immobilier",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE,
                          colClasses = c("numeric", "numeric"))

        # Creation du dataframe
        df <- data.frame(id_mp = paste("im", 1L:nrow(temp), sep = "-"), temp)

        # Creation de l'objet
        immo <- new("Immobilier", ptf = df)

        # Output
        return(immo)
    }
)
