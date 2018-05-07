##' Fonction \code{load_epargne}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Epargne}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{Epargne}} necessite des donnees presentes dans un fichier nomme \code{Epargne.csv}.
##'
##' @name load_epargne
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Epargne-class.R
##'
setGeneric(name = "load_epargne", def = function(address) {standardGeneric("load_epargne")})
setMethod(
    f = "load_epargne",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        ptf <- read.csv2(address, header = TRUE,
                         colClasses = c("numeric", "factor", "integer", "integer", "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

        # Creation du dataframe
        df <- data.frame(id_mp = paste("ep", 1L:nrow(ptf), sep = "-"), ptf)

        # Creation de l'objet
        epargne <- new("Epargne", ptf = df)

        # Output
        return(epargne)
    }
)
