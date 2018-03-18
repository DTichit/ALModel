##' Fonction \code{load_obligation}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Obligation}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{Obligation}} necessite des donnees presentes dans un fichier nomme \code{Obligations.csv}.
##'
##' Cette fonction permet egalement d'aggreger les donnees : 1 obligation par maturite residuelle.
##'
##' @name load_obligation
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "load_obligation", def = function(address) {standardGeneric("load_obligation")})
setMethod(
    f = "load_obligation",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE,
                          colClasses = c("numeric", "numeric", "numeric", "numeric", "integer", "factor", "factor"))

        # Creation du dataframe
        df <- data.frame(id_mp = paste("ob", 1L:nrow(temp), sep = "-"), temp)

        # Creation de l'objet
        obligation <- new("Obligation", ptf = df)

        # Output
        return(obligation)
    }
)
