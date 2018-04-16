##' Fonction \code{load_tresorerie}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Tresorerie}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{Tresorerie}} necessite des donnees presentes dans un fichier nomme \code{Tresorerie.csv}.
##'
##' @name load_tresorerie
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Tresorerie-class.R
##'
setGeneric(name = "load_tresorerie", def = function(address) {standardGeneric("load_tresorerie")})
setMethod(
    f = "load_tresorerie",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE,
                          colClasses = c("numeric"))

        # Creation de l'objet
        tresorerie <- new("Tresorerie", solde = temp[["solde"]])

        # Output
        return(tresorerie)
    }
)
