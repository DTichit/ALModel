##' Fonction \code{load_immobilier_cible}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Immobilier}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{Immobilier}} necessite des donnees presentes dans un fichier nomme \code{Immobilier.csv}.
##'
##' @name load_immobilier_cible
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R
##'
setGeneric(name = "load_immobilier_cible", def = function(address) {standardGeneric("load_immobilier_cible")})
setMethod(
    f = "load_immobilier_cible",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE,
                          colClasses = c("numeric", "numeric", "numeric"))

        # Test sur la somme des proportions
        if(sum(temp$prop) != 1)
            stop("[load_immobilier_cible] : Somme des proportions differente de 1.")

        # Creation du dataframe
        df <- data.frame(id_mp = paste("im", "cib", 1L:nrow(temp), sep = "-"), temp)

        # Creation de l'objet
        immo <- new("Immobilier", ptf = df)

        # Output
        return(immo)
    }
)
