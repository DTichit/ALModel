##' Fonction \code{load_action_cible}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Action}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}. Cette fonction sera utilisee pour le chargement du portfeuille cible.
##'
##' La creation d'un objet \code{\link{Action}} necessite des donnees presentes dans un fichier nomme \code{Actions.csv}.
##'
##' @name load_action_cible
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Action-class.R
##'
setGeneric(name = "load_action_cible", def = function(address) {standardGeneric("load_action_cible")})
setMethod(
    f = "load_action_cible",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE,
                          colClasses = c("numeric", "numeric", "numeric"))

        # Test sur la somme des proportions
        if(sum(temp$prop) != 1)
            stop("[load_action_cible] : Somme des proportions differente de 1.")

        # Creation du dataframe
        df <- data.frame(id_mp = paste("ac", "cib", 1L:nrow(temp), sep = "-"), temp)

        # Creation de l'objet
        action <- new("Action", ptf = df)

        # Output
        return(action)
    }
)
