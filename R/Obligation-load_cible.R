##' Fonction \code{load_obligation_cible}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Obligation}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}. Cette fonction sera utilisee pour le chargement du portfeuille cible.
##'
##' La creation d'un objet \code{\link{Obligation}} necessite des donnees presentes dans un fichier nomme \code{Obligations.csv}.
##'
##' @name load_obligation_cible
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "load_obligation_cible", def = function(address) {standardGeneric("load_obligation_cible")})
setMethod(
    f = "load_obligation_cible",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE,
                          colClasses = c("numeric", "numeric", "numeric", "integer", "numeric"))

        # Test sur la somme des proportions
        if(sum(temp$prop) != 1)
            stop("[load_obligation_cible] : Somme des proportions differente de 1.")

        # Creation du dataframe
        df <- data.frame(id_mp = paste("ob", "cib", 1L:nrow(temp), sep = "-"), temp, cible = TRUE)

        # Creation de l'objet
        obligation <- new("Obligation", ptf = df)

        # Output
        return(obligation)
    }
)
