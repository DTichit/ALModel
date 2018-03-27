##' Fonction \code{load_rachat_conj}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{RachatConj}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{RachatConj}} necessite des donnees presentes dans un fichier nommes \code{Rachats_conjoncturels.csv}.
##'
##' @name load_rachat_conj
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include RachatConj-class.R
##'
setGeneric(name = "load_rachat_conj", def = function(address) {standardGeneric("load_rachat_conj")})
setMethod(
    f = "load_rachat_conj",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE,
                          colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

        # Creation de l'objet
        rachat_conj <- new("RachatConj", alpha = temp[,"alpha"], beta = temp[,"beta"], gamma = temp[,"gamma"],
                           delta = temp[,"delta"], RCmin = temp[,"RCmin"], RCmax = temp[,"RCmax"])

        # Output
        return(rachat_conj)
    }
)
