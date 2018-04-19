##' Fonction \code{load_fonds_propres}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{FondsPropres}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' @name load_fonds_propres
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include FondsPropres-class.R
##'
setGeneric(name = "load_fonds_propres", def = function(address) {standardGeneric("load_fonds_propres")})
setMethod(
    f = "load_fonds_propres",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier csv
        temp <- read.csv2(paste(address, "fonds_propres.csv", sep = "/"), header = FALSE)

        # Creation de l'objet
        fp <- new("FondsPropres", capital_social = temp[1L, 2L], report_a_nouveau = temp[2L, 2L],
                  resultat_exercice = 0)

        # Output
        return(fp)
    }
)
