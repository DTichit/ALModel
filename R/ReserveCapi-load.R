##' Fonction \code{load_reserve_capi}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{ReserveCapi}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{ReserveCapi}} necessite des donnees presentes dans les fichiers nommes \code{reserve_capi.csv}.
##'
##' @name load_reserve_capi
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include ReserveCapi-class.R
##'
setGeneric(name = "load_reserve_capi", def = function(address) {standardGeneric("load_reserve_capi")})
setMethod(
    f = "load_reserve_capi",
    signature = c(address = "character"),
    definition = function(address){

        # Lecture du fichier
        temp <- read.csv2(address, header = TRUE, colClasses = c("numeric"))

        # Creation de l'objet
        reserve_capi <- new("ReserveCapi", montant = temp[,"montant"])

        # Output
        return(reserve_capi)
    }
)
