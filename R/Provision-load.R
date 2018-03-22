##' Fonction \code{load_provision}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Provision}}.
##' Les donnees auront ete prealablement disposees dans une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{Provision}} necessite la creation de deux autres objets : \code{\link{PPE}} et \code{\link{ReserveCapi}}.
##'
##' @name load_provision
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{PPE}} : \code{\link{load_ppe}}.
##' @seealso Construction d'un objet de type \code{\link{ReserveCapi}} : \code{\link{load_reserve_capi}}.
##' @export
##' @include Provision-class.R ReserveCapi-load.R ReserveCapi-class.R PPE-load.R PPE-class.R
##'
setGeneric(name = "load_provision", def = function(address) {standardGeneric("load_provision")})
setMethod(
    f = "load_provision",
    signature = c(address = "character"),
    definition = function(address){

        # Creation des attributs
        ppe             <- load_ppe(paste(address, "01_PPE", "ppe.csv", sep = "/"))
        reserve_capi    <- load_reserve_capi(paste(address, "02_ReserveCapi", "reserve_capi.csv", sep = "/"))

        # Creation de l'objet
        provision <- new("Provision",
                         ppe = ppe, reserve_capi = reserve_capi)

        # Output
        return(provision)
    }
)
