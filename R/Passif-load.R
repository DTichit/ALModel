##' Fonction \code{load_passif}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Passif}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{Passif}} necessite la creation de deux autres objets : \code{\link{Passif}}, \code{\link{Provision}} et \code{\link{HypPassif}}.
##'
##' @name load_passif
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{PTFPassif}} : \code{\link{load_ptf_passif}}.
##' @seealso Construction d'un objet de type \code{\link{HypPassif}} : \code{\link{load_hyp_passif}}.
##' @seealso Construction d'un objet de type \code{\link{Provision}} : \code{\link{load_provision}}.
##' @export
##' @include Passif-class.R PTFPassif-class.R PTFPassif-load.R HypPassif-load.R HypPassif-class.R Provision-load.R Provision-class.R
##'
setGeneric(name = "load_passif", def = function(address) {standardGeneric("load_passif")})
setMethod(
    f = "load_passif",
    signature = c(address = "character"),
    definition = function(address){

        # Creation des attributs
        ptf_passif  <- load_ptf_passif(paste(address, "01_Portefeuilles", sep = "/"))
        hyp_passif  <- load_hyp_passif(paste(address, "02_Hypotheses", sep = "/"))
        provision   <- load_provision(paste(address, "03_Provisions", sep = "/"))

        # Creation de l'objet
        passif <- new("Passif",
                      ptf_passif = ptf_passif, hyp_passif = hyp_passif, provision = provision)

        # Output
        return(passif)
    }
)
