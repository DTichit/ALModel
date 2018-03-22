##' Fonction \code{load_passif}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{Passif}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{Passif}} necessite la creation de deux autres objets : \code{\link{Passif}} et \code{\link{HypPassif}}.
##'
##' @name load_passif
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{Passif}} : \code{\link{load_ptf_passif}}.
##' @seealso Construction d'un objet de type \code{\link{HypPassif}} : \code{\link{load_hyp_passif}}.
##' @seealso Construction d'un objet de type \code{\link{PPE}} : \code{\link{load_ppe}}.
##' @export
##' @include Passif-class.R Passif-class.R Passif-load.R HypPassif-load.R HypPassif-class.R PPE-load.R PPE-class.R
##'
setGeneric(name = "load_passif", def = function(address) {standardGeneric("load_passif")})
setMethod(
    f = "load_passif",
    signature = c(address = "character"),
    definition = function(address){

        # Creation des attributs
        ptf_passif  <- load_ptf_passif(paste(address, "01_Portefeuilles", sep = "/"))
        hyp_passif  <- load_hyp_passif(paste(address, "02_Hypotheses", sep = "/"))
        ppe         <- load_ppe(paste(address, "03_PPE", "PPE.csv", sep = "/"))

        # Creation de l'objet
        passif <- new("Passif",
                      ptf_passif = ptf_passif, hyp_passif = hyp_passif, ppe = ppe)

        # Output
        return(passif)
    }
)
