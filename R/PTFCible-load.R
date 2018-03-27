##' Fonction \code{load_ptf_cible}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{PTFCible}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{PTFCible}} necessite la creation d'un autre objet : \code{\link{Obligation}}.
##'
##' @name load_ptf_cible
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{Obligation}} : \code{\link{load_obligation_cible}}.
##' @export
##' @include Actif-class.R Action-class.R Action-load.R Obligation-class.R Obligation-load.R Immobilier-class.R Immobilier-load.R
##'
setGeneric(name = "load_ptf_cible", def = function(address) {standardGeneric("load_ptf_cible")})
setMethod(
    f = "load_ptf_cible",
    signature = c(address = "character"),
    definition = function(address){

        # Creation des attributs
        obligation  <- load_obligation_cible(paste(address, "01_Portfeuilles", "Obligations.csv", sep = "/"))

        # Lecture des allocations cibles
        alloc_cible <- read.csv2(paste(address, "allocation_cible.csv", sep = "/"))

        # Test sur la somme des proportions
        if(sum(alloc_cible$prop) != 1)
            stop("[load_ptf_cible] : Somme des proportions differente de 1 dans le fichier 'allocation_cible.csv'.")

        # Creation de l'objet
        ptf_cible <- new("PTFCible",obligation = obligation, alloc_cible = alloc_cible)

        # Output
        return(ptf_cible)
    }
)
