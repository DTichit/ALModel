##' Fonction \code{load_hyp_passif}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{HypPassif}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' La creation d'un objet \code{\link{HypPassif}} necessite la creation de deux autres objets differents : \code{\link{TabMorta}} et \code{\link{TabRachat}}.
##'
##' @name load_hyp_passif
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @author Damien Tichit pour Sia Partners
##' @seealso Construction d'un objet de type \code{\link{TabMorta}} : \code{\link{load_tab_morta}}.
##' @seealso Construction d'un objet de type \code{\link{TabRachat}} : \code{\link{load_tab_rachat}}.
##' @export
##' @include HypPassif-class.R TabMorta-class.R TabMorta-load.R TabMorta-load.R TabMorta-class.R
##'
setGeneric(name = "load_hyp_passif", def = function(address) {standardGeneric("load_hyp_passif")})
setMethod(
    f = "load_hyp_passif",
    signature = c(address = "character"),
    definition = function(address){

        # Creation des tables de mortalite
        tab_morta_h <- load_tab_morta(paste(address, "01_TablesMortalite", "TM-H.csv", sep = "/"))
        tab_morta_f <- load_tab_morta(paste(address, "01_TablesMortalite", "TM-F.csv", sep = "/"))

        # Creation des tables de rachats
        tab_rachat_tot  <- load_tab_rachat(paste(address, "02_TablesRachat", "TR-T.csv", sep = "/"))
        tab_rachat_part  <- load_tab_rachat(paste(address, "02_TablesRachat", "TR-P.csv", sep = "/"))

        # Chargement de l'objet modelisant les rachats conjoncturels
        rachat_conj <- load_rachat_conj(paste(address, "03_RachatsConjoncturels", sep = "/"))

        # Lecture des fichiers csv
        prop_pb <- read.csv2(paste(address, "prop_pb.csv", sep = "/"), header = TRUE, colClasses = c("character", "numeric"))
        dividende <- read.csv2(paste(address, "dividende.csv", sep = "/"), header = TRUE, colClasses = c("numeric"))

        # Test sur le fichier
        if(sum(prop_pb$proportion) != 1)
            stop("La somme des proportions de PB a attribuer est differente de 1.")

        # Creation de l'objet
        hyp_passif <- new("HypPassif",
                          tab_morta_h = tab_morta_h, tab_morta_f = tab_morta_f,
                          tab_rachat_tot = tab_rachat_tot, tab_rachat_part = tab_rachat_part,
                          rachat_conj = rachat_conj, prop_pb = prop_pb, dividende = dividende[["taux"]],
                          esg_simu = list(), cible = list())

        # Output
        return(hyp_passif)
    }
)
