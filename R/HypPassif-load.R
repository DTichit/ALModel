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

        # Creation des tables
        tab_morta_h <- load_tab_morta(paste(address, "01_TablesMortalite", "TM-H.csv", sep = "/"))
        tab_morta_f <- load_tab_morta(paste(address, "01_TablesMortalite", "TM-F.csv", sep = "/"))
        tab_rachat_tot  <- load_tab_rachat(paste(address, "02_TablesRachat", "TR-T.csv", sep = "/"))
        tab_rachat_part  <- load_tab_rachat(paste(address, "02_TablesRachat", "TR-P.csv", sep = "/"))


        # Lecture du fichiers contenant les chargements
        temp <- read.csv2(paste(address, "chargements.csv", sep = "/"))

        # Creation de la liste contenant les chargements
        chgt <- list(epargne = list(encours = temp[1L, "EP.chgt_encours"],
                                    rachats = temp[1L, "EP.chgt_rachat"],
                                    fixes   = temp[1L, "EP.chgt_fixes"]))

        # Creation de l'objet
        hyp_passif <- new("HypPassif",
                          tab_morta_h = tab_morta_h, tab_morta_f = tab_morta_f, tab_rachat_tot = tab_rachat_tot, tab_rachat_part = tab_rachat_part,
                          chargements = chgt)

        # Output
        return(hyp_passif)
    }
)
