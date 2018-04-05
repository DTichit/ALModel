##' Fonction \code{load_esg}.
##'
##' Cette fonction permet de charger les donnees pour un objet de type \code{\link{ESG}}. Les donnees auront ete prealablement disposees dans
##' une architecture propre a \code{SiALM}.
##'
##' @name load_esg
##' @docType methods
##' @param address est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
##' pour la construction de l'objet.
##' @param avec_va est un \code{logical} indiquant si les donnees doivent etre chargees sur le jeu de donnees avec Volatility Adjustment. Par defaut, la valeur est a \code{TRUE}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include ESG-class.R

setGeneric(name = "load_esg", def = function(address, avec_va = TRUE) {standardGeneric("load_esg")})
setMethod(
    f = "load_esg",
    signature = c(address = "character"),
    definition = function(address, avec_va){

        # Volatility Adjustment
        if(avec_va)
            va <- "avecVA"
        else
            va <- "sansVA"

        # Message indiquant le chargement de l'ESG
        message("Chargement de l'ESG.")

        # Creation des attributs
        coef_actu       <- data.table::fread(paste(address, va, "coefActu.csv", sep = "/"))
        ctz_nom         <- data.table::fread(paste(address, va, "CTZ_Nom.csv", sep = "/"))
        ctz_reel        <- data.table::fread(paste(address, va, "CTZ_Reel.csv", sep = "/"))
        eq_dividends    <- data.table::fread(paste(address, va, "EqDividends.csv", sep = "/"))
        eq_index        <- data.table::fread(paste(address, va, "EqIndex.csv", sep = "/"))
        im_index        <- data.table::fread(paste(address, va, "ImIndex.csv", sep = "/"))
        im_loyer        <- data.table::fread(paste(address, va, "ImLoyer.csv", sep = "/"))
        inflation       <- data.table::fread(paste(address, va, "Inflation.csv", sep = "/"))


        # Creation de l'objet
        esg <- new("ESG",
                   coef_actu = coef_actu, ctz_nom = ctz_nom, ctz_reel = ctz_reel, eq_dividends = eq_dividends,
                   eq_index = eq_index, im_index = im_index, im_loyer = im_loyer, inflation = inflation)


        # Message
        message("Fin de chargement de l'ESG.")


        # Output
        return(esg)
    }
)
