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

        # Coupon les warnings
        options(warn = -1)

        # Creation des attributs
        coef_actu       <- data.table::fread(paste(address, va, "coefActu.csv", sep = "/"), dec = ",", sep = ";")[,CoefActu:=ch2numFunct(CoefActu)]
        ctz_nom         <- data.table::fread(paste(address, va, "CTZ_Nom.csv", sep = "/"), dec = ",", sep = ";")[,ZeroCoupon:=ch2numFunct(ZeroCoupon)]
        ctz_reel        <- data.table::fread(paste(address, va, "CTZ_Reel.csv", sep = "/"), dec = ",", sep = ";")[,ZeroCoupon:=ch2numFunct(ZeroCoupon)]
        eq_dividends    <- data.table::fread(paste(address, va, "EqDividends.csv", sep = "/"), dec = ",", sep = ";")[,EqDividends:=ch2numFunct(EqDividends)]
        eq_index        <- data.table::fread(paste(address, va, "EqIndex.csv", sep = "/"), dec = ",", sep = ";")[,EqIndex:=ch2numFunct(EqIndex)]
        im_index        <- data.table::fread(paste(address, va, "ImIndex.csv", sep = "/"), dec = ",", sep = ";")[,ImIndex:=ch2numFunct(ImIndex)]
        im_loyer        <- data.table::fread(paste(address, va, "ImLoyer.csv", sep = "/"), dec = ",", sep = ";")[,ImLoyer:=ch2numFunct(ImLoyer)]
        inflation       <- data.table::fread(paste(address, va, "Inflation.csv", sep = "/"), dec = ",", sep = ";")[,Inflation:=ch2numFunct(Inflation)]



        # Remettre les warnings
        options(warn = 0)


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
