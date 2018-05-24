##' Classe \code{ESG}
##'
##' Cette classe aggrege l'ensemble des donnees relatives  a un ESG. Les donnees sont renseignees par simulation ainsi que par annee de projection.
##'
##' @name ESG
##' @docType class
##' @slot coef_actu est un \code{data.table} : coefficients d'actualisation.
##' @slot ctz_nom est un \code{data.table} : taux zero coupon reels.
##' @slot ctz_reel est un \code{data.table} : taux zero coupon nominaux.
##' @slot eq_dividends est un \code{data.table} : dividendes verses.
##' @slot eq_index est un \code{data.table} : log-rendements du PTF action.
##' @slot im_index est un \code{data.table} : log-rendements du PTF immobilier
##' @slot im_loyer est un \code{data.table} : loyers verses.
##' @slot inflation est un \code{data.table} : taux d'inflation.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @exportClass Epargne
##'
setClass(
    Class = "ESG",

    slots = c(coef_actu = "data.table",
              ctz_nom = "data.table",
              ctz_reel = "data.table",
              eq_dividends = "data.table",
              eq_index = "data.table",
              im_index = "data.table",
              im_loyer = "data.table",
              inflation = "data.table"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL


        # Output
        if (is.null(list_err))
            return(TRUE)
        else
            return(cat(list_err))
    }
)



## ######################################
##          Constructeur
## ######################################
setMethod(
    f = "initialize",
    signature = "ESG",
    definition = function(.Object,
                          coef_actu = "data.table",
                          ctz_nom = "data.table",
                          ctz_reel = "data.table",
                          eq_dividends = "data.table",
                          eq_index = "data.table",
                          im_index = "data.table",
                          im_loyer = "data.table",
                          inflation = "data.table"){

        if(! (missing(coef_actu) | missing(ctz_nom) | missing(ctz_reel) | missing(eq_dividends) |
              missing(eq_index) | missing(im_index) | missing(im_loyer) | missing(inflation))) {

            .Object@coef_actu       <- coef_actu
            .Object@ctz_nom         <- ctz_nom
            .Object@ctz_reel        <- ctz_reel
            .Object@eq_dividends    <- eq_dividends
            .Object@eq_index        <- eq_index
            .Object@im_index        <- im_index
            .Object@im_loyer        <- im_loyer
            .Object@inflation       <- inflation

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide

            .Object@coef_actu       <- data.table()
            .Object@ctz_nom         <- data.table()
            .Object@ctz_reel        <- data.table()
            .Object@eq_dividends    <- data.table()
            .Object@eq_index        <- data.table()
            .Object@im_index        <- data.table()
            .Object@im_loyer        <- data.table()
            .Object@inflation       <- data.table()

        }

        # Output
        return(.Object)
    }
)

## ######################################
##          Getteur
## ######################################
setMethod(
    f = "[",
    signature = "ESG",
    definition = function(x, i){
        switch(EXPR = i,
               "coef_actu" = {return(x@coef_actu)},
               "ctz_nom" = {return(x@ctz_nom)},
               "ctz_reel" = {return(x@ctz_reel)},
               "eq_dividends" = {return(x@eq_dividends)},
               "eq_index" = {return(x@eq_index)},
               "im_index" = {return(x@im_index)},
               "im_loyer" = {return(x@im_loyer)},
               "inflation" = {return(x@inflation)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "ESG",
    definition = function(x, i, value){
        switch(EXPR = i,
               "coef_actu" = {x@coef_actu <- value},
               "ctz_nom" = {x@ctz_nom <- value},
               "ctz_reel" = {x@ctz_reel <- value},
               "eq_dividends" = {x@eq_dividends <- value},
               "eq_index" = {x@eq_index <- value},
               "im_index" = {x@im_index <- value},
               "im_loyer" = {x@im_loyer <- value},
               "inflation" = {x@inflation <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
