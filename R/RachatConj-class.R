##' Classe \code{RachatConj}
##'
##' Cette classe permet de modeliser les rachats conjoncturels.
##'
##'
##' @name RachatConj
##' @docType class
##' @slot alpha est un \code{numeric}
##' @slot beta est un \code{numeric}
##' @slot gamma est un \code{numeric}
##' @slot delta est un \code{numeric}
##' @slot RCmin est un \code{numeric}
##' @slot RCmax est un \code{numeric}
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @exportClass RachatConj
##'
setClass(
    Class = "RachatConj",

    slots = c(alpha = "numeric",
              beta  = "numeric",
              gamma = "numeric",
              delta = "numeric",
              RCmin = "numeric",
              RCmax = "numeric"),

    validity = function (object){

        # Liste stockant les erreurs
        retval <- NULL


        # Output
        if (is.null(retval))
            return (TRUE)
        else
            return (cat(retval))
    }
)



## ######################################
##          Constructeur
## ######################################
setMethod(
    f = "initialize",
    signature = "RachatConj",
    definition = function(.Object,
                          alpha = "numeric",
                          beta  = "numeric",
                          gamma = "numeric",
                          delta = "numeric",
                          RCmin = "numeric",
                          RCmax = "numeric"
    ){

        if(! (missing(alpha) | missing(beta) | missing(gamma) | missing(delta) | missing(RCmin) | missing(RCmax))){
            .Object@alpha <- alpha
            .Object@beta  <- beta
            .Object@gamma <- gamma
            .Object@delta <- delta
            .Object@RCmin <- RCmin
            .Object@RCmax <- RCmax

            # Validation du format
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@alpha <- NA
            .Object@beta  <- NA
            .Object@gamma <- NA
            .Object@delta <- NA
            .Object@RCmin <- NA
            .Object@RCmax <- NA
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
    signature = "RachatConj",
    definition = function(x, i){
        switch(EXPR = i,
               "alpha" = {return(x@alpha)},
               "beta"  = {return(x@beta)},
               "gamma" = {return(x@gamma)},
               "delta" = {return(x@delta)},
               "RCmin" = {return(x@RCmin)},
               "RCmax" = {return(x@RCmax)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "RachatConj",
    definition = function(x, i, value){
        switch(EXPR = i,
               "alpha" = {x@alpha <- value},
               "beta"  = {x@beta <- value},
               "gamma" = {x@gamma <- value},
               "delta" = {x@delta <- value},
               "RCmin" = {x@RCmin <- value},
               "RCmax" = {x@RCmax <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
