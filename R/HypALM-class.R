##' Classe \code{HypALM}
##'
##' Cette classe aggrege l'ensemble des hypotheses relatives au modele ALM.
##'
##' @name HypALM
##' @docType class
##' @slot nb_simu est un \code{integer} representant le nombre de simulation souhaite pour calculer le BEL.
##' @slot an_proj est un \code{integer} representant le nombre d'annees de projection.
##' @slot tsr est un \code{numeric} contenant les taux sans risque avec Volatility Adjustment.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##'
setClass(
    Class = "HypALM",

    slots = c(nb_simu = "integer",
              an_proj = "integer",
              tsr     = "numeric"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! is.integer(object@nb_simu))       list_err <- c(list_err, "[HypALM] : 'nb_simu' non valide")
        if(! is.integer(object@an_proj))       list_err <- c(list_err, "[HypALM] : 'an_proj' non valide")

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
    signature = "HypALM",
    definition = function(.Object,
                          nb_simu = "integer",
                          an_proj = "integer",
                          tsr = "numeric"){

        if(! (missing(nb_simu) | missing(an_proj) | missing(tsr))){
            .Object@nb_simu <- nb_simu
            .Object@an_proj <- an_proj
            .Object@tsr     <- tsr

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@nb_simu <- NULL
            .Object@an_proj <- NULL
            .Object@tsr     <- NULL

            warnings("[HypALM] : Attention au moins un des obets est manquant a l'initialisation")
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
    signature = "HypALM",
    definition = function(x, i){
        switch(EXPR = i,
               "nb_simu" = {return(x@nb_simu)},
               "an_proj" = {return(x@an_proj)},
               "tsr"     = {return(x@tsr)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "HypALM",
    definition = function(x, i, value){
        switch(EXPR = i,
               "nb_simu" = {x@nb_simu <- value},
               "an_proj" = {x@an_proj <- value},
               "tsr"     = {x@tsr <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
