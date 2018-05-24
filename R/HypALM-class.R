##' Classe \code{HypALM}
##'
##' Cette classe aggrege l'ensemble des hypotheses relatives au modele ALM.
##'
##' @name HypALM
##' @docType class
##' @slot esg est un objet de type \code{\link{ESG}}.
##' @slot nb_simu est un \code{integer} representant le nombre de simulation souhaite pour calculer le BEL.
##' @slot an_proj est un \code{integer} representant le nombre d'annees de projection.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @include ESG-class.R
##' @exportClass HypALM
##'
setClass(
    Class = "HypALM",

    slots = c(esg = "ESG",
              nb_simu = "integer",
              an_proj = "integer"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@esg))       list_err <- c(list_err, "[HypALM] : 'esg' n'est pas valide")
        if(! is.integer(object@nb_simu))    list_err <- c(list_err, "[HypALM] : 'nb_simu' non valide")
        if(! is.integer(object@an_proj))    list_err <- c(list_err, "[HypALM] : 'an_proj' non valide")

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
                          esg = "ESG",
                          nb_simu = "integer",
                          an_proj = "integer"){

        if(! (missing(nb_simu) | missing(an_proj) | missing(esg))){
            .Object@esg     <- esg
            .Object@nb_simu <- nb_simu
            .Object@an_proj <- an_proj

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@esg     <- new("ESG")
            .Object@nb_simu <- NA
            .Object@an_proj <- NA

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
               "esg"     = {return(x@esg)},
               "nb_simu" = {return(x@nb_simu)},
               "an_proj" = {return(x@an_proj)},
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
               "esg"     = {x@esg <- value},
               "nb_simu" = {x@nb_simu <- value},
               "an_proj" = {x@an_proj <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
