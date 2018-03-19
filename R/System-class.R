##' Classe \code{System}
##'
##' Cette classe regroupe les actifs et les passifs d'une compagnie d'assurance.
##'
##'
##' @name System
##' @docType class
##' @slot passif est un objet de type \code{\link{Passif}}.
##' @slot actif est un objet de type \code{\link{Actif}}.
##' @slot ppe est un objet de type \code{\link{PPE}}.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include Passif-class.R Actif-class.R PPE-class.R
##'
setClass(
    Class = "System",

    slots = c(passif = "Passif",
              actif  = "Actif",
              ppe    = "PPE"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@passif))    list_err <- c(list_err, "[System] : 'passif' n'est pas valide")
        if(! validObject(object@actif))     list_err <- c(list_err, "[System] : 'actif' n'est pas valide")
        if(! validObject(object@ppe))       list_err <- c(list_err, "[System] : 'ppe' n'est pas valide")

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
    signature = "System",
    definition = function(.Object,
                          passif = "Passif",
                          actif = "Actif",
                          ppe = "PPE"){

        if(! (missing(passif) | missing(actif) | missing(ppe))){
            .Object@passif  <- passif
            .Object@actif   <- actif
            .Object@ppe     <- ppe

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@passif <- new("Passif")
            .Object@actif  <- new("Actif")
            .Object@ppe  <- new("PPE")
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
    signature = "System",
    definition = function(x, i){
        switch(EXPR = i,
               "passif" = {return(x@passif)},
               "actif" = {return(x@actif)},
               "ppe" = {return(x@ppe)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "System",
    definition = function(x, i, value){
        switch(EXPR = i,
               "passif" = {x@passif <- value},
               "actif" = {x@actif <- value},
               "ppe" = {x@ppe <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
