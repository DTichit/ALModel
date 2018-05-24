##' Classe \code{System}
##'
##' Cette classe regroupe les actifs et les passifs d'une compagnie d'assurance.
##'
##'
##' @name System
##' @docType class
##' @slot passif est un objet de type \code{\link{Passif}}.
##' @slot actif est un objet de type \code{\link{Actif}}.
##' @slot taux_pb est une \code{list} contenant les taux de pb contractuels.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include Passif-class.R Actif-class.R PPE-class.R
##' @exportClass System
##'
setClass(
    Class = "System",

    slots = c(passif = "Passif",
              actif  = "Actif",
              taux_pb = "list"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@passif))    list_err <- c(list_err, "[System] : 'passif' n'est pas valide")
        if(! validObject(object@actif))     list_err <- c(list_err, "[System] : 'actif' n'est pas valide")

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
                          taux_pb = "list"){

        if(! (missing(passif) | missing(actif) | missing(taux_pb))){
            .Object@passif  <- passif
            .Object@actif   <- actif
            .Object@taux_pb <- taux_pb

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@passif  <- new("Passif")
            .Object@actif   <- new("Actif")
            .Object@taux_pb <- list(technique = NA, financier = NA)
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
               "taux_pb" = {return(x@taux_pb)},
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
               "taux_pb" = {x@taux_pb <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
