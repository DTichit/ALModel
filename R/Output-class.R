##' Classe \code{Output}
##'
##' Cette classe englobe l'ensemble des fonctions permettant d'afficher les resultats apres le lancement d'un run.
##'
##' @name Output
##' @docType class
##' @slot stock est une \code{list}.
##' @slot system est un \code{\link{System}}.
##' @slot esg est un \code{\link{ESG}}
##' @slot be est un vecteur \code{numeric} contenant la somme des flux actualises pour chaque simulation.
##' @slot nav est un vecteur \code{numeric} contenant la somme des flux actualises pour chaque simulation.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @exportClass Output
##' @include System-class.R ESG-class.R
##'
##'
setClass(
    Class = "Output",

    slots = c(stock = "list",
              system = "System",
              esg = "ESG",
              be = "numeric",
              nav = "numeric"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet

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
    signature = "Output",
    definition = function(.Object,
                          stock = "list",
                          system = "System",
                          esg = "ESG",
                          be = "numeric",
                          nav = "numeric"){

        if(! (missing(stock) | missing(system) | missing(be) | missing(nav) | missing(esg))){

            .Object@stock   <- stock
            .Object@system  <- system
            .Object@esg     <- esg
            .Object@be      <- be
            .Object@nav     <- nav

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@stock   <- list()
            .Object@system  <- new("System")
            .Object@esg     <- new("ESG")
            .Object@be      <- NULL
            .Object@nav     <- NULL

            warnings("[Output] : Attention au moins un des obets est manquant a l'initialisation")
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
    signature = "Output",
    definition = function(x, i){
        switch(EXPR = i,
               "stock" = {return(x@stock)},
               "system" = {return(x@system)},
               "esg" = {return(x@esg)},
               "be" = {return(x@be)},
               "nav" = {return(x@nav)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "Output",
    definition = function(x, i, value){
        switch(EXPR = i,
               "stock" = {x@stock <- value},
               "system" = {x@system <- value},
               "egs" = {x@esg <- value},
               "be" = {x@be <- value},
               "nav" = {x@nav <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
