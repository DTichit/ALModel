##' Classe \code{Epargne}
##'
##' Cette classe represente le portefeuille des contrats epargne de la compagnie d'assurance.
##'
##'
##' @name Epargne
##' @docType class
##' @slot ptf est un objet de type \code{data.frame} contenant les donnees relatives au portfeuille.
##' @slot proba est un objet de type \code{\link{ProbaEpargne}} contenant les probas relatives au portfeuille.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##'
setClass(
    Class = "Epargne",

    slots = c(ptf = "data.frame",
              proba = "ProbaEpargne"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@proba))      list_err <- c(list_err, "[Epargne] : 'proba' n'est pas valide")

        # Output
        if (is.null(list_err))
            return (TRUE)
        else
            return (cat(list_err))
    }
)



## ######################################
##          Constructeur
## ######################################
setMethod(
    f = "initialize",
    signature = "Epargne",
    definition = function(.Object,
                          ptf = data.frame()
    ){

        if(! missing(ptf)){
            .Object@ptf <- ptf
            .Object@proba <- new("ProbaEpargne")

            # Validation du format
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@ptf <- data.frame()
            .Object@proba <- new("ProbaEpargne")
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
    signature = "Epargne",
    definition = function(x, i){
        switch(EXPR = i,
               "ptf" = {return(x@ptf)},
               "proba" = {return(x@proba)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "Epargne",
    definition = function(x, i, value){
        switch(EXPR = i,
               "ptf" = {x@ptf <- value},
               "proba" = {x@proba <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
