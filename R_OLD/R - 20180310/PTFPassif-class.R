##' Classe \code{PTFPassif}
##'
##' Cette classe represente le portfeuille passif de la compagnie d'assurance.
##'
##' @name PTFPassif
##' @docType class
##' @slot epargne est un objet de la classe \code{\link{Epargne}} representant le portfeuille epargne.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include Epargne-class.R
##'
setClass(
    Class = "PTFPassif",

    slots = c(epargne = "Epargne"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@epargne))      list_err <- c(list_err, "[PTFPassif] : 'epargne' n'est pas valide")

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
    signature = "PTFPassif",
    definition = function(.Object,
                          epargne = "Epargne"){

        if(! (missing(epargne))){
            .Object@epargne     <- epargne

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@epargne     <- new("Epargne")

            warnings("[PTFPassif] : Attention au moins un des obets est manquant a l'initialisation")
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
    signature = "PTFPassif",
    definition = function(x, i){
        switch(EXPR = i,
               "epargne" = {return(x@epargne)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "PTFPassif",
    definition = function(x, i, value){
        switch(EXPR = i,
               "epargne" = {x@epargne <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
