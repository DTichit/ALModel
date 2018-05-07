##' Classe \code{PRE}
##'
##' Cette classe permet de modeliser la PRE.
##'
##'
##' @name PRE
##' @docType class
##' @slot montant est un \code{numeric}.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##'
setClass(
    Class = "PRE",

    slots = c(montant = "numeric"),

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
    signature = "PRE",
    definition = function(.Object,
                          montant = "numeric"
    ){

        if(! missing(montant)){
            .Object@montant <- montant

            # Validation du format
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@montant <- NA
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
    signature = "PRE",
    definition = function(x, i){
        switch(EXPR = i,
               "montant" = {return(x@montant)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "PRE",
    definition = function(x, i, value){
        switch(EXPR = i,
               "montant" = {x@montant <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)


