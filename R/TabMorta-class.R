##' Classe \code{TabMorta}
##'
##' Cette classe represente une table de mortalite.
##'
##'
##' @name TabMorta
##' @docType class
##' @slot table est un objet de type \code{\link{data.frame}} contenant la table de mortalite.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @exportClass TabMorta
##'
setClass(
    Class = "TabMorta",

    slots = c(table = "data.frame"),

    validity = function (object){

        # Liste stockant les erreurs
        retval <- NULL

        # Tests
        if(length(object@table) == 0L)        retval <- c(retval, "[TabMorta] : 'table' ne doit pas etre vide")

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
    signature = "TabMorta",
    definition = function(.Object,
                          table = data.frame()
    ){

        if(! missing(table)){
            .Object@table <- table

            # Validation du format
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@table <- data.frame()
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
    signature = "TabMorta",
    definition = function(x, i){
        switch(EXPR = i,
               "table" = {return(x@table)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "TabMorta",
    definition = function(x, i, value){
        switch(EXPR = i,
               "table" = {x@table <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)


