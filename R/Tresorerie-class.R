##' Classe \code{Tresorerie}
##'
##' Cette classe represente la tresorerie de la compagnie d'assurance.
##'
##'
##' @name Tresorerie
##' @docType class
##' @slot ptf est un objet de type \code{\link{data.frame}} contenant les donnees relatives au portfeuille.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##'
setClass(
    Class = "Tresorerie",

    slots = c(ptf = "data.frame"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL


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
    signature = "Tresorerie",
    definition = function(.Object,
                          ptf = data.frame()
    ){

        if(! missing(ptf)){
            .Object@ptf <- ptf

            # Validation du format
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@ptf <- data.frame()
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
    signature = "Tresorerie",
    definition = function(x, i){
        switch(EXPR = i,
               "ptf" = {return(x@ptf)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "Tresorerie",
    definition = function(x, i, value){
        switch(EXPR = i,
               "ptf" = {x@ptf <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
