##' Classe \code{Tresorerie}
##'
##' Cette classe represente la tresorerie de la compagnie d'assurance.
##'
##'
##' @name Tresorerie
##' @docType class
##' @slot solde est un objet \code{numeric} indiquant le solde.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @exportClass Tresorerie
##'
setClass(
    Class = "Tresorerie",

    slots = c(solde = "numeric"),

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
                          solde = "numeric"
    ){

        if(! missing(solde)){
            .Object@solde <- solde

            # Validation du format
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@solde <- NA
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
               "solde" = {return(x@solde)},
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
               "solde" = {x@solde <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
