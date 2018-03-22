##' Classe \code{ReserveCapi}
##'
##' Cette classe represente la Reserve de Capitalisation.
##'
##'
##' @name ReserveCapi
##' @docType class
##' @slot montant est un \code{numeric} representant le capital present dans la reserve.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##'
setClass(
    Class = "ReserveCapi",

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
    signature = "ReserveCapi",
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
    signature = "ReserveCapi",
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
    signature = "ReserveCapi",
    definition = function(x, i, value){
        switch(EXPR = i,
               "montant" = {x@montant <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)


