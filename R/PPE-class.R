##' Classe \code{PPE}
##'
##' Cette classe represente le Provision pour Participation aux Excedents.
##'
##'
##' @name PPE
##' @docType class
##' @slot ppe est un \code{\link{numeric}} contenant les montants dotes sur les huits dernieres annees.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @exportClass PPE
##'
setClass(
    Class = "PPE",

    slots = c(ppe = "numeric"),

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
    signature = "PPE",
    definition = function(.Object,
                          ppe = "numeric"
    ){

        if(! missing(ppe)){
            .Object@ppe <- ppe

            # Validation du format
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@ppe <- rep(x = NA, length = 8L)
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
    signature = "PPE",
    definition = function(x, i){
        switch(EXPR = i,
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
    signature = "PPE",
    definition = function(x, i, value){
        switch(EXPR = i,
               "ppe" = {x@ppe <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)


