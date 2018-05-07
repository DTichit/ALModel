##' Classe \code{Provision}
##'
##' Cette classe aggrege les differentes provisions relatives au passif d'une compagnie d'assurance : PPE, Reserve de Capitalisation
##'
##' @name Provision
##' @docType class
##' @slot ppe est un objet de la classe \code{\link{PPE}}.
##' @slot reserve_capi est un objet de la classe \code{\link{ReserveCapi}}.
##' @slot pre est un objet de la classe \code{\link{PRE}}.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include ReserveCapi-class.R PPE-class.R PRE-class.R
##'
setClass(
    Class = "Provision",

    slots = c(ppe = "PPE",
              reserve_capi = "ReserveCapi",
              pre = "PRE"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@ppe))           list_err <- c(list_err, "[Provision] : 'ppe' n'est pas valide")
        if(! validObject(object@reserve_capi))  list_err <- c(list_err, "[Provision] : 'reserve_capi' n'est pas valide")
        if(! validObject(object@pre))           list_err <- c(list_err, "[Provision] : 'ppe' n'est pas valide")

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
    signature = "Provision",
    definition = function(.Object,
                          ppe = "PPE",
                          reserve_capi = "ReserveCapi",
                          pre = "PRE"){

        if(! (missing(reserve_capi) | missing(ppe) | missing(pre))){
            .Object@ppe             <- ppe
            .Object@reserve_capi    <- reserve_capi
            .Object@pre             <- pre

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@reserve_capi    <- new("ReserveCapi")
            .Object@ppe             <- new("PPE")
            .Object@pre             <- new("PRE")

            warnings("[Provision] : Attention au moins un des obets est manquant a l'initialisation")
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
    signature = "Provision",
    definition = function(x, i){
        switch(EXPR = i,
               "reserve_capi" = {return(x@reserve_capi)},
               "ppe" = {return(x@ppe)},
               "pre" = {return(x@pre)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "Provision",
    definition = function(x, i, value){
        switch(EXPR = i,
               "reserve_capi" = {x@reserve_capi <- value},
               "ppe" = {x@ppe <- value},
               "pre" = {x@pre <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
