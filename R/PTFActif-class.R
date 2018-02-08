##' Classe \code{PTFActif}
##'
##' Cette classe represente le portfeuille financier de la compagnie d'assurance.
##'
##' @name PTFActif
##' @docType class
##' @slot action est un objet de la classe \code{\link{Action}} representant le portfeuille action.
##' @slot obligation est un objet de la classe \code{\link{Obligation}} representant le portfeuille obligation.
##' @slot tresorerie est un objet de la classe \code{\link{Tresorerie}} representant le portfeuille tresorerie.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include Action-class.R Obligation-class.R Tresorerie-class.R
##'
setClass(
    Class = "PTFActif",

    slots = c(action = "Action",
              obligation = "Obligation",
              tresorerie = "Tresorerie"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@action))       list_err <- c(list_err, "[PTFActif] : 'action' n'est pas valide")
        if(! validObject(object@obligation))   list_err <- c(list_err, "[PTFActif] : 'obligation' n'est pas valide")
        if(! validObject(object@tresorerie))   list_err <- c(list_err, "[PTFActif] : 'tresorerie' n'est pas valide")

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
    signature = "PTFActif",
    definition = function(.Object,
                          action = "Action",
                          obligation = "Obligation",
                          tresorerie = "Tresorerie"){

        if(! (missing(action) | missing(obligation) | missing(tresorerie))){
            .Object@action      <- action
            .Object@obligation  <- obligation
            .Object@tresorerie  <- tresorerie

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@action     <- new("Action")
            .Object@obligation  <- new("Obligation")
            .Object@tresorerie     <- new("Tresorerie")

            warnings("[Actif] : Attention au moins un des obets est manquant a l'initialisation")
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
    signature = "PTFActif",
    definition = function(x, i){
        switch(EXPR = i,
               "action" = {return(x@action)},
               "obligation" = {return(x@obligation)},
               "tresorerie" = {return(x@tresorerie)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "PTFActif",
    definition = function(x, i, value){
        switch(EXPR = i,
               "action" = {x@action <- value},
               "obligation" = {x@obligation <- value},
               "tresorerie" = {x@tresorerie <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
