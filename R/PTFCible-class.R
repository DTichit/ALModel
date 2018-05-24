##' Classe \code{PTFCible}
##'
##' Cette classe represente le portfeuille financier cible de la compagnie d'assurance dans le cadre de
##'
##' @name PTFCible
##' @docType class
##' @slot obligation est un objet de la classe \code{\link{Obligation}} representant le portfeuille cible obligation.
##' @slot alloc_cible est un objet \code{data.frame} indiquant l'allocation cible pour chaque produit.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include Action-class.R Obligation-class.R Immobilier-class.R
##' @exportClass PTFCible
##'
setClass(
    Class = "PTFCible",

    slots = c(obligation = "Obligation",
              alloc_cible = "data.frame"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@obligation))   list_err <- c(list_err, "[PTFCible] : 'obligation' n'est pas valide")
        if(! is.data.frame(object@alloc_cible))   list_err <- c(list_err, "[PTFCible] : 'alloc_cible' n'est pas valide")

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
    signature = "PTFCible",
    definition = function(.Object,
                          obligation = "Obligation",
                          alloc_cible = "data.frame"){

        if(! ( missing(obligation) | missing(alloc_cible))){
            .Object@obligation  <- obligation
            .Object@alloc_cible <- alloc_cible

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@obligation  <- new("Obligation")
            .Object@alloc_cible     <- data.frame()

            warnings("[PTFCible] : Attention au moins un des obets est manquant a l'initialisation")
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
    signature = "PTFCible",
    definition = function(x, i){
        switch(EXPR = i,
               "obligation" = {return(x@obligation)},
               "alloc_cible" = {return(x@alloc_cible)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "PTFCible",
    definition = function(x, i, value){
        switch(EXPR = i,
               "obligation" = {x@obligation <- value},
               "alloc_cible" = {x@alloc_cible <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
