##' Classe \code{ALM}
##'
##' Cette classe aggrege un objet \code{\link{System}} ainsi que l'ensemble des hypotheses du modele ALM.
##'
##' @name ALM
##' @docType class
##' @slot system est un objet de la classe \code{\link{System}} qui aggregent les actifs et les passifs ainsi que leurs hypothèses.
##' @slot hyp_alm est un objet de la classe \code{\link{HypALM}} qui contient les differentes hypotheses du modele ALM.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include System-class.R HypALM-class.R
##' @exportClass ALM
##'
setClass(
    Class = "ALM",

    slots = c(system = "System",
              hyp_alm = "HypALM"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@system))    list_err <- c(list_err, "[ALM] : 'system' n'est pas valide")
        if(! validObject(object@hyp_alm))   list_err <- c(list_err, "[ALM] : 'hyp_alm' n'est pas valide")

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
    signature = "ALM",
    definition = function(.Object,
                          system = "System",
                          hyp_alm = "HypALM"){

        if(! (missing(system) | missing(hyp_alm))){
            .Object@system      <- system
            .Object@hyp_alm     <- hyp_alm

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@system      <- new("System")
            .Object@hyp_alm     <- new("HypALM")

            warnings("[ALM] : Attention au moins un des obets est manquant a l'initialisation")
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
    signature = "ALM",
    definition = function(x, i){
        switch(EXPR = i,
               "system" = {return(x@system)},
               "hyp_alm" = {return(x@hyp_alm)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "ALM",
    definition = function(x, i, value){
        switch(EXPR = i,
               "system" = {x@system <- value},
               "hyp_alm" = {x@hyp_alm <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
