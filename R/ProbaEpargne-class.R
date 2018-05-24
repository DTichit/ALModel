##' Classe \code{ProbaEpargne}
##'
##' Cette classe permet de stocker les differentes probas relatives a un portfeuille epargne : deces et rachats.
##'
##'
##' @name ProbaEpargne
##' @docType class
##' @slot deces_contr est un objet de type \code{data.frame}.
##' @slot deces_pm est un objet de type \code{data.frame}.
##' @slot rachat_part est un objet de type \code{data.frame}.
##' @slot rachat_tot_contr est un objet de type \code{data.frame}.
##' @slot rachat_tot_pm est un objet de type \code{data.frame}.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @exportClass ProbaEpargne
##'
setClass(
    Class = "ProbaEpargne",

    slots = c(deces_contr = "data.frame",
              deces_pm = "data.frame",
              rachat_part = "data.frame",
              rachat_tot_contr = "data.frame",
              rachat_tot_pm = "data.frame"),

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
    signature = "ProbaEpargne",
    definition = function(.Object,
                          deces_contr = data.frame(),
                          deces_pm = data.frame(),
                          rachat_part = data.frame(),
                          rachat_tot_contr = data.frame(),
                          rachat_tot_pm = data.frame()
    ){

        if(! (missing(deces_contr) | missing(deces_pm) | missing(rachat_part) | missing(rachat_tot_contr) | missing(rachat_tot_pm))){

            .Object@deces_contr <- deces_contr
            .Object@deces_pm <- deces_pm
            .Object@rachat_part <- rachat_part
            .Object@rachat_tot_contr <- rachat_tot_contr
            .Object@rachat_tot_pm <- rachat_tot_pm

            # Validation du format
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@deces_contr <- data.frame()
            .Object@deces_pm <- data.frame()
            .Object@rachat_part <- data.frame()
            .Object@rachat_tot_contr <- data.frame()
            .Object@rachat_tot_pm <- data.frame()
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
    signature = "ProbaEpargne",
    definition = function(x, i){
        switch(EXPR = i,
               "deces_contr" = {return(x@deces_contr)},
               "deces_pm" = {return(x@deces_pm)},
               "rachat_part" = {return(x@rachat_part)},
               "rachat_tot_contr" = {return(x@rachat_tot_contr)},
               "rachat_tot_pm" = {return(x@rachat_tot_pm)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "ProbaEpargne",
    definition = function(x, i, value){
        switch(EXPR = i,
               "deces_contr" = {x@deces_contr <- value},
               "deces_pm" = {x@deces_pm <- value},
               "rachat_part" = {x@rachat_part <- value},
               "rachat_tot_contr" = {x@rachat_tot_contr <- value},
               "rachat_tot_pm" = {x@rachat_tot_pm <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
