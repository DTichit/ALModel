##' Classe \code{HypPassif}
##'
##' Cette classe aggrege l'ensemble des hypotheses relatives au passif d'une compagnie d'assurance : tables de rachat, de mortalite...
##'
##' @name HypPassif
##' @docType class
##' @slot tab_morta_h est un objet de la classe \code{\link{TabMorta}} contenant la table de mortalite pour les hommes.
##' @slot tab_morta_f est un objet de la classe \code{\link{TabMorta}} contenant la table de mortalite pour les hommes.
##' @slot tab_rachat_tot est un objet de la classe \code{\link{TabRachat}} contenant la table modelisant les rachats totaux.
##' @slot tab_rachat_part est un objet de la classe \code{\link{TabRachat}} contenant la table modelisant les rachats partiels.
##' @slot rachat_conj est un objet de la classe \code{\link{RachatConj}} contenant les parametres modelisant les rachats conjoncturels
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include TabMorta-class.R TabRachat-class.R
##'
setClass(
    Class = "HypPassif",

    slots = c(tab_morta_h = "TabMorta",
              tab_morta_f = "TabMorta",
              tab_rachat_tot = "TabRachat",
              tab_rachat_part = "TabRachat",
              rachat_conj = "RachatConj"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@tab_morta_h))       list_err <- c(list_err, "[HypPassif] : 'tab_morta_h' non valide")
        if(! validObject(object@tab_morta_f))       list_err <- c(list_err, "[HypPassif] : 'tab_morta_f' non valide")
        if(! validObject(object@tab_rachat_tot))    list_err <- c(list_err, "[HypPassif] : 'tab_rachat_tot' non valide")
        if(! validObject(object@tab_rachat_part))   list_err <- c(list_err, "[HypPassif] : 'tab_rachat_part' non valide")
        if(! validObject(object@rachat_conj))       list_err <- c(list_err, "[HypPassif] : 'rachat_conj' non valide")

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
    signature = "HypPassif",
    definition = function(.Object,
                          tab_morta_h = "TabMorta",
                          tab_morta_f = "TabMorta",
                          tab_rachat_tot = "TabRachat",
                          tab_rachat_part = "TabRachat",
                          rachat_conj = "RachatConj"){

        if(! (missing(tab_morta_h) | missing(tab_morta_f) | missing(tab_rachat_tot) | missing(tab_rachat_part) | missing(rachat_conj))){
            .Object@tab_morta_h <- tab_morta_h
            .Object@tab_morta_f <- tab_morta_f
            .Object@tab_rachat_tot <- tab_rachat_tot
            .Object@tab_rachat_part <- tab_rachat_part
            .Object@rachat_conj <- rachat_conj

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@tab_morta_h <- new("TabMorta")
            .Object@tab_morta_f <- new("TabMorta")
            .Object@tab_rachat_tot <- new("TabRachat")
            .Object@tab_rachat_part <- new("TabRachat")
            .Object@rachat_conj <- new("RachatConj")
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
    signature = "HypPassif",
    definition = function(x, i){
        switch(EXPR = i,
               "tab_morta_h" = {return(x@tab_morta_h)},
               "tab_morta_f" = {return(x@tab_morta_f)},
               "tab_rachat_part" = {return(x@tab_rachat_part)},
               "tab_rachat_tot" = {return(x@tab_rachat_tot)},
               "rachat_conj" = {return(x@rachat_conj)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "HypPassif",
    definition = function(x, i, value){
        switch(EXPR = i,
               "tab_morta_h" = {x@tab_morta_h <- value},
               "tab_morta_f" = {x@tab_morta_f <- value},
               "tab_rachat_part" = {x@tab_rachat_part <- value},
               "tab_rachat_tot" = {x@tab_rachat_tot <- value},
               "rachat_conj" = {x@rachat_conj <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
