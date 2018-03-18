##' Classe \code{HypActif}
##'
##' Cette classe aggrege l'ensemble des hypotheses relatives au passif de la compagnie d'assurance : portefeuille cible
##'
##' @name HypActif
##' @docType class
##' @slot ptf_cible est un objet de la classe \code{\link{PTFCible}} representant le portfeuille financier cible.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include PTFCible-class.R
##'
setClass(
    Class = "HypActif",

    slots = c(ptf_cible = "PTFCible"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@ptf_cible))       list_err <- c(list_err, "[HypActif] : 'ptf_cible' n'est pas valide")

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
    signature = "HypActif",
    definition = function(.Object,
                          ptf_cible = "PTFCible"){

        if(! (missing(ptf_cible))){
            .Object@ptf_cible      <- ptf_cible

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@ptf_cible     <- new("PTFCible")

            warnings("[HypActif] : Attention au moins un des obets est manquant a l'initialisation")
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
    signature = "HypActif",
    definition = function(x, i){
        switch(EXPR = i,
               "ptf_cible" = {return(x@ptf_cible)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "HypActif",
    definition = function(x, i, value){
        switch(EXPR = i,
               "ptf_cible" = {x@ptf_cible <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)