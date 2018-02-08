##' Classe \code{Actif}
##'
##' Cette classe aggrege l'ensemble des donnees relatives a l'actif de la compagnie d'assurance : portefeuille
##'
##' @name Actif
##' @docType class
##' @slot ptf_actif est un objet de la classe \code{\link{PTFActif}} representant le portfeuille financier.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include PTFActif-class.R
##'
setClass(
    Class = "Actif",

    slots = c(ptf_actif = "PTFActif"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@ptf_actif))       list_err <- c(list_err, "[Actif] : 'ptf_actif' n'est pas valide")

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
    signature = "Actif",
    definition = function(.Object,
                          ptf_actif = "PTFActif"){

        if(! missing(ptf_actif)){
            .Object@ptf_actif      <- ptf_actif

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@ptf_actif     <- new("PTFActif")

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
    signature = "Actif",
    definition = function(x, i){
        switch(EXPR = i,
               "ptf_actif" = {return(x@ptf_actif)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "Actif",
    definition = function(x, i, value){
        switch(EXPR = i,
               "ptf_actif" = {x@ptf_actif <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
