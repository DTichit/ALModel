##' Classe \code{Passif}
##'
##' Cette classe aggrege l'ensemble des donnees relatives au passif de la compagnie d'assurance : hypotheses, portefeuille, provisions
##'
##' @name Passif
##' @docType class
##' @slot ptf_passif est un objet de la classe \code{\link{PTFPassif}} representant le portfeuille passif.
##' @slot hyp_passif est un objet de la classe \code{\link{HypPassif}} contenant l'ensemble des hypotheses du passif.
##' @slot ppe est un objet de la classe \code{\link{PPE}}.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include PTFPassif-class.R HypPassif-class.R PPE-class.R
##'
setClass(
    Class = "Passif",

    slots = c(ptf_passif = "PTFPassif",
              hyp_passif = "HypPassif",
              ppe = "PPE"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet
        if(! validObject(object@ptf_passif))    list_err <- c(list_err, "[Passif] : 'ptf_passif' n'est pas valide")
        if(! validObject(object@hyp_passif))    list_err <- c(list_err, "[Passif] : 'ptf_passif' n'est pas valide")
        if(! validObject(object@ppe))           list_err <- c(list_err, "[Passif] : 'ppe' n'est pas valide")

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
    signature = "Passif",
    definition = function(.Object,
                          ptf_passif = "PTFPassif",
                          hyp_passif = "HypPassif",
                          ppe        = "PPE"){

        if(! (missing(ptf_passif) | missing(hyp_passif) | missing(ppe))){
            .Object@ptf_passif  <- ptf_passif
            .Object@hyp_passif  <- hyp_passif
            .Object@ppe         <- ppe

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@ptf_passif  <- new("PTFPassif")
            .Object@hyp_passif  <- new("HypPassif")
            .Object@ppe         <- new("PPE")

            warnings("[Passif] : Attention au moins un des obets est manquant a l'initialisation")
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
    signature = "Passif",
    definition = function(x, i){
        switch(EXPR = i,
               "ptf_passif" = {return(x@ptf_passif)},
               "hyp_passif" = {return(x@hyp_passif)},
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
    signature = "Passif",
    definition = function(x, i, value){
        switch(EXPR = i,
               "ptf_passif" = {x@ptf_passif <- value},
               "hyp_passif" = {x@hyp_passif <- value},
               "ppe" = {x@ppe <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
