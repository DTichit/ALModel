##' Classe \code{FondsPropres}
##'
##' Cette classe aggrege l'ensemble des donnees relatives au passif de la compagnie d'assurance : hypotheses, portefeuille, provisions
##'
##' @name FondsPropres
##' @docType class
##' @slot capital_social est un \code{numeric} representant le montant du capital social.
##' @slot report_a_nouveau est un \code{numeric} representant le montant du report a nouveau.
##' @slot resultat_exercice est un \code{numeric} representant le resultat de l'exercice en cours.
##' @slot dette est un \code{numeric}.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @export
##' @include
##'
setClass(
    Class = "FondsPropres",

    slots = c(capital_social = "numeric",
              report_a_nouveau = "numeric",
              resultat_exercice = "numeric",
              dette = "numeric"),

    validity = function (object){

        # Liste stockant les erreurs
        list_err <- NULL

        # Tests sur les classes de l'objet

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
    signature = "FondsPropres",
    definition = function(.Object,
                          capital_social  = "numeric",
                          report_a_nouveau  = "numeric",
                          resultat_exercice  = "numeric"){

        .Object@dette <- 0

        if(! (missing(capital_social) | missing(report_a_nouveau) | missing(resultat_exercice))){
            .Object@capital_social      <- capital_social
            .Object@report_a_nouveau    <- report_a_nouveau
            .Object@resultat_exercice   <- resultat_exercice

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@capital_social      <- NA
            .Object@report_a_nouveau    <- NA
            .Object@resultat_exercice   <- NA

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
    signature = "FondsPropres",
    definition = function(x, i){
        switch(EXPR = i,
               "capital_social" = {return(x@capital_social)},
               "report_a_nouveau" = {return(x@report_a_nouveau)},
               "resultat_exercice" = {return(x@resultat_exercice)},
               "dette" = {return(x@dette)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


## ######################################
##          Setteur
## ######################################
setReplaceMethod(
    f = "[",
    signature = "FondsPropres",
    definition = function(x, i, value){
        switch(EXPR = i,
               "capital_social" = {x@capital_social <- value},
               "report_a_nouveau" = {x@report_a_nouveau <- value},
               "resultat_exercice" = {x@resultat_exercice <- value},
               "dette" = {x@dette <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
