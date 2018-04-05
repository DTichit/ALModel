##' Classe \code{HypActif}
##'
##' Cette classe aggrege l'ensemble des hypotheses relatives au passif de la compagnie d'assurance : portefeuille cible, frais financiers.
##'
##' @name HypActif
##' @docType class
##' @slot ptf_cible est un objet de la classe \code{\link{PTFCible}} representant le portfeuille financier cible.
##' @slot esg_simu est une \code{list} reprenant les donnees de l'ESG pour une simulation.
##' @slot frais_fin est un objet de la classe \code{\link{data.frame}} contenant les hyptheses relatives aux frais financiers.
##' @slot revalo_treso est un \code{numeric} indiquant le taux auquel est revalorise la tresorerie.
##' @author Damien Tichit pour Sia Partners
##' @keywords classes
##' @seealso Le parametre \code{esg_simu} se met a jour grace a la fonction \code{\link{update_esg}}
##' @export
##' @include PTFCible-class.R ALM-update_esg.R
##'
setClass(
    Class = "HypActif",

    slots = c(ptf_cible = "PTFCible",
              esg_simu  = "list",
              frais_fin = "data.frame",
              revalo_treso = "numeric"),

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
                          ptf_cible = "PTFCible",
                          esg_simu  = "list",
                          frais_fin = "data.frame",
                          revalo_treso = "numeric"){

        if(! (missing(ptf_cible) | missing(ptf_cible) | missing(esg_simu) | missing(revalo_treso))){
            .Object@ptf_cible       <- ptf_cible
            .Object@esg_simu        <- esg_simu
            .Object@frais_fin       <- frais_fin
            .Object@revalo_treso    <- revalo_treso

            # Validation de l'objet
            validObject(.Object)

        } else {
            #Traitement du cas vide
            .Object@ptf_cible     <- new("PTFCible")
            .Object@esg_simu      <- list()
            .Object@frais_fin     <- NA
            .Object@revalo_treso     <- NA

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
               "esg_simu"  = {return(x@esg_simu)},
               "frais_fin" = {return(x@frais_fin)},
               "revalo_treso" = {return(x@revalo_treso)},
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
               "esg_simu"  = {x@esg_simu <- value},
               "frais_fin" = {x@frais_fin <- value},
               "revalo_treso" = {x@revalo_treso <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
