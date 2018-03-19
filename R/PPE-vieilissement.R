##' Fonction \code{vieillissement_ppe}.
##'
##' Cette fonction permet de vieillir d'une annee l'objet \code{\link{PPE}}
##'
##' @name vieillissement_ppe
##' @docType methods
##' @param ppe est un objet de type \code{\link{PPE}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PPE-class.R
##'
setGeneric(name = "vieillissement_ppe", def = function(ppe) {standardGeneric("vieillissement_ppe")})
setMethod(
    f = "vieillissement_ppe",
    signature = c(ppe = "PPE"),
    definition = function(ppe){

        # Warning si la 8eme annee n'a as ete reprise
        if(ppe@ppe[8L]>0)
            warning("La dotation de la PPE de l'annee i-8 n'a pas ete reprise.")


        ## ###########################
        ##   Vieilissement de la PPE
        ## ###########################
        for (i in 7L:1L)
            ppe@ppe[i+1L] <- ppe@ppe[i]



        ## ###########################
        ## Mise a jour de la 1ere annee
        ## ###########################
        ppe@ppe[1L] <- 0


        # Output
        return(list(ppe = ppe))
    }
)
