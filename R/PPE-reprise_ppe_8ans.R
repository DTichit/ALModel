##' Fonction \code{reprise_ppe_8ans}.
##'
##' Cette fonction permet de reprendre le montant dotee 8 annees auparavant. Elle met egalement a 0 l'element du vecteur correspondant a la PPE dotee 8 ans auparavant.
##'
##' @name reprise_ppe_8ans
##' @docType methods
##' @param ppe est un objet de type \code{\link{PPE}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PPE-class.R
##'
setGeneric(name = "reprise_ppe_8ans", def = function(ppe) {standardGeneric("reprise_ppe_8ans")})
setMethod(
    f = "reprise_ppe_8ans",
    signature = c(ppe = "PPE"),
    definition = function(ppe){


        ## ###########################
        ##  PPE dotee 8ans auparavant
        ## ###########################
        ppe_8ans <- ppe@ppe[8L]



        ## ###########################
        ##  Mise a zero de la PPE 8ans
        ## ###########################
        ppe@ppe[8L] <- 0




        # Output
        return(list(ppe = ppe,
                    ppe_8ans = ppe_8ans))
    }
)
