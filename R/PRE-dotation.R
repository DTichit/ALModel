##' Fonction \code{dotation_pre}.
##'
##' Cette fonction permet de doter la PRE.
##'
##' @name dotation_pre
##' @docType methods
##' @param pre est un objet de type \code{\link{PRE}}.
##' @param pmvl est une \code{list} contenant les differentes PMVL par classes d'actifs modelisees.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PRE-class.R
##'
setGeneric(name = "dotation_pre", def = function(pre, pmvl) {standardGeneric("dotation_pre")})
setMethod(
    f = "dotation_pre",
    signature = c(pre = "PRE", pmvl = "list"),
    definition = function(pre, pmvl){

        # Extraction des PMVL
        pmvl_immobilier <- sum(pmvl[["immobilier"]])
        pmvl_action     <- sum(pmvl[["action"]])

        # Somme des PMVL
        pmvl <- pmvl_immobilier + pmvl_action


        # Image du montant avant la dotation
        pre_av_dotation <- pre@montant


        # Mise a jour de la PRE
        pre@montant <- abs(min(pmvl, 0))


        # Calcul du flux
        flux <- pre@montant - pre_av_dotation


        # Output
        return(list(pre = pre,
                    flux = flux))
    }
)
