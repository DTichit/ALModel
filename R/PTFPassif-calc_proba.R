##' Fonction \code{calc_proba_ptf_passif}
##'
##' Cette fonction permet de calculer et de completer les objets relatifs aux probabilites des differents portefeuilles.
##'
##' @name calc_proba_ptf_passif
##' @docType methods
##' @param ptf_passif est un objet de type \code{\link{PTFPassif}}.
##' @param hyp_passif est un objet de type \code{\link{HypPassif}}.
##' @param an est un objet de type \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PTFPassif-class.R HypPassif-class.R
##'
setGeneric(name = "calc_proba_ptf_passif", def = function(ptf_passif, hyp_passif, an) {standardGeneric("calc_proba_ptf_passif")})
setMethod(
    f = "calc_proba_ptf_passif",
    signature = c(ptf_passif = "PTFPassif", hyp_passif = "HypPassif", an = "integer"),
    definition = function(ptf_passif, hyp_passif, an){



        ## ######################################################
        ## ######################################################
        ##
        ##                  Calcul des probas
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Epargne
        ## ###########################

        # Projection sur une annee des contrats epargne
        res_proba_epargne <- calc_proba_epargne(epargne = ptf_passif@epargne, hyp_passif = hyp_passif, an = an)

        # Mise a jour de l'attribut
        ptf_passif@epargne <- res_proba_epargne[["epargne"]]





        # Output
        return(list(ptf_passif = ptf_passif))
    }
)
