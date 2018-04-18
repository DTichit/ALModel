##' Fonction \code{revalo_ptf_cible}
##'
##' Cette fonction permet de revaloriser les portfeuilles cibles : mise a jour des valeurs de marche et des
##'
##' @name revalo_ptf_cible
##' @docType methods
##' @param ptf_cible est un objet de type \code{\link{PTFCible}}.
##' @param esg est une \code{list}.
##' @param an est un \code{integer} reprensentant l'annee sur laquelle on travaille.
##' @author Damien Tichit pour Sia Partners
##' @include PTFCible-class.R
##'
setGeneric(name = "revalo_ptf_cible", def = function(ptf_cible, esg, an) {standardGeneric("revalo_ptf_cible")})
setMethod(
    f = "revalo_ptf_cible",
    signature = c(ptf_cible = "PTFCible", esg = "list", an = "integer"),
    definition = function(ptf_cible, esg, an){




        ## ######################################################
        ## ######################################################
        ##
        ##              Revalorisation des PTFs cibles
        ##
        ## ######################################################
        ## ######################################################



        ## ###########################
        ##    Gestion des obligations
        ## ###########################

        # Extraction de la courbe des taux
        name_ctz <- names(esg$ctz_nom)
        num <- which(.subset2(esg$ctz_nom, which(name_ctz == "ProjYr")) == an)
        yield_curve <- .subset2(esg$ctz_nom, which(name_ctz == "ZeroCoupon"))[num]

        # Revalorisation des obligations
        res_revalo_oblig <- revalo_obligation_cible(obligation = ptf_cible@obligation, yield_curve = yield_curve)

        # Mise a jour de l'attribut
        ptf_cible@obligation <- res_revalo_oblig[["obligation"]]





        # Output
        return(list(ptf_cible = ptf_cible))
    }
)
