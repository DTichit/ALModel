##' Fonction \code{proj_1an_passif}
##'
##' Cette fonction permet de projeter horizon 1 an un portefeuille passif.
##'
##' @name proj_1an_passif
##' @docType methods
##' @param passif est un objet de type \code{Passif}.
##' @param an est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "proj_1an_passif", def = function(passif, an) {standardGeneric("proj_1an_passif")})
setMethod(
    f = "proj_1an_passif",
    signature = c(passif = "Passif", an = "integer"),
    definition = function(passif, an){



        ## ######################################################
        ## ######################################################
        ##
        ##              Gestion des contrats epargnes
        ##
        ## ######################################################
        ## ######################################################

        # Projection sur une annee des contrats epargne
        temp <- proj_1an_epargne(epargne = passif@ptf_passif@epargne, hyp_passif = passif@hyp_passif)

        # Mise a jour de l'attribut
        passif@ptf_passif@epargne <- temp[["epargne"]]



        # Output
        return(list(passif = passif))
    }
)
