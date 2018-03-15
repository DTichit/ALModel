##' Fonction \code{proj_1an_passif}
##'
##' Cette fonction permet de projeter horizon 1 an le passif d'une compagnie d'assurance.
##'
##' @name proj_1an_passif
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "proj_1an_passif", def = function(passif) {standardGeneric("proj_1an_passif")})
setMethod(
    f = "proj_1an_passif",
    signature = c(passif = "Passif"),
    definition = function(passif){



        ## ######################################################
        ## ######################################################
        ##
        ## Gestion du portfeuille : Prestation, revalorisation aux TMG...
        ##
        ## ######################################################
        ## ######################################################

        # Projection sur une annee des contrats epargne
        res_proj_ptf <- proj_1an_ptf_passif(ptf_passif = passif@ptf_passif, hyp_passif = passif@hyp_passif)

        # Mise a jour de l'attribut
        passif@ptf_passif <- res_proj_ptf[["ptf_passif"]]





        ## ######################################################
        ## ######################################################
        ##
        ##                  Gestion de la PB
        ##
        ## ######################################################
        ## ######################################################

        # Determination de la PB a attribuer



        # Output
        return(list(passif = passif))
    }
)
