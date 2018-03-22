##' Fonction \code{revalo_passif}
##'
##' Cette fonction permet de revaloriser le passif d'une compagnie d'assurance.
##'
##' @name revalo_passif
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @param pb est un \code{numeric} representant le montant de PB a distribuer.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "revalo_passif", def = function(passif, pb) {standardGeneric("revalo_passif")})
setMethod(
    f = "revalo_passif",
    signature = c(passif = "Passif", pb = "numeric"),
    definition = function(passif, pb){



        ## ######################################################
        ## ######################################################
        ##
        ##    Determination du montant total de PB a distribuer
        ##
        ## ######################################################
        ## ######################################################

        # Reprise de la PPE dotee 8 ans auparavant
        ppe_8ans <- reprise_ppe_8ans(passif@provision@ppe)

        # Mise a jour de l'objet PPE
        passif@provision@ppe <- ppe_8ans[["ppe"]]

        # Montant total a distribuer
        pb_totale <- pb + ppe_8ans[["ppe_8ans"]]




        ## ######################################################
        ## ######################################################
        ##
        ##          Determination de la PB a distribuer
        ##
        ## ######################################################
        ## ######################################################

        warning("Fonction a finaliser")






        # Output
        return(list(passif = passif))
    }
)
