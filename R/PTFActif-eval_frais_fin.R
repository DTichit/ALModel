##' Fonction \code{eval_frais_fin}
##'
##' Cette fonction permet d'evaluer les frais relatifs au portfeuille actif d'une compagnie d'assurance.
##'
##' @name eval_frais_fin
##' @docType methods
##' @param ptf_actif est un objet de type \code{\link{PTFActif}}.
##' @param hyp_actif est un objet de type \code{\link{HypActif}}.
##' @author Damien Tichit pour Sia Partners
##' @include Actif-class.R HypActif-class.R
##'
setGeneric(name = "eval_frais_fin", def = function(ptf_actif, hyp_actif) {standardGeneric("eval_frais_fin")})
setMethod(
    f = "eval_frais_fin",
    signature = c(ptf_actif = "PTFActif", hyp_actif = "HypActif"),
    definition = function(ptf_actif, hyp_actif) {


        warning("Frais financiers : Cette partie doit etre code !! : A voir si elle doit etre placee ici !")


        ## ######################################################
        ## ######################################################
        ##
        ##
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##
        ## ###########################

        #
        frais <- 0




        # Output
        return(list(frais = frais))
    }
)
