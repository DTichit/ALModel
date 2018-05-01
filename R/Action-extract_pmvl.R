##' Fonction \code{extract_pmvl_action}
##'
##' Cette fonction permet de determiner les PMVL pouvant etre realises sur un PTF d'actions.
##'
##' @name extract_pmvl_action
##' @docType methods
##' @param action est un objet de type \code{Action}.
##' @author Damien Tichit pour Sia Partners
##' @include Action-class.R
##'
setGeneric(name = "extract_pmvl_action", def = function(action) {standardGeneric("extract_pmvl_action")})
setMethod(
    f = "extract_pmvl_action",
    signature = c(action = "Action"),
    definition = function(action){



        ## ###########################
        ##    Extraction de donnees
        ## ###########################
        names_ptf   <- names(action@ptf)
        vnc_ptf     <- .subset2(action@ptf, which(names_ptf == "valeur_comptable"))
        vm_ptf      <- .subset2(action@ptf, which(names_ptf == "valeur_marche"))


        ## ###########################
        ##    PMVL sur le PTF oblig
        ## ###########################
        pmvl <- vm_ptf - vnc_ptf




        # Output
        return(pmvl)
    }
)
