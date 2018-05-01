##' Fonction \code{extract_pmvl_obligation}
##'
##' Cette fonction permet de determiner les PMVL pouvant etre realises sur un PTF obligataire.
##'
##' @name extract_pmvl_obligation
##' @docType methods
##' @param obligation est un objet de type \code{Obligation}.
##' @author Damien Tichit pour Sia Partners
##' @include Obligation-class.R
##'
setGeneric(name = "extract_pmvl_obligation", def = function(obligation) {standardGeneric("extract_pmvl_obligation")})
setMethod(
    f = "extract_pmvl_obligation",
    signature = c(obligation = "Obligation"),
    definition = function(obligation){



        ## ###########################
        ##    Extraction de donnees
        ## ###########################
        names_ptf   <- names(obligation@ptf)
        vnc_ptf     <- .subset2(obligation@ptf, which(names_ptf == "valeur_nette_comptable"))
        vm_ptf      <- .subset2(obligation@ptf, which(names_ptf == "valeur_marche"))


        ## ###########################
        ##    PMVL sur le PTF oblig
        ## ###########################
        pmvl <- vm_ptf - vnc_ptf




        # Output
        return(pmvl)
    }
)
