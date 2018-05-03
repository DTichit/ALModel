##' Fonction \code{extract_pmvl_immobilier}
##'
##' Cette fonction permet de determiner les PMVL pouvant etre realises sur un PTF immobilier.
##'
##' @name extract_pmvl_immobilier
##' @docType methods
##' @param immobilier est un objet de type \code{Immobilier}.
##' @author Damien Tichit pour Sia Partners
##' @include Immobilier-class.R
##'
setGeneric(name = "extract_pmvl_immobilier", def = function(immobilier) {standardGeneric("extract_pmvl_immobilier")})
setMethod(
    f = "extract_pmvl_immobilier",
    signature = c(immobilier = "Immobilier"),
    definition = function(immobilier){



        ## ###########################
        ##    Extraction de donnees
        ## ###########################
        names_ptf   <- names(immobilier@ptf)
        vnc_ptf     <- .subset2(immobilier@ptf, which(names_ptf == "valeur_comptable"))
        vm_ptf      <- .subset2(immobilier@ptf, which(names_ptf == "valeur_marche"))


        ## ###########################
        ##    PMVL sur le PTF oblig
        ## ###########################
        pmvl <- vm_ptf - vnc_ptf



        ## ###########################
        ##    Detail : PVL et MVL
        ## ###########################

        # PVL
        pvl <- sum(pmvl[which(pmvl > 0)])

        # MVL
        mvl <- -sum(pmvl[which(pmvl < 0)])




        # Output
        return(list(pmvl = sum(pmvl),
                    pvl = pvl,
                    mvl = mvl))
    }
)
