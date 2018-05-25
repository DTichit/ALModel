##' Fonction \code{calcul_fonds_propres}
##'
##' Cette fonction permet de determiner le montant total des fonds propres.
##'
##' @name calcul_fonds_propres
##' @docType methods
##' @param fp est un objet de type \code{FondsPropres}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include FondsPropres-class.R
##'
setGeneric(name = "calcul_fonds_propres", def = function(fp) {standardGeneric("calcul_fonds_propres")})
setMethod(
    f = "calcul_fonds_propres",
    signature = c(fp = "FondsPropres"),
    definition = function(fp){




        ## ###########################
        ## Extraction de donnees
        ## ###########################

        cap_prop <- fp@capitaux_propres
        ran <- fp@report_a_nouveau
        resultat <- fp@resultat_exercice
        dette <- fp@dette




        ## ###########################
        ##           Total
        ## ###########################

        total <- cap_prop + ran + resultat + dette






        # Output
        return(list(total = total,
                    composants = list(capitaux_propres = cap_prop,
                                      report_a_nouveau = ran,
                                      resultat = resultat,
                                      dette = dette)))
    }
)
