##' Fonction \code{calcul_pb}.
##'
##' Cette fonction permet de determiner la PB a distribuer.
##'
##' @name calcul_pb
##' @docType methods
##' @param taux_pb est une \code{list} contenant les deux taux de pb contractuels.
##' @param resultat_fin est un \code{numeric}
##' @param resultat_tech est un \code{numeric}
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include
##'
setGeneric(name = "calcul_pb", def = function(taux_pb, resultat_fin, resultat_tech) {standardGeneric("calcul_pb")})
setMethod(
    f = "calcul_pb",
    signature = c(taux_pb = "list", resultat_fin = "numeric", resultat_tech = "numeric"),
    definition = function(taux_pb, resultat_fin, resultat_tech) {


        ## ###########################
        ##     PB a distribuer
        ## ###########################

        # PB financiere
        if(resultat_fin < 0)
            pb_fin <- 0
        else
            pb_fin <- taux_pb[["financier"]] * resultat_fin


        # PB technique
        if(resultat_tech < 0)
            pb_tech <- resultat_tech
        else
            pb_tech <- taux_pb[["technique"]] * resultat_tech




        ## ###########################
        ##  Resultats non distribues
        ## ###########################

        # Difference entre le resultat et la PB
        reste_fin <- resultat_fin - pb_fin
        reste_tech <- resultat_tech - pb_tech





        # Output
        return(list(pb = list(financier = pb_fin,
                              technique = pb_tech),
                    reste = list(financier = reste_fin,
                                 technique = reste_tech)))

    }
)
