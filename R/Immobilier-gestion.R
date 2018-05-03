##' Fonction \code{gestion_immobilier}
##'
##' Cette fonction permet de gerer un portfeuille immobilier : recolte des loyers, recalcul des VM
##'
##' @name gestion_immobilier
##' @docType methods
##' @param immobilier est un objet de type \code{\link{Immobilier}}.
##' @param hyp_actif est un objet de type \code{\link{HypActif}}.
##' @param an est un \code{integer} reprensentant l'annee sur laquelle on travaille.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R HypActif-class.R
##'
setGeneric(name = "gestion_immobilier", def = function(immobilier, hyp_actif, an) {standardGeneric("gestion_immobilier")})
setMethod(
    f = "gestion_immobilier",
    signature = c(immobilier = "Immobilier", hyp_actif = "HypActif", an = "integer"),
    definition = function(immobilier, hyp_actif, an){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(immobilier@ptf)




        ## ######################################################
        ## ######################################################
        ##
        ##              Mise a jour de la VM
        ##
        ## ######################################################
        ## ######################################################



        ## ###########################
        ##   Mise a jour des VM
        ## ###########################

        # Extraction du log rendement
        log_rdt <- exp(hyp_actif@esg_simu$im_index[an])

        # Calcul des nouvelles VM
        new_vm <- .subset2(immobilier@ptf, which(name_ptf == "valeur_marche")) * log_rdt

        # Mise a jour de l'attribut
        immobilier@ptf$valeur_marche <- new_vm



        ## ###########################
        ##       Calcul des PMVL
        ## ###########################

        # Calcul des PMVL
        pmvl <- new_vm - .subset2(immobilier@ptf, which(name_ptf == "valeur_comptable"))






        ## ######################################################
        ## ######################################################
        ##
        ##                  Calcul des loyers
        ##
        ## ######################################################
        ## ######################################################

        # Extraction loyer
        loyers <- hyp_actif@esg_simu$im_loyer[an]

        # Calcul du loyer
        loyers <- loyers * .subset2(immobilier@ptf, which(name_ptf == "valeur_marche"))





        # Output
        return(list(immobilier = immobilier,
                    pmvl = sum(pmvl),
                    loyers = sum(loyers)))
    }
)
