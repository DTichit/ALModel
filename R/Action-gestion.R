##' Fonction \code{gestion_action}
##'
##' Cette fonction permet de gerer un portfeuille action : recolte des dividendes, recalcul des VM
##'
##' @name gestion_action
##' @docType methods
##' @param action est un objet de type \code{\link{Action}}.
##' @param hyp_actif est un objet de type \code{\link{HypActif}}.
##' @param an est un \code{integer} reprensentant l'annee sur laquelle on travaille.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Action-class.R HypActif-class.R
##'
setGeneric(name = "gestion_action", def = function(action, hyp_actif, an) {standardGeneric("gestion_action")})
setMethod(
    f = "gestion_action",
    signature = c(action = "Action", hyp_actif = "HypActif", an = "integer"),
    definition = function(action, hyp_actif, an){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(action@ptf)




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
        log_rdt <- exp(hyp_actif@esg_simu$eq_index[an])

        # Calcul des nouvelles VM
        new_vm <- .subset2(action@ptf, which(name_ptf == "valeur_marche")) * log_rdt

        # Mise a jour de l'attribut
        action@ptf$valeur_marche <- new_vm



        ## ###########################
        ##       Calcul des PMVL
        ## ###########################

        # Calcul des PMVL
        pmvl <- new_vm - .subset2(action@ptf, which(name_ptf == "valeur_comptable"))






        ## ######################################################
        ## ######################################################
        ##
        ##                  Calcul des dividendes
        ##
        ## ######################################################
        ## ######################################################

        # Extraction dividendes
        div <- hyp_actif@esg_simu$eq_dividends[an]

        # Calcul des dividendes
        dividendes <- div * .subset2(action@ptf, which(name_ptf == "valeur_marche"))





        # Output
        return(list(action = action,
                    pmvl = pmvl,
                    dividendes = dividendes))
    }
)
