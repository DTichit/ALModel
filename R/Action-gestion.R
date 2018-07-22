##' Fonction \code{gestion_action}
##'
##' Cette fonction permet de gerer un portfeuille action : recolte des dividendes, recalcul des VM
##'
##' @name gestion_action
##' @docType methods
##' @param action est un objet de type \code{\link{Action}}.
##' @param esg_simu est une \code{list} contenant les rendements et les dividendes.
##' @param an est un \code{integer} reprensentant l'annee sur laquelle on travaille.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Action-class.R HypActif-class.R
##'
setGeneric(name = "gestion_action", def = function(action, esg_simu, an) {standardGeneric("gestion_action")})
setMethod(
    f = "gestion_action",
    signature = c(action = "Action", esg_simu = "list", an = "integer"),
    definition = function(action, esg_simu, an){


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
        log_rdt <- exp(esg_simu$eq_index[an])

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
        div <- esg_simu$eq_dividends[an]

        # Calcul des dividendes
        dividendes <- div * .subset2(action@ptf, which(name_ptf == "valeur_marche"))





        # Output
        return(list(action = action,
                    pmvl = sum(pmvl),
                    dividendes = sum(dividendes)))
    }
)
