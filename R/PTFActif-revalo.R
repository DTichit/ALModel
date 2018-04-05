##' Fonction \code{revalo_ptf_actif}
##'
##' Cette fonction permet de projeter de revaloriser le portfeuille financier.
##' Elle calcule egalement les plus ou moins value latentes (PMVL) engendrees.
##'
##' @name revalo_ptf_actif
##' @docType methods
##' @param ptf_actif est un objet de type \code{\link{PTFActif}}.
##' @param hyp_actif est un objet de type \code{\link{HypActif}}.
##' @param an est un \code{integer} reprensentant l'annee sur laquelle on travaille.
##' @author Damien Tichit pour Sia Partners
##' @include PTFActif-class.R HypActif-class.R
##'
setGeneric(name = "revalo_ptf_actif", def = function(ptf_actif, hyp_actif, an) {standardGeneric("revalo_ptf_actif")})
setMethod(
    f = "revalo_ptf_actif",
    signature = c(ptf_actif = "PTFActif", hyp_actif = "HypActif", an = "integer"),
    definition = function(ptf_actif, hyp_actif, an){


        ## ######################################################
        ## ######################################################
        ##
        ##              Revalorisation des actifs
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##   Gestion des obligations
        ## ###########################

        # Extraction de la courbe des taux
        name_ctz <- names(hyp_actif@esg_simu$ctz_nom)
        num <- which(.subset2(hyp_actif@esg_simu$ctz_nom, which(name_ctz == "ProjYr")) == an)
        yield_curve <- .subset2(hyp_actif@esg_simu$ctz_nom, which(name_ctz == "ZeroCoupon"))[num]

        # Revalorisation des obligations
        res_revalo_oblig <- revalo_obligation(obligation = ptf_actif@obligation, yield_curve = yield_curve)

        # Mise a jour de l'attribut
        ptf_actif@obligation <- res_revalo_oblig[["obligation"]]




        ## ###########################
        ##    Gestion des actions
        ## ###########################

        # Extraction du log rendement
        log_rdt <- hyp_actif@esg_simu$eq_index[an]

        # Revalorisation des actions
        res_revalo_action <- revalo_action(action = ptf_actif@action, log_rdt = log_rdt)

        # Mise a jour de l'attribut
        ptf_actif@action <- res_revalo_action[["action"]]



        ## ###########################
        ##    Gestion de l'immo
        ## ###########################

        # Extraction du log rendement
        log_rdt <- hyp_actif@esg_simu$im_index[an]

        # Revalorisation de l'immo
        res_revalo_immo <- revalo_immobilier(immobilier = ptf_actif@immobilier, log_rdt = log_rdt)

        # Mise a jour de l'attribut
        ptf_actif@immobilier <- res_revalo_immo[["immobilier"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Agregation des PMVL
        ##
        ## ######################################################
        ## ######################################################

        # PMVL par produit
        pmvl <- list(action = res_revalo_action[["pmvl"]],
                     obligation = res_revalo_oblig[["pmvl"]],
                     immobilier = res_revalo_immo[["pmvl"]])



        # Output
        return(list(ptf_actif = ptf_actif,
                    pmvl = pmvl))
    }
)
