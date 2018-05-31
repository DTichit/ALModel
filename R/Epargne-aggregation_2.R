##' Fonction \code{aggregation_epargne_2}.
##'
##' Cette fonction permet d'effectuer la derniere aggregation des model-point pour un portefeuille de contrats d'epargne.
##' Cette aggregation se fait uniquement par tmg. Elle s'effectue apres le calcul des probas.
##'
##' @name aggregation_epargne_2
##' @docType methods
##' @param epargne est un objet de type \code{\link{Epargne}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Epargne-class.R
##'
setGeneric(name = "aggregation_epargne_2", def = function(epargne) {standardGeneric("aggregation_epargne_2")})
setMethod(
    f = "aggregation_epargne_2",
    signature = c(epargne = "Epargne"),
    definition = function(epargne){


        # Extraction de donnees du ptf
        name_ptf <- names(epargne@ptf)
        pm_ptf  <- .subset2(epargne@ptf, which(name_ptf == "pm"))
        tmg_ptf <- .subset2(epargne@ptf, which(name_ptf == "tmg"))
        nb_contr_ptf <- .subset2(epargne@ptf, which(name_ptf == "nb_contr"))





        ## ######################################################
        ## ######################################################
        ##
        ##                  Gestion des probas
        ##
        ## ######################################################
        ## ######################################################

        # Mise a jour des nouvelles probas
        epargne@proba@deces_contr <- agregation_proba(epargne = epargne, proba = epargne@proba@deces_contr, variable = "nb_contr")
        epargne@proba@deces_pm <- agregation_proba(epargne = epargne, proba = epargne@proba@deces_pm, variable = "pm")
        epargne@proba@rachat_tot_pm <- agregation_proba(epargne = epargne, proba = epargne@proba@rachat_tot_pm, variable = "pm")
        epargne@proba@rachat_tot_contr <- agregation_proba(epargne = epargne, proba = epargne@proba@rachat_tot_contr, variable = "nb_contr")
        epargne@proba@rachat_part <- agregation_proba(epargne = epargne, proba = epargne@proba@rachat_part, variable = "pm")




        ## ######################################################
        ## ######################################################
        ##
        ##                  Gestion du PTF
        ##
        ## ######################################################
        ## ######################################################

        # Aggregation des donnees du PTF
        temp <- (epargne@ptf %>% group_by(tmg)
                 %>% summarise(revalo_prec = weighted.mean(revalo_prec, pm),
                               prime = sum(prime),
                               chgt_administration = weighted.mean(chgt_administration, pm),
                               chgt_acquisition = weighted.mean(chgt_acquisition, pm),
                               chgt_rachats = weighted.mean(chgt_rachats, pm),
                               chgt_deces = weighted.mean(chgt_deces, pm),
                               frais_uni_gestion = weighted.mean(frais_uni_gestion, nb_contr),
                               frais_uni_rachats = weighted.mean(frais_uni_rachats, nb_contr),
                               frais_uni_deces = weighted.mean(frais_uni_deces, nb_contr),
                               nb_contr = sum(nb_contr),
                               pm = sum(pm)))

        # Mise a jour de l'attribut
        epargne@ptf <- data.frame(id_mp = paste("ep", 1L:nrow(temp), sep = "-"), temp)

        # Tri par tmg
        epargne@ptf <- epargne@ptf[order(epargne@ptf$tmg),]




        # Output
        return(epargne)
    }
)

