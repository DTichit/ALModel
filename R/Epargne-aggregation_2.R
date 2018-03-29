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
        ##                  Gestion du PTF
        ##
        ## ######################################################
        ## ######################################################

        # Aggregation des donnees du PTF
        temp <- (epargne@ptf %>% group_by(tmg)
                 %>% summarise(revalo_prec = weighted.mean(revalo_prec, pm),
                               prime = sum(prime),
                               chgt_gestion = weighted.mean(chgt_gestion, pm),
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




        ## ######################################################
        ## ######################################################
        ##
        ##                  Gestion des probas
        ##
        ## ######################################################
        ## ######################################################




        ## ###########################
        ##      Deces par contrats
        ## ###########################

        # Aggregation des donnees
        temp <- cbind(epargne@proba@deces_contr, nb_contr = nb_contr_ptf, tmg = tmg_ptf)
        temp <- (temp[,-1L] %>% group_by(tmg)
                 %>% summarise_all(funs(weighted.mean(x = ., w = nb_contr))))

        # Tri par tmg
        temp <- temp[order(temp$tmg),]

        # Mise a jour de l'objet
        epargne@proba@deces_contr <- data.frame(id_mp = paste("ep", 1L:nrow(temp), sep = "-"), temp)

        # Ne conserver que les variables "utiles"
        name_deces <- names(epargne@proba@deces_contr)
        num_del <- match(c("tmg", "nb_contr"), name_deces)
        epargne@proba@deces_contr <- epargne@proba@deces_contr[,-num_del]



        ## ###########################
        ##      Deces par pm
        ## ###########################

        # Aggregation des donnees
        temp <- cbind(epargne@proba@deces_pm, pm = pm_ptf, tmg = tmg_ptf)
        temp <- (temp[,-1L] %>% group_by(tmg)
                 %>% summarise_all(funs(weighted.mean(x = ., w = pm))))

        # Tri par tmg
        temp <- temp[order(temp$tmg),]

        # Mise a jour de l'objet
        epargne@proba@deces_pm <- data.frame(id_mp = paste("ep", 1L:nrow(temp), sep = "-"), temp)

        # Ne conserver que les variables "utiles"
        name_deces <- names(epargne@proba@deces_pm)
        num_del <- match(c("tmg", "pm"), name_deces)
        epargne@proba@deces_pm <- epargne@proba@deces_pm[,-num_del]



        ## ###########################
        ##    Rachats totaux par pm
        ## ###########################

        # Aggregation des donnees
        temp <- cbind(epargne@proba@rachat_tot_pm, pm = pm_ptf, tmg = tmg_ptf)
        temp <- (temp[,-1L] %>% group_by(tmg)
                 %>% summarise_all(funs(weighted.mean(x = ., w = pm))))

        # Tri par tmg
        temp <- temp[order(temp$tmg),]

        # Mise a jour de l'objet
        epargne@proba@rachat_tot_pm <- data.frame(id_mp = paste("ep", 1L:nrow(temp), sep = "-"), temp)

        # Ne conserver que les variables "utiles"
        name_rachat_tot <- names(epargne@proba@rachat_tot_pm)
        num_del <- match(c("tmg", "pm"), name_rachat_tot)
        epargne@proba@rachat_tot_pm <- epargne@proba@rachat_tot_pm[,-num_del]



        ## ###########################
        ## Rachats totaux par contrats
        ## ###########################

        # Aggregation des donnees
        temp <- cbind(epargne@proba@rachat_tot_contr, nb_contr = nb_contr_ptf, tmg = tmg_ptf)
        temp <- (temp[,-1L] %>% group_by(tmg)
                 %>% summarise_all(funs(weighted.mean(x = ., w = nb_contr))))

        # Tri par tmg
        temp <- temp[order(temp$tmg),]

        # Mise a jour de l'objet
        epargne@proba@rachat_tot_contr <- data.frame(id_mp = paste("ep", 1L:nrow(temp), sep = "-"), temp)

        # Ne conserver que les variables "utiles"
        name_rachat_tot <- names(epargne@proba@rachat_tot_contr)
        num_del <- match(c("tmg", "nb_contr"), name_rachat_tot)
        epargne@proba@rachat_tot_contr <- epargne@proba@rachat_tot_contr[,-num_del]



        ## ###########################
        ##      Rachats partiels
        ## ###########################

        # Aggregation des donnees
        temp <- cbind(epargne@proba@rachat_part, pm = pm_ptf, tmg = tmg_ptf)
        temp <- (temp[,-1L] %>% group_by(tmg)
                 %>% summarise_all(funs(weighted.mean(x = ., w = pm))))

        # Tri par tmg
        temp <- temp[order(temp$tmg),]

        # Mise a jour de l'objet
        epargne@proba@rachat_part <- data.frame(id_mp = paste("ep", 1L:nrow(temp), sep = "-"), temp)

        # Ne conserver que les variables "utiles"
        name_rachat_part <- names(epargne@proba@rachat_part)
        num_del <- match(c("tmg", "pm"), name_rachat_part)
        epargne@proba@rachat_part <- epargne@proba@rachat_part[,-num_del]






        # Output
        return(epargne)
    }
)

