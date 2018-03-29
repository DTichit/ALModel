##' Fonction \code{aggregation_epargne_1}.
##'
##' Cette fonction permet d'effectuer une premiere aggregation des model-point pour un portefeuille de contrats d'epargne.
##' Cette aggregation se fait par tmg, sexe, age et anciennete. Elle s'effectue avant le calcul des probas.
##'
##' @name aggregation_epargne_1
##' @docType methods
##' @param epargne est un objet de type \code{\link{Epargne}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Epargne-class.R
##'
setGeneric(name = "aggregation_epargne_1", def = function(epargne) {standardGeneric("aggregation_epargne_1")})
setMethod(
    f = "aggregation_epargne_1",
    signature = c(epargne = "Epargne"),
    definition = function(epargne){

        # Aggration des donnees
        temp <- (epargne@ptf %>% group_by(sexe, age, anc, tmg)
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

        # Output
        return(epargne)
    }
)
