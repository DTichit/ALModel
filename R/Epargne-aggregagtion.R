##' Fonction \code{aggregation_epargne}.
##'
##' Cette fonction permet d'aggreger les model-point pour un portfeuille d'actions.
##'
##' @name aggregation_epargne
##' @docType methods
##' @param epargne est un objet de type \code{\link{Epargne}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R
##'
setGeneric(name = "aggregation_epargne", def = function(epargne) {standardGeneric("aggregation_epargne")})
setMethod(
    f = "aggregation_epargne",
    signature = c(epargne = "Epargne"),
    definition = function(epargne){

        # Aggration des donnees
        temp <- (epargne@ptf %>% group_by(age, anc, tmg, sexe)
                 %>% summarise(prime = sum(prime),
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
