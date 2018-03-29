##' Fonction \code{aggregation_obligation}.
##'
##' Cette fonction permet d'aggreger les model-point pour un portfeuille d'obligations.
##'
##' @name aggregation_obligation
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "aggregation_obligation", def = function(obligation) {standardGeneric("aggregation_obligation")})
setMethod(
    f = "aggregation_obligation",
    signature = c(obligation = "Obligation"),
    definition = function(obligation){

        # Aggregation des donnees
        temp <- as.data.frame(obligation@ptf %>% group_by(mat_res) %>% summarise(valeur_comptable = sum(valeur_comptable),
                                                                                 valeur_marche = sum(valeur_marche),
                                                                                 coupon = weighted.mean(coupon, nominal),
                                                                                 nominal = sum(nominal)) %>% arrange(mat_res))

        # Creation du dataframe
        obligation@ptf <- temp

        # Output
        return(obligation)
    }
)
