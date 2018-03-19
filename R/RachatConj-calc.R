##' Fonction \code{calc_rachat_conj}.
##'
##' Cette fonction permet de calculer les taux de rachats conjoncturels.
##'
##' @name calc_rachat_conj
##' @docType methods
##' @param rachat_conj est un objet de type \code{\link{RachatConj}}.
##' @param tx_cible est un \code{numeric}.
##' @param tx_serv est un \code{numeric}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include RachatConj-class.R
##'
setGeneric(name = "calc_rachat_conj", def = function(rachat_conj, tx_cible, tx_serv) {standardGeneric("calc_rachat_conj")})
setMethod(
    f = "calc_rachat_conj",
    signature = c(rachat_conj = "RachatConj", tx_cible = "numeric", tx_serv = "numeric"),
    definition = function(rachat_conj, tx_cible, tx_serv){



        ## ###########################
        ##      Initialisation
        ## ###########################

        # Calcul de la difference
        diff_taux <- tx_serv - tx_cible

        # Initialisation du vecteur des taux de rachats
        rachat <- rep(x = NA, length = length(tx_cible))




        ## ######################################################
        ## ######################################################
        ##
        ##              Calcul des taux de rachats
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ## 1 - diff_taux < alpha
        ## ###########################

        # Indicatrice
        ind <- which(diff_taux < rachat_conj@alpha)

        # Calcul du taux
        if(length(ind) > 0L)
            rachat[ind] <- rachat_conj@RCmax



        ## ###########################
        ## 2 - alpha < diff_taux < beta
        ## ###########################

        # Indicatrice
        ind <- which((rachat_conj@alpha <= diff_taux) & (diff_taux < rachat_conj@beta))

        # Calcul du taux
        if(length(ind) > 0L)
            rachat[ind] <- rachat_conj@RCmax * ((diff_taux[ind] - rachat_conj@beta)/(rachat_conj@alpha - rachat_conj@beta))



        ## ###########################
        ## 3 - beta < diff_taux < gamma
        ## ###########################

        # Indicatrice
        ind <- which((rachat_conj@beta <= diff_taux) & (diff_taux < rachat_conj@gamma))

        # Calcul du taux
        if(length(ind) > 0L)
            rachat[ind] <- 0



        ## ###########################
        ## 4 - gamma < diff_taux < delta
        ## ###########################

        # Indicatrice
        ind <- which((rachat_conj@gamma <= diff_taux) & (diff_taux < rachat_conj@delta))

        # Calcul du taux
        if(length(ind) > 0L)
            rachat[ind] <- rachat_conj@RCmin * ((diff_taux[ind] - rachat_conj@gamma)/(rachat_conj@delta - rachat_conj@gamma))



        ## ###########################
        ## 5 - diff_taux > delta
        ## ###########################

        # Indicatrice
        ind <- which((diff_taux >= rachat_conj@delta))

        # Calcul du taux
        if(length(ind) > 0L)
            rachat[ind] <- rachat_conj@RCmin




        # Output
        return(list(rachat = rachat))
    }
)
