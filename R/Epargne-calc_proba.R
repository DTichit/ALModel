##' Fonction \code{calc_proba_epargne}.
##'
##'Cette fonction permet de completer l'objet contenant les probabilites relatives au portefeuille epargne.
##'
##' @name calc_proba_epargne
##' @docType methods
##' @param epargne est un objet de type \code{\link{Epargne}}.
##' @param hyp_passif est un objet de type \code{\link{HypPassif}}.
##' @param an est un objet de type \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Epargne-class.R HypPassif-class.R
##'
setGeneric(name = "calc_proba_epargne", def = function(epargne, hyp_passif, an) {standardGeneric("calc_proba_epargne")})
setMethod(
    f = "calc_proba_epargne",
    signature = c(epargne = "Epargne", hyp_passif = "HypPassif", an = "integer"),
    definition = function(epargne, hyp_passif, an){

        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(epargne@ptf)



        ## ######################################################
        ## ######################################################
        ##
        ##               Calcul des probas
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        sexe_ptf_epargne <- .subset2(epargne@ptf, which(name_ptf == "sexe"))
        age_ptf_epargne  <- .subset2(epargne@ptf, which(name_ptf == "age"))
        anc_ptf_epargne  <- .subset2(epargne@ptf, which(name_ptf == "anc"))


        ## ###########################
        ## Probas deces
        ## ###########################

        # Selection des contrats par sexe
        contrats_h <- which(sexe_ptf_epargne == "H")
        contrats_f <- which(sexe_ptf_epargne == "F")

        # Calcul des taux de deces par model point
        tx_deces <- rep(x = NA, length = nrow(epargne@ptf))
        tx_deces[contrats_h] <- calc_qx(tab_morta = hyp_passif@tab_morta_h, age = age_ptf_epargne[contrats_h])
        tx_deces[contrats_f] <- calc_qx(tab_morta = hyp_passif@tab_morta_f, age = age_ptf_epargne[contrats_f])



        ## ###########################
        ## Probas rachats partiels
        ## ###########################

        # Calcul des taux de rachats par model point
        tx_rachat_part <- calc_rx(tab_rachat = hyp_passif@tab_rachat_part, anc = anc_ptf_epargne)



        ## ###########################
        ## Probas rachats totaux
        ## ###########################

        # Calcul des taux de rachats par model point
        tx_rachat_tot  <- calc_rx(tab_rachat = hyp_passif@tab_rachat_tot, anc = anc_ptf_epargne)









        ## ######################################################
        ## ######################################################
        ##
        ##               Completer l'objet
        ##
        ## ######################################################
        ## ######################################################

        # Si c'est la 1ere annee, on initialise les data.frame
        if(an == 1L) {

            # Data frame permettant d'initialiser
            df <- data.frame(id_mp = epargne@ptf$id_mp)

            # Mise a jour des objets
            epargne@proba@deces_contr       <- df
            epargne@proba@deces_pm          <- df
            epargne@proba@rachat_part       <- df
            epargne@proba@rachat_tot_contr  <- df
            epargne@proba@rachat_tot_pm     <- df

        }

        # Completer les DF
        epargne@proba@deces_contr[,an+1L]       <- tx_deces
        epargne@proba@deces_pm[,an+1L]          <- tx_deces
        epargne@proba@rachat_part[,an+1L]       <- tx_rachat_part
        epargne@proba@rachat_tot_contr[,an+1L]  <- tx_rachat_tot
        epargne@proba@rachat_tot_pm[,an+1L]     <- tx_rachat_tot




        # Output
        return(list(epargne = epargne))
    }
)
