##' Fonction \code{vieillissement_obligation}
##'
##' Cette fonction permet de vieillir un portfeuille obligataire : mise a jour de la maturite residuelle et vente des obligations arrivees a maturite.
##'
##' Attention : le PTF doit etre aggrege
##'
##' @name vieillissement_obligation
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "vieillissement_obligation", def = function(obligation) {standardGeneric("vieillissement_obligation")})
setMethod(
    f = "vieillissement_obligation",
    signature = c(obligation = "Obligation"),
    definition = function(obligation){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf_oblig <- names(obligation@ptf)




        ## ######################################################
        ## ######################################################
        ##
        ##          Mise a jour de la duree de detention
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        dur_det <- .subset2(obligation@ptf, which(name_ptf_oblig == "duree_detention"))

        # Mise a jour des maturites
        obligation@ptf$duree_detention <- dur_det + 1L





        ## ######################################################
        ## ######################################################
        ##
        ##       Vente des obligations arrivees a maturite
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        maturite_new <- .subset2(obligation@ptf, which(name_ptf_oblig == "maturite"))
        dur_det_new  <- .subset2(obligation@ptf, which(name_ptf_oblig == "duree_detention"))

        # Determination des oblig a vendre
        ind_oblig_sell <- which(maturite_new == dur_det_new)

        # S'il y a des obligs a vendre :
        if(length(ind_oblig_sell) > 0L) {

            # Extraction de donnees
            vr_ptf <- .subset2(obligation@ptf, which(name_ptf_oblig == "valeur_remboursement"))

            # Gain sur les obligations vendues
            vente <- sum(vr_ptf[ind_oblig_sell])

            # Suppression des oblig du PTF
            obligation@ptf <- obligation@ptf[-ind_oblig_sell,]

        }




        # Output
        return(list(obligation = obligation,
                    flux = list(vente = if.is_null(get0("vente"), 0))))
    }
)
