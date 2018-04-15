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
        ##                  Mise a jour de la VNC
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        vnc_ptf <- .subset2(obligation@ptf, which(name_ptf_oblig == "valeur_nette_comptable"))
        tri_ptf <- .subset2(obligation@ptf, which(name_ptf_oblig == "tri"))

        # Mise a jour des maturites
        obligation@ptf$valeur_nette_comptable <- vnc_ptf / (1 + tri_ptf)






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
            vm_oblig <- .subset2(obligation@ptf, which(name_ptf_oblig == "valeur_marche"))
            vnc_oblig <- .subset2(obligation@ptf, which(name_ptf_oblig == "valeur_nette_comptable"))

            # Gain sur les obligations vendues
            vm_sell <- vm_oblig[ind_oblig_sell]

            # Calcul des plus ou moins values
            pmv_oblig <- vm_sell - vnc_oblig[ind_oblig_sell]

            # Suppression des oblig du PTF
            obligation@ptf <- obligation@ptf[-ind_oblig_sell,]

        } else {

            # Aucune vente
            vm_sell <- 0 ; pmv_oblig <- 0

        }




        # Output
        return(list(obligation = obligation,
                    flux = list(vente = sum(vm_sell),
                                pmv = sum(pmv_oblig))))
    }
)
