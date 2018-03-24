##' Fonction \code{vieillissement_obligation}
##'
##' Cette fonction permet de vieillir un portfeuille obligataire : mise a jour de la maturite residuelle et vente des obligations arrivees a maturite.
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
        ##          Mise a jour de la maturite residuelle
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        mat_res_oblig <- .subset2(obligation@ptf, which(name_ptf_oblig == "mat_res"))

        # Mise a jour des maturites
        obligation@ptf$mat_res <- mat_res_oblig - 1L






        ## ######################################################
        ## ######################################################
        ##
        ##       Vente des obligations arrivees a maturite
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        mat_res_oblig_new <- .subset2(obligation@ptf, which(name_ptf_oblig == "mat_res"))

        # Determination des oblig a vendre
        ind_oblig_sell <- which(mat_res_oblig_new == 0L)

        # S'il y a des obligs a vendre :
        if(length(ind_oblig_sell) > 0L) {

            # Extraction de donnees
            vm_oblig <- .subset2(obligation@ptf, which(name_ptf_oblig == "valeur_marche"))

            # Gain sur les obligations vendues
            vente_oblig <- vm_oblig[ind_oblig_sell]

            # Suppression des oblig du PTF
            obligation@ptf <- obligation@ptf[-ind_oblig_sell,]

        } else {

            # Aucune vente
            vente_oblig <- 0

        }




        # Output
        return(list(obligation = obligation,
                    flux = list(vente = vente_oblig)))
    }
)
