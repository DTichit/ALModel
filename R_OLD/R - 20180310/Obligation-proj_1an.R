##' Fonction \code{proj_1an_obligation}
##'
##' Cette fonction permet de projeter horizon 1 an un portefeuille d'obligations.
##'
##' @name proj_1an_obligation
##' @docType methods
##' @param obligation est un objet de type \code{\link{Obligation}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Obligation-class.R
##'
setGeneric(name = "proj_1an_obligation", def = function(obligation) {standardGeneric("proj_1an_obligation")})
setMethod(
    f = "proj_1an_obligation",
    signature = c(obligation = "Obligation"),
    definition = function(obligation){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf_oblig <- names(obligation@ptf)





        ## ######################################################
        ## ######################################################
        ##
        ##               Evaluation des coupons
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        nb_ptf      <- .subset2(obligation@ptf, which(name_ptf_oblig == "nombre"))
        nominal_ptf <- .subset2(obligation@ptf, which(name_ptf_oblig == "nominal"))
        tx_coup_ptf <- .subset2(obligation@ptf, which(name_ptf_oblig == "tx_coupon"))

        # Calcul des coupons
        coupons <- nb_ptf * tx_coup_ptf * nominal_ptf






        ## ######################################################
        ## ######################################################
        ##
        ##       Vente des obligations arrivees a maturite
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        mat_res_oblig_new <- .subset2(obligation@ptf, which(name_ptf_oblig == "maturite_residuelle"))

        # Determination des oblig a vendre
        ind_oblig_sell <- which(mat_res_oblig_new == 0L)

        # S'il y a des obligs a vendre :
        if(length(ind_oblig_sell) > 0L) {

            # Gain sur les obligations vendues
            vente_oblig <- vm_oblig_new[ind_oblig_sell]

            # Suppression des oblig du PTF
            obligation@ptf <- obligation@ptf[-ind_oblig_sell,]

        } else {

            # Aucune vente
            vente_oblig <- 0

        }




        # Output
        return(list(obligation = obligation,
                    gain = list(coupons = coupons,
                                pmvl_oblig = pmvl_oblig,
                                vente_oblig = vente_oblig)))
    }
)
