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
        name_ptf <- names(obligation@ptf)
        mat_res  <- .subset2(obligation@ptf, which(name_ptf == "mat_res"))







        ## ######################################################
        ## ######################################################
        ##
        ##              Creation d'un nouveau PTF
        ##
        ## ######################################################
        ## ######################################################

        if(length(mat_res) != max(mat_res)) {

            # Creation des nouvelles maturites residuelles
            new_mat_res <- 1L:max(mat_res)

            # Creation du nouveau PTF
            ptf <- data.frame(id_mp = paste("ob", new_mat_res, sep = "-"), mat_res = new_mat_res,
                              valeur_comptable = 0, valeur_marche = 0, coupon = 0, nominal = 0)

            # ID
            id_new <- match(mat_res, new_mat_res)
            id_old <- match(new_mat_res, mat_res)
            id_old <- id_old[!is.na(id_old)]

            # Mise a jour du nouveau PTF
            ptf[id_new, "valeur_comptable"] <- obligation@ptf$valeur_comptable[id_old]
            ptf[id_new, "valeur_marche"]    <- obligation@ptf$valeur_marche[id_old]
            ptf[id_new, "coupon"]           <- obligation@ptf$coupon[id_old]
            ptf[id_new, "nominal"]          <- obligation@ptf$nominal[id_old]

            # Mise a jour de l'attribut
            obligation@ptf <- ptf

        }



        ## ######################################################
        ## ######################################################
        ##
        ##       Vente des obligations arrivees a maturite
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de donnees
        name_ptf <- names(obligation@ptf)
        mat_res <- .subset2(obligation@ptf, which(name_ptf == "mat_res"))

        # Determination des oblig a vendre
        ind_oblig_sell <- which(mat_res == 1L)

        # Extraction de donnees
        vm_sell <- sum(obligation@ptf$valeur_marche[ind_oblig_sell])
        vc_sell <- sum(obligation@ptf$valeur_comptable[ind_oblig_sell])

        # Calcul des plus ou moins values
        pmv_oblig <- vm_sell - vc_sell






        ## ######################################################
        ## ######################################################
        ##
        ##          Mise a jour de la maturite residuelle
        ##
        ## ######################################################
        ## ######################################################

        # Nouvelles maturites residuelles
        mat_res_new <- mat_res - 1L
        mat_res_new <- mat_res_new[which(mat_res_new != 0)]

        # ID
        id_new <- match(mat_res_new, mat_res)
        id_old <- match(2L:max(mat_res), mat_res)

        # Mise a jour du PTF
        n_maj <- match(c("id_mp", "mat_res"), name_ptf)
        obligation@ptf[id_new, -n_maj] <- obligation@ptf[id_old, -n_maj]


        # Mettre a 0 la derniere ligne
        if(obligation@ptf$nominal[which(mat_res == max(mat_res))] > 0) {
            maj <- match(c("valeur_comptable", "valeur_marche", "coupon", "nominal"), name_ptf)
            obligation@ptf[which(mat_res == max(mat_res)), maj] <- 0
        }



        # Output
        return(list(obligation = obligation,
                    flux = list(vente = vm_sell,
                                pmv = pmv_oblig)))
    }
)
