##' Fonction \code{proj_1an_immobilier}
##'
##' Cette fonction permet de projeter horizon 1 an un portefeuille d'immobiliers.
##'
##' @name proj_1an_immobilier
##' @docType methods
##' @param immobilier est un objet de type \code{\link{Immobilier}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Immobilier-class.R
##'
setGeneric(name = "proj_1an_immobilier", def = function(immobilier) {standardGeneric("proj_1an_immobilier")})
setMethod(
    f = "proj_1an_immobilier",
    signature = c(immobilier = "Immobilier"),
    definition = function(immobilier){

        ## ###########################
        ##   Extrimmobilier des donnnes
        ## ###########################
        name_ptf_immobilier <- names(immobilier@ptf)





        ## ######################################################
        ## ######################################################
        ##
        ##            Premiere mise a jour des MP
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ## Mise a jour des VM
        ## ###########################

        # Extrimmobilier des donnees
        vm_immobilier_prev <- .subset2(immobilier@ptf, which(name_ptf_immobilier == "valeur_marche"))

        # Calcul des nouvelles VM
        vm_immobilier_new  <- vm_immobilier_prev

        # Mise a jour de l'attribut
        immobilier@ptf$valeur_marche <- vm_immobilier_new









        ## ######################################################
        ## ######################################################
        ##
        ##        Evaluation des gains : loyers et PMVL
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Loyers
        ## ###########################

        # Extrimmobilier de donnees
        vm_immobilier_new   <- .subset2(immobilier@ptf, which(name_ptf_immobilier == "valeur_marche"))
        loyer_ptf           <- .subset2(immobilier@ptf, which(name_ptf_immobilier == "loyer"))
        nb_ptf              <- .subset2(immobilier@ptf, which(name_ptf_immobilier == "nombre"))

        # Calcul des loyers
        loyers <- nb_ptf * loyer_ptf


        ## ###########################
        ##            PMVL
        ## ###########################

        # Calcul des PMVL
        pmvl_immobilier <- vm_immobilier_new - vm_immobilier_prev




        # Output
        return(list(immobilier = immobilier,
                    gain = list(loyers = loyers,
                                pmvl_immobilier = pmvl_immobilier)))
    }
)
