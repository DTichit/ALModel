##' Fonction \code{gestion_fin_projection_actif}
##'
##' Cette fonction permet de gerer la fin de projection du modele sur l'actif de la compagnie d'assurance.
##'
##' @name gestion_fin_projection_actif
##' @docType methods
##' @param actif est un objet de type \code{\link{Actif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Actif-class.R
##'
setGeneric(name = "gestion_fin_projection_actif", def = function(actif) {standardGeneric("gestion_fin_projection_actif")})
setMethod(
    f = "gestion_fin_projection_actif",
    signature = c(actif = "Actif"),
    definition = function(actif){



        ## ######################################################
        ## ######################################################
        ##
        ##                  Vente des actifs
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##          Actions
        ## ###########################

        # Extraction donnees
        names_ptf <- names(actif@ptf_actif@action@ptf)
        vm <- sum(.subset2(actif@ptf_actif@action@ptf, which(names_ptf == "valeur_marche")))
        vc <- sum(.subset2(actif@ptf_actif@action@ptf, which(names_ptf == "valeur_comptable")))

        # Vente actions
        vente_actions <- vm

        # PMVR
        pmvr_actions <- vm - vc


        # Mise a 0 des attributs
        actif@ptf_actif@action@ptf <- actif@ptf_actif@action@ptf[-(1L:nrow(actif@ptf_actif@action@ptf)),]




        ## ###########################
        ##         Immobilier
        ## ###########################

        # Extraction donnees
        names_ptf <- names(actif@ptf_actif@immobilier@ptf)
        vm <- sum(.subset2(actif@ptf_actif@immobilier@ptf, which(names_ptf == "valeur_marche")))
        vc <- sum(.subset2(actif@ptf_actif@immobilier@ptf, which(names_ptf == "valeur_comptable")))

        # Vente immobilier
        vente_immo <- vm

        # PMVR
        pmvr_immo <- vm - vc


        # Mise a 0 des attributs
        actif@ptf_actif@immobilier@ptf <- actif@ptf_actif@immobilier@ptf[-(1L:nrow(actif@ptf_actif@immobilier@ptf)),]




        ## ###########################
        ##         Obligations
        ## ###########################

        # Extraction donnees
        names_ptf <- names(actif@ptf_actif@obligation@ptf)
        vm <- sum(.subset2(actif@ptf_actif@obligation@ptf, which(names_ptf == "valeur_marche")))
        vc <- sum(.subset2(actif@ptf_actif@obligation@ptf, which(names_ptf == "valeur_nette_comptable")))

        # Vente obligation
        vente_oblig <- vm

        # PMVR
        pmvr_oblig <- vm - vc


        # Mise a 0 des attributs
        actif@ptf_actif@obligation@ptf <- actif@ptf_actif@obligation@ptf[-(1L:nrow(actif@ptf_actif@obligation@ptf)),]




        ## ###########################
        ##         Tresorerie
        ## ###########################

        # Extraction du solde
        solde_treso <- actif@ptf_actif@tresorerie@solde

        # Mise a 0 des attributs
        actif@ptf_actif@tresorerie@solde <- numeric()







        # Output
        return(list(actif = actif,
                    vente = list(action = vente_actions,
                                 obligation = vente_oblig,
                                 immobilier = vente_immo,
                                 tresorerie = solde_treso),
                    pmvr = list(actions = pmvr_actions,
                                obligations = pmvr_oblig,
                                immobilier = pmvr_immo)))
    }
)
