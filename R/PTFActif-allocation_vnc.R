##' Fonction \code{allocation_vnc_ptf_actif}
##'
##' Cette fonction permet de determiner la VNC des differents produits financiers.
##'
##' @name allocation_vnc_ptf_actif
##' @docType methods
##' @param ptf_actif est un objet de type \code{PTFActif}.
##' @author Damien Tichit pour Sia Partners
##' @include Actif-class.R
##'
setGeneric(name = "allocation_vnc_ptf_actif", def = function(ptf_actif) {standardGeneric("allocation_vnc_ptf_actif")})
setMethod(
    f = "allocation_vnc_ptf_actif",
    signature = c(ptf_actif = "PTFActif"),
    definition = function(ptf_actif){


        ## ###########################
        ##    Determination des VM
        ## ###########################

        # VNC des actions
        ptf <- ptf_actif@action@ptf
        vnc_action <- sum(.subset2(ptf, which(names(ptf) == "valeur_comptable")))

        # VNC des obligations
        ptf <- ptf_actif@obligation@ptf
        vnc_oblig <- sum(.subset2(ptf, which(names(ptf) == "valeur_nette_comptable")))

        # VNC des immo
        ptf <- ptf_actif@immobilier@ptf
        vnc_immo <- sum(.subset2(ptf, which(names(ptf) == "valeur_comptable")))

        # Solde tresorerie
        solde_treso <- ptf_actif@tresorerie@solde

        # Total
        vnc_total <- vnc_action + vnc_oblig + vnc_immo + solde_treso



        ## ###########################
        ##   Aggregation des donnees
        ## ###########################

        # Liste aggregeant les VNC
        vnc_actif <- list(action = vnc_action,
                          obligation = vnc_oblig,
                          immobilier = vnc_immo,
                          tresorerie = solde_treso,
                          total = vnc_total)

        # Liste aggregeant les proportions
        prop_vnc_actif <- list(action = vnc_action / vnc_total,
                               obligation = vnc_oblig / vnc_total,
                               immobilier = vnc_immo / vnc_total,
                               tresorerie = solde_treso / vnc_total,
                               total = 1)




        # Output
        return(list(vnc_actif = vnc_actif,
                    prop_vnc_actif = prop_vnc_actif))
    }
)
