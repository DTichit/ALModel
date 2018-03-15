##' Fonction \code{allocation_ptf_actif}
##'
##' Cette fonction permet de determiner l'allocation des differents produits financiers.
##'
##' @name allocation_ptf_actif
##' @docType methods
##' @param ptf_actif est un objet de type \code{PTFActif}.
##' @author Damien Tichit pour Sia Partners
##' @include Actif-class.R
##'
setGeneric(name = "allocation_ptf_actif", def = function(ptf_actif) {standardGeneric("allocation_ptf_actif")})
setMethod(
    f = "allocation_ptf_actif",
    signature = c(ptf_actif = "PTFActif"),
    definition = function(ptf_actif){


        ## ###########################
        ##    Determination des VM
        ## ###########################

        # VM des actions
        ptf <- ptf_actif@action@ptf
        vm_action <- sum(.subset2(ptf, which(names(ptf) == "valeur_marche")))

        # VM des obligations
        ptf <- ptf_actif@obligation@ptf
        vm_oblig <- sum(.subset2(ptf, which(names(ptf) == "valeur_marche")))

        # VM des immo
        ptf <- ptf_actif@immobilier@ptf
        vm_immo <- sum(.subset2(ptf, which(names(ptf) == "valeur_marche")))

        # Solde tresorerie
        ptf <- ptf_actif@tresorerie@ptf
        solde_treso <- sum(.subset2(ptf, which(names(ptf) == "solde")))

        # Total
        vm_total <- vm_action + vm_oblig + vm_immo + solde_treso



        ## ###########################
        ##   Aggregation des donnees
        ## ###########################

        # Liste aggregeant les VM
        vm_actif <- list(action = vm_action,
                          obligation = vm_oblig,
                          immobilier = vm_immo,
                          tresorerie = solde_treso,
                          total = vm_total)

        # Liste aggregeant les proportions
        prop_vm_actif <- list(action = vm_action / vm_total,
                               obligation = vm_oblig / vm_total,
                               immobilier = vm_immo / vm_total,
                               tresorerie = solde_treso / vm_total,
                               total = 1)




        # Output
        return(list(vm_actif = vm_actif,
                    prop_vm_actif = prop_vm_actif))
    }
)
