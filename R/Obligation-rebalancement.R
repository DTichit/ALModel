##' Fonction \code{rebalancement_obligation}
##'
##' Cette fonction permet de rebalancer le portfeuille d'obligation vers un portfeuile cible.
##' Le montant total, en valeur de marche, du portefeuille cible est renseigne dans le parametre \code{alloc_cible}.
##'
##' @name rebalancement_obligation
##' @docType methods
##' @param oblig est un objet de type \code{\link{Obligation}}. Ce parametre represente le ptf actuel de la compagnie.
##' @param oblig_cible est un objet de type \code{\link{Obligation}}. Ce parametre represente le ptf cible.
##' @param alloc_cible est un \code{numeric}. Ce parametre indique l'allocation cible a atteindre.
##' @author Damien Tichit pour Sia Partners
##' @include Obligation-class.R
##'
setGeneric(name = "rebalancement_obligation", def = function(oblig, oblig_cible, alloc_cible) {standardGeneric("rebalancement_obligation")})
setMethod(
    f = "rebalancement_obligation",
    signature = c(oblig = "Obligation", oblig_cible = "Obligation", alloc_cible = "numeric"),
    definition = function(oblig, oblig_cible, alloc_cible) {


        warning("La fonction 'rebalancement_obligation' n'est pas codee !")

        ## ###########################
        ##   Extraction des donnees
        ## ###########################

        # Extraction des PTF en les triant pour ne pas faire d'erreurs par la suite
        ptf_cible <- oblig_cible@ptf[order(oblig_cible@ptf[,"id_mp"]), ]
        ptf       <- oblig@ptf[order(oblig@ptf[,"id_mp"]), ]

        # Extraction des donnees du PTF
        names_ptf <- names(ptf)
        num_vm_ptf <- which(names_ptf == "valeur_marche")
        vm_ptf <- .subset2(ptf, num_vm_ptf)
        cible_action <- .subset2(ptf, which(names_ptf == "cible"))

        # Extraction des donnees du PTF cible
        names_ptf_cible <- names(ptf_cible)
        vm_ptf_cible <- .subset2(ptf_cible, which(names_ptf_cible == "valeur_marche"))
        prop_ptf_cible <- .subset2(ptf_cible, which(names_ptf_cible == "prop"))

        # Differentiel
        diff_oblig <- sum(vm_ptf) - alloc_cible



        ## ###########################
        ##          ACHAT
        ## ###########################

        if(diff_oblig < 0) {

            ## ###
            ## Dans ce cas la, il manque des actions => ACHAT
            ## ###



            ## ###########################
            ##          VENTE
            ## ###########################

        } else {

            ## ###
            ## Dans ce cas la, il manque des actions => VENTE
            ## ###



        }


        # Mise a jour de l'objet
        oblig@ptf <- ptf


        # Calcul des parametres de sorties
        if (diff_oblig < 0) {
            achat <- diff_oblig ; vente <- 0
        } else {
            vente <- diff_oblig ; achat <- 0
        }


        # Output
        return(list(oblig = oblig,
                    flux = list(achat = achat,
                                vente = vente)))
    }
)
