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


        ## ###########################
        ##   Extraction des donnees
        ## ###########################

        # Extraction des PTF
        ptf       <- oblig@ptf

        # Tri pour eviter de faire des erreurs
        # Ce tri permet de vendre dans un premier temps les obligations cibles puis celles ayant une maturiete residuelle petite
        ptf <- ptf[order(ptf$cible, (ptf$maturite - ptf$duree_detention)),]

        # Extraction des donnees du PTF
        names_ptf <- names(ptf)
        vm_ptf <- .subset2(ptf, which(names_ptf == "valeur_marche"))

        # Initialisation des PMVR
        pmvr <- 0

        # Differentiel
        diff_alloc <- sum(vm_ptf) - alloc_cible

        # Flux : parametre de sortie
        flux <- alloc_cible - sum(vm_ptf)




        if(diff_alloc < 0) {

            ## ######################################################
            ## ######################################################
            ##
            ##                          ACHAT
            ##
            ## ######################################################
            ## ######################################################

            # Extraction des donnees du PTF cible
            ptf_cible <- oblig_cible@ptf
            names_ptf_cible <- names(ptf_cible)


            # Numero de colonne
            num_prop <- which(names_ptf_cible == "prop")

            # Calcul de l'achat devant etre effectue
            achat <- abs(diff_alloc) * .subset2(ptf_cible, num_prop)
            nb_achat <- achat / .subset2(ptf_cible, which(names_ptf_cible == "valeur_marche"))

            # Suppression de la colonne 'prop'
            ptf_cible <- ptf_cible[-num_prop]

            # Mise a jour des colonnes existantes
            ptf_cible$valeur_marche <- achat
            ptf_cible$valeur_remboursement <- nb_achat * .subset2(ptf_cible, which(names_ptf_cible == "valeur_remboursement"))
            ptf_cible$nominal <- nb_achat * .subset2(ptf_cible, which(names_ptf_cible == "nominal"))


            # Ajout de nouvelles colonnes
            ptf_cible$tri <- ptf_cible$coupon
            ptf_cible$valeur_achat <- achat
            ptf_cible$valeur_nette_comptable <- achat
            ptf_cible$duree_detention <- 1L
            ptf_cible$spread <- 0


            # Ajout du nouveau PTF
            ptf <-  dplyr::bind_rows(ptf, ptf_cible)


        } else {

            ## ######################################################
            ## ######################################################
            ##
            ##                          VENTE
            ##
            ## ######################################################
            ## ######################################################




            # Extraction de donnees
            vnc_ptf <- .subset2(ptf, which(names_ptf == "valeur_nette_comptable"))

            # Somme cumulee des VM
            cum_sum_vm <- cumsum(vm_ptf)

            # ID a supprimer
            id_del <- which(cum_sum_vm <= diff_alloc)


            if(length(id_del) >= 1L) {

                # Obligations etant supprimees
                vm_del <- vm_ptf[id_del]
                vnc_del <- vnc_ptf[id_del]

                # Supprimer les lignes du PTF
                ptf <- ptf[-id_del,]

                # Mise a jour du reste a vendre
                diff_alloc <- diff_alloc - sum(vm_del)

                # Calcul des PMVR
                pmvr <- sum(abs(vm_del) - abs(vnc_del))

            }


            # S'il reste des obligations a vendre
            if(nrow(ptf) > 0L){

                # Mise en image de donnees
                vm <- ptf[1L, "valeur_marche"]
                vnc <- ptf[1L, "valeur_nette_comptable"]

                # Part de l'oblig supprimee
                vnc_del <- diff_alloc * (vnc / vm)
                vm_del <- diff_alloc

                # Vente d'une partie de la 1ere obligation
                ptf[1L, "valeur_achat"] <- ptf[1L, "valeur_achat"] - diff_alloc * (ptf[1L, "valeur_achat"] / vm)
                ptf[1L, "valeur_nette_comptable"] <- vnc - vnc_del
                ptf[1L, "valeur_marche"] <- vm - diff_alloc
                ptf[1L, "nominal"] <- ptf[1L, "nominal"] - diff_alloc * (ptf[1L, "nominal"] / vm)
                ptf[1L, "valeur_remboursement"] <- ptf[1L, "valeur_remboursement"] - diff_alloc * (ptf[1L, "valeur_remboursement"] / vm)


                # Mise a jour du reste a vendre
                diff_alloc <- 0


                # Calcul des PMVR
                pmvr <- pmvr + sum(abs(vm_del) - abs(vnc_del))
            }

        }


        # Mise a jour de l'objet
        oblig@ptf <- ptf



        # Output
        return(list(oblig = oblig,
                    pmvr = pmvr,
                    flux = flux))
    }
)
