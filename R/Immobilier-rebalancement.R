##' Fonction \code{rebalancement_immobilier}
##'
##' Cette fonction permet de rebalancer le portfeuille immobilier vers un portfeuile cible.
##' Le montant total, en valeur de marche, du portefeuille cible est renseigne dans le parametre \code{alloc_cible}.
##'
##' @name rebalancement_immobilier
##' @docType methods
##' @param immo est un objet de type \code{\link{Immobilier}}. Ce parametre represente le ptf actuel de la compagnie.
##' @param immo_cible est un objet de type \code{\link{Immobilier}}. Ce parametre represente le ptf cible.
##' @param alloc_cible est un \code{numeric}. Ce parametre indique l'allocation cible a atteindre.
##' @author Damien Tichit pour Sia Partners
##' @include Immobilier-class.R
##'
setGeneric(name = "rebalancement_immobilier", def = function(immo, immo_cible, alloc_cible) {standardGeneric("rebalancement_immobilier")})
setMethod(
    f = "rebalancement_immobilier",
    signature = c(immo = "Immobilier", immo_cible = "Immobilier", alloc_cible = "numeric"),
    definition = function(immo, immo_cible, alloc_cible) {

        ## ###########################
        ##   Extraction des donnees
        ## ###########################

        # Extraction des PTF en les triant pour ne pas faire d'erreurs par la suite
        ptf_cible <- immo_cible@ptf[order(immo_cible@ptf[,"id_mp"]), ]
        ptf       <- immo@ptf[order(immo@ptf[,"id_mp"]), ]

        # Extraction des donnees du PTF
        names_ptf <- names(ptf)
        num_vm_ptf <- which(names_ptf == "valeur_marche")
        vm_ptf <- .subset2(ptf, num_vm_ptf)
        cible_immo <- .subset2(ptf, which(names_ptf == "cible"))

        # Extraction des donnees du PTF cible
        names_ptf_cible <- names(ptf_cible)
        vm_ptf_cible <- .subset2(ptf_cible, which(names_ptf_cible == "valeur_marche"))
        prop_ptf_cible <- .subset2(ptf_cible, which(names_ptf_cible == "prop"))

        # Differentiel
        diff_immo <- sum(vm_ptf) - alloc_cible



        ## ###########################
        ##          ACHAT
        ## ###########################

        if(diff_immo < 0) {

            ## ###
            ## Dans ce cas la, il manque des immos => ACHAT
            ## ###

            # Determiner les immos provenant du PTF cible
            im_cible <- which(cible_immo)

            # Si toutes les immos cibles ne sont pas presentes
            if(length(im_cible) != nrow(ptf_cible)) {

                # Merge des 2 dataframe en supprimant les immos cibles du 1er ptf : cela permet de construire un ptf en partant de 0
                ptfs_merge <- merge(ptf[which(! ptf$cible),], ptf_cible, by = "id_mp", all.x = TRUE, all.y = TRUE)

                # Montant a acheter par immo : penser a comptabiliser les immos cibles deja presentes dans le 1er ptf
                alloc_unit_immo <- (abs(diff_immo) + sum(ptf[which(ptf$cible==TRUE), "valeur_marche"])) * ptfs_merge$prop

                # Constitution du nouveau portfeuille
                ptf <- data.frame(id_mp = ptfs_merge$id_mp, nombre = psum(ptfs_merge$nombre, alloc_unit_immo / ptfs_merge$valeur_marche.y, na.rm = T),
                                  valeur_marche = psum(ptfs_merge$valeur_marche.x, alloc_unit_immo, na.rm = T),
                                  loyer = ptfs_merge$loyer.x, cible = ptfs_merge$cible)

                # Mise a jour des NAs pour les loyers
                is.na_loyer <- which(is.na(ptf$loyer))
                ptf[is.na_loyer, "loyer"] <- ptfs_merge[is.na_loyer, "loyer.y"]

                # Mise a jour des NAs pour la colonne 'cible'
                ptf[which(is.na(ptf$cible)), "cible"] <- TRUE



            } else {

                # Trier par id pour ne pas faire d'erreur
                ptf <- ptf[order(ptf[,"id_mp"]), ]
                ptf_cible <- ptf_cible[order(ptf_cible[,"id_mp"]), ]


                # Extraction de donnees
                vm_cible_ptf <- .subset2(ptf[im_cible,], num_vm_ptf)

                # Re-equilibrage au sein du PTF existant
                re_eq_unit_immo <- sum(vm_cible_ptf) * prop_ptf_cible - vm_cible_ptf

                # Achat d'immos
                achat_unit_immo <- abs(diff_immo) * prop_ptf_cible

                # Allocation totale par immo
                alloc_unit_immo <- re_eq_unit_immo + achat_unit_immo


                # Mise a jour des donnees
                ptf[im_cible, "valeur_marche"] <- vm_cible_ptf + alloc_unit_immo
                ptf[im_cible, "nombre"] <- ptf[im_cible, "valeur_marche"] / vm_ptf_cible
            }




            ## ###########################
            ##          VENTE
            ## ###########################

        } else {

            ## ###
            ## Dans ce cas la, il manque des immos => VENTE
            ## ###


            # Differencier le type d'immo
            im_ncible <- which(! cible_immo)
            im_cible  <- which(cible_immo)

            ## ###
            ## 1ere etape : Priorite a la vente des immos "non-cibles"
            ## ###
            if(length(im_ncible) > 0L){

                # VM totale des immos "non-cibles"
                cumsum_vm_immo_ncible <- cumsum(ptf[im_ncible,"valeur_marche"])

                # immos pour lesquelles la somme cumulee est superieure au besoin
                ac_sup_besoin <- which((cumsum_vm_immo_ncible > diff_immo))


                if(length(ac_sup_besoin) == 0L) {

                    ## ###
                    ## Si la somme est inferieure au besoin => On vend tout
                    ## ###

                    # Somme des immos vendues
                    sum_ac_sold <- sum(ptf[im_ncible, "valeur_marche"])

                    # Reste des immos a vendre
                    reste_a_vendre <- diff_immo - sum_ac_sold

                    # Vente des immos
                    ptf <- ptf[- im_ncible,]


                } else {

                    ## ###
                    ## Si la somme est superieure au besoin => On vend immo apres immo
                    ## ###

                    # Initialisation des parametres
                    reste_a_vendre <- diff_immo
                    non_cible <- TRUE

                    # Vente des immos une apres l'autre
                    while ((reste_a_vendre > 0) & non_cible) {

                        # VM de la premiere immo
                        vm <- ptf[1L, "valeur_marche"]

                        if(vm < reste_a_vendre) {

                            # Vente de l'immo
                            ptf <- ptf[-1L,]

                            # Mise a jour du parametre
                            reste_a_vendre <- reste_a_vendre - vm

                            # Teste si l'immo suivante est 'non-cible'
                            non_cible <- (! ptf[1L, "cible"])

                        } else {

                            # VM unitaire
                            vm_unitaire <- vm / ptf[1L, "nombre"]

                            # Vente d'une partie de l'immo
                            ptf[1L, "valeur_marche"] <- vm - reste_a_vendre
                            ptf[1L, "nombre"] <- (vm - reste_a_vendre)/vm_unitaire

                            # Mise a jour du parametre
                            reste_a_vendre <- 0

                        }
                    }
                }
            }


            # Si pas de vente d'immo "non-cible", le reste a vendre est encore total
            if(is.na(reste_a_vendre))
                reste_a_vendre <- diff_immo


            ## ###
            ## 2eme etape : Vente des immos provenant du PTF cible en suivant les proportions renseignees
            ## ###
            if(reste_a_vendre > 0) {

                # Extraction de donnees
                im_cible <- which(.subset2(ptf, which(names_ptf == "cible")))
                vm_cible_ptf <- .subset2(ptf[im_cible,], num_vm_ptf)

                # Re-equilibrage au sein du PTF existant
                re_eq_unit_immo <- sum(vm_cible_ptf) * prop_ptf_cible - vm_cible_ptf

                # Vente d'immos
                vente_unit_immo <- abs(reste_a_vendre) * prop_ptf_cible

                # Allocation unitaire par immo
                alloc_unit_immo <- re_eq_unit_immo + vente_unit_immo


                # Mise a jour des donnees
                ptf[im_cible, "valeur_marche"] <- vm_cible_ptf - alloc_unit_immo
                ptf[im_cible, "nombre"] <- ptf[im_cible, "valeur_marche"] / vm_ptf_cible

            }

        }


        # Mise a jour de l'objet
        immo@ptf <- ptf


        # Calcul des parametres de sorties
        if (diff_immo < 0) {
            achat <- diff_immo ; vente <- 0
        } else {
            vente <- diff_immo ; achat <- 0
        }


        # Output
        return(list(immo = immo,
                    flux = list(achat = achat,
                                vente = vente)))
    }
)
