##' Fonction \code{rebalancement_action}
##'
##' Cette fonction permet de rebalancer le portfeuille d'action vers un portfeuile cible.
##' Le montant total, en valeur de marche, du portefeuille cible est renseigne dans le parametre \code{alloc_cible}.
##'
##' @name rebalancement_action
##' @docType methods
##' @param action est un objet de type \code{\link{Action}}. Ce parametre represente le ptf actuel de la compagnie.
##' @param action_cible est un objet de type \code{\link{Action}}. Ce parametre represente le ptf cible.
##' @param alloc_cible est un \code{numeric}. Ce parametre indique l'allocation cible a atteindre.
##' @author Damien Tichit pour Sia Partners
##' @include Action-class.R
##'
setGeneric(name = "rebalancement_action", def = function(action, action_cible, alloc_cible) {standardGeneric("rebalancement_action")})
setMethod(
    f = "rebalancement_action",
    signature = c(action = "Action", action_cible = "Action", alloc_cible = "numeric"),
    definition = function(action, action_cible, alloc_cible) {

        ## ###########################
        ##   Extraction des donnees
        ## ###########################

        # Extraction des PTF en les triant pour ne pas faire d'erreurs par la suite
        ptf_cible <- action_cible@ptf[order(action_cible@ptf[,"id_mp"]), ]
        ptf       <- action@ptf[order(action@ptf[,"id_mp"]), ]

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
        diff_action <- sum(vm_ptf) - alloc_cible



        ## ###########################
        ##          ACHAT
        ## ###########################

        if(diff_action < 0) {

            ## ###
            ## Dans ce cas la, il manque des actions => ACHAT
            ## ###

            # Determiner les actions provenant du PTF cible
            ac_cible <- which(cible_action)

            # Si toutes les actions cibles ne sont pas presentes
            if(length(ac_cible) != nrow(ptf_cible)) {

                # Merge des 2 dataframe en supprimant les actions cibles du 1er ptf : cela permet de construire un ptf en partant de 0
                ptfs_merge <- merge(ptf[which(! ptf$cible),], ptf_cible, by = "id_mp", all.x = TRUE, all.y = TRUE)

                # Montant a acheter par action : penser a comptabiliser les actions cibles deja presentes dans le 1er ptf
                alloc_unit_action <- (abs(diff_action) + sum(ptf[which(ptf$cible==TRUE), "valeur_marche"])) * ptfs_merge$prop

                # Constitution du nouveau portfeuille
                ptf <- data.frame(id_mp = ptfs_merge$id_mp, nombre = psum(ptfs_merge$nombre, alloc_unit_action / ptfs_merge$valeur_marche.y, na.rm = T),
                                  valeur_marche = psum(ptfs_merge$valeur_marche.x, alloc_unit_action, na.rm = T),
                                  dividende = ptfs_merge$dividende.x, cible = ptfs_merge$cible)

                # Mise a jour des NAs pour les dividendes
                is.na_dividende <- which(is.na(ptf$dividende))
                ptf[is.na_dividende, "dividende"] <- ptfs_merge[is.na_dividende, "dividende.y"]

                # Mise a jour des NAs pour la colonne 'cible'
                ptf[which(is.na(ptf$cible)), "cible"] <- TRUE



            } else {

                # Trier par id pour ne pas faire d'erreur
                ptf <- ptf[order(ptf[,"id_mp"]), ]
                ptf_cible <- ptf_cible[order(ptf_cible[,"id_mp"]), ]


                # Extraction de donnees
                vm_cible_ptf <- .subset2(ptf[ac_cible,], num_vm_ptf)

                # Re-equilibrage au sein du PTF existant
                re_eq_unit_action <- sum(vm_cible_ptf) * prop_ptf_cible - vm_cible_ptf

                # Achat d'actions
                achat_unit_action <- abs(diff_action) * prop_ptf_cible

                # Allocation totale par action
                alloc_unit_action <- re_eq_unit_action + achat_unit_action


                # Mise a jour des donnees
                ptf[ac_cible, "valeur_marche"] <- vm_cible_ptf + alloc_unit_action
                ptf[ac_cible, "nombre"] <- ptf[ac_cible, "valeur_marche"] / vm_ptf_cible
            }




            ## ###########################
            ##          VENTE
            ## ###########################

        } else {

            ## ###
            ## Dans ce cas la, il manque des actions => VENTE
            ## ###


            # Differencier le type d'action
            ac_ncible <- which(! cible_action)
            ac_cible  <- which(cible_action)

            ## ###
            ## 1ere etape : Priorite a la vente des actions "non-cibles"
            ## ###
            if(length(ac_ncible) > 0L){

                # VM totale des actions "non-cibles"
                cumsum_vm_action_ncible <- cumsum(ptf[ac_ncible,"valeur_marche"])

                # Actions pour lesquelles la somme cumulee est superieure au besoin
                ac_sup_besoin <- which((cumsum_vm_action_ncible > diff_action))


                if(length(ac_sup_besoin) == 0L) {

                    ## ###
                    ## Si la somme est inferieure au besoin => On vend tout
                    ## ###

                    # Somme des actions vendues
                    sum_ac_sold <- sum(ptf[ac_ncible, "valeur_marche"])

                    # Reste des actions a vendre
                    reste_a_vendre <- diff_action - sum_ac_sold

                    # Vente des actions
                    ptf <- ptf[- ac_ncible,]


                } else {

                    ## ###
                    ## Si la somme est superieure au besoin => On vend action apres action
                    ## ###

                    # Initialisation des parametres
                    reste_a_vendre <- diff_action
                    non_cible <- TRUE

                    # Vente des actions une apres l'autre
                    while ((reste_a_vendre > 0) & non_cible) {

                        # VM de la premiere action
                        vm <- ptf[1L, "valeur_marche"]

                        if(vm < reste_a_vendre) {

                            # Vente de l'action
                            ptf <- ptf[-1L,]

                            # Mise a jour du parametre
                            reste_a_vendre <- reste_a_vendre - vm

                            # Teste si l'action suivante est 'non-cible'
                            non_cible <- (! ptf[1L, "cible"])

                        } else {

                            # VM unitaire
                            vm_unitaire <- vm / ptf[1L, "nombre"]

                            # Vente d'une partie de l'action
                            ptf[1L, "valeur_marche"] <- vm - reste_a_vendre
                            ptf[1L, "nombre"] <- (vm - reste_a_vendre)/vm_unitaire

                            # Mise a jour du parametre
                            reste_a_vendre <- 0

                        }
                    }
                }
            }


            # Si pas de vente d'action "non-cible", le reste a vendre est encore total
            if(is.na(reste_a_vendre))
                reste_a_vendre <- diff_action


            ## ###
            ## 2eme etape : Vente des actions provenant du PTF cible en suivant les proportions renseignees
            ## ###
            if(reste_a_vendre > 0) {

                # Extraction de donnees
                ac_cible <- which(.subset2(ptf, which(names_ptf == "cible")))
                vm_cible_ptf <- .subset2(ptf[ac_cible,], num_vm_ptf)

                # Re-equilibrage au sein du PTF existant
                re_eq_unit_action <- sum(vm_cible_ptf) * prop_ptf_cible - vm_cible_ptf

                # Vente d'actions
                vente_unit_action <- abs(reste_a_vendre) * prop_ptf_cible

                # Allocation unitaire par action
                alloc_unit_action <- re_eq_unit_action + vente_unit_action


                # Mise a jour des donnees
                ptf[ac_cible, "valeur_marche"] <- vm_cible_ptf - alloc_unit_action
                ptf[ac_cible, "nombre"] <- ptf[ac_cible, "valeur_marche"] / vm_ptf_cible

            }

        }


        # Mise a jour de l'objet
        action@ptf <- ptf


        # Calcul des parametres de sorties
        if (diff_action < 0) {
            achat <- diff_action ; vente <- 0
        } else {
            vente <- diff_action ; achat <- 0
        }


        # Output
        return(list(action = action,
                    flux = list(achat = achat,
                                vente = vente)))
    }
)
