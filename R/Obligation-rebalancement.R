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

        # Extraction des donnees du PTF cible
        names_ptf_cible <- names(ptf_cible)
        prop_ptf_cible <- .subset2(ptf_cible, which(names_ptf_cible == "prop"))
        id_mp_cible <- as.character(.subset2(ptf_cible, which(names_ptf_cible == "id_mp")))
        mat_res_cible <- .subset2(ptf_cible, which(names_ptf_cible == "mat_res"))
        cle_cible  <- paste(ptf_cible$cible, mat_res_cible, sep = ".")

        # Extraction des donnees du PTF
        names_ptf <- names(ptf)
        num_vm_ptf <- which(names_ptf == "valeur_marche")
        vm_ptf <- .subset2(ptf, num_vm_ptf)
        id_mp <- as.character(.subset2(ptf, which(names_ptf == "id_mp")))
        mat_res <- .subset2(ptf, which(names_ptf == "mat_res"))
        cle  <- paste(ptf$cible, mat_res, sep = ".")

        # Presence des ces obligations dans le PTF ?
        id_pres_ptf <- match(cle_cible, cle)
        id_pres <- match(cle, cle_cible)
        id_pres_cib <- id_pres[!is.na(id_pres)]



        # Differentiel
        diff_alloc <- sum(vm_ptf) - alloc_cible


        ## ###########################
        ##          ACHAT
        ## ###########################

        if(diff_alloc < 0) {

            ## ###
            ## Dans ce cas la, manque => ACHAT
            ## ###

            # Presence des cibles dans le PTF
            if(! all(! is.na(id_pres_ptf))) {

                # Besoin d'ajouter les nouvelles obligs dans le PTF
                merge <- merge(ptf, ptf_cible, by = c("cible", "mat_res"), all.x = TRUE, all.y = TRUE)

                # Creation du nouveau portefeuille
                ptf <- data.frame(id_mp = pna.omit(as.character(merge$id_mp.x), as.character(merge$id_mp.y)), mat_res = merge$mat_res, cible = merge$cible,
                                  valeur_comptable = pna.omit(merge$valeur_comptable, merge$valeur_achat), valeur_marche = pna.omit(merge$valeur_marche, merge$valeur_achat),
                                  coupon = pna.omit(merge$coupon.x, merge$coupon.y), nominal = pna.omit(merge$nominal.x, merge$nominal.y), prop = merge$prop)


            } else {

                # Determination de la quantite a acheter
                achat <- ptf$prop[id_pres_ptf] * abs(diff_alloc)
                nb_achat <- achat / ptf_cible$valeur_marche

                # Mise en image de donnees
                nominal_prev <- ptf$nominal[id_pres_ptf]
                nominal_achat <- nb_achat * ptf_cible$nominal[id_pres_cib]

                # Mise a jour du PTF
                ptf$valeur_comptable[id_pres_ptf] <- ptf$valeur_comptable[id_pres_ptf] + achat
                ptf$valeur_marche[id_pres_ptf] <- ptf$valeur_marche[id_pres_ptf] + achat
                ptf$coupon[id_pres_ptf] <- (ptf$coupon[id_pres_ptf] * nominal_prev + ptf_cible$coupon[id_pres_cib] * nominal_achat) / (nominal_prev + nominal_achat)
                ptf$nominal[id_pres_ptf] <- nominal_prev + nominal_achat


            }


            ## ###########################
            ##          VENTE
            ## ###########################

        } else {

            ## ###
            ## Dans ce cas la : trop => VENTE
            ## ###

            # Presence des non-cibles
            id_ncib <- which(is.na(id_pres))

            if(length(id_ncib) > 0L) {

                # Somme des VM des non-cibles
                sum_vm_ncib <- sum(ptf$valeur_marche[id_ncib])

                if(sum_vm_ncib > diff_alloc) {

                    # Somme cumulee des VM
                    cum_sum_vm <- cumsum(ptf$valeur_marche[id_ncib])

                    # ID a supprimer
                    id_del <- which(cum_sum_vm > diff_alloc)[1L]

                    if((id_del > 1L) & (! is.null(id_del))) {

                        # VM etant supprimees
                        vm_del <- ptf[1L:(id_del-1L), "valeur_marche"]

                        # Supprimer les lignes du PTF
                        ptf <- ptf[-(1L:(id_del-1L)),]

                        # Mise a jour du reste a vendre
                        diff_alloc <- diff_alloc - sum(vm_del)

                    }

                    # Vente d'une partie des VM
                    ptf[1L, "valeur_marche"] <- ptf[1L, "valeur_marche"] - diff_alloc

                    # Mise a jour du reste a vendre
                    diff_alloc <- 0


                } else {

                    # VM etant supprimees
                    vm_del <- ptf[id_ncib, "valeur_marche"]

                    # Supprimer les lignes du PTF
                    ptf <- ptf[-id_ncib,]

                    # Mise a jour du reste a vendre
                    diff_alloc <- diff_alloc - sum(vm_del)

                }


            }


            # Vente des non cibles, le cas echeant
            if(diff_alloc > 0) {

                # Calcul de la vente devant etre effectue
                vente <- ptf$prop[id_pres_ptf] * abs(diff_alloc)
                nb_vente <- vente / ptf$valeur_marche[id_pres_ptf]


                # Mise en image de donnees
                nominal_prev <- ptf$nominal[id_pres_ptf]
                prop_vc <- ptf$valeur_comptable[id_pres_ptf] / ptf$valeur_marche[id_pres_ptf]
                prop_nom <- ptf$nominal[id_pres_ptf] / ptf$valeur_marche[id_pres_ptf]

                # Mise a jour du PTF
                ptf$valeur_marche[id_pres_ptf] <- ptf$valeur_marche[id_pres_ptf] - vente
                ptf$valeur_comptable[id_pres_ptf] <- ptf$valeur_comptable[id_pres_ptf] - vente * prop_vc
                ptf$nominal[id_pres_ptf] <- ptf$nominal[id_pres_ptf] - vente * prop_vm


                # Mise a jour du reste a vendre
                diff_alloc <- diff_alloc - sum(vente)


            }

        }


        # Mise a jour de l'objet
        oblig@ptf <- ptf


        # Calcul des parametres de sorties
        if (diff_alloc < 0) {
            achat <- diff_alloc ; vente <- 0
        } else {
            vente <- diff_alloc ; achat <- 0
        }


        # Output
        return(list(oblig = oblig,
                    flux = list(achat = achat,
                                vente = vente)))
    }
)
