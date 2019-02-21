##' Fonction \code{revalo_epargne}
##'
##' Cette fonction permet de revaloriser un portefeuille Epargne. La distribution se fait de la faca suivante :
##' \describe{
##' \item{Revalorisation cible}{ : la revalorisation s'effectue de telle façon a ce que tous les MP aient la meme revalorisation et en se rapprochant du taux cible.}
##' \item{Revalorisation supplementaire}{ : la revalorisation s'effectue au proportionnellement aux PM.}
##' }
##' Les besoins reglementaires sont tout d'abord decompte du montant devant etre obligatoirement distribue.
##'
##' @name revalo_epargne
##' @docType methods
##' @param epargne est un objet de type \code{\link{Epargne}}.
##' @param revalo_cible est un \code{numeric} representant le montant de revalorisation cible a distibuer.
##' @param revalo_supp est un \code{numeric} representant le montant de revalorisation supplementaire a distibuer.
##' @param cible est un \code{numeric} representant un taux cible.
##' @param agreg_out est une valeur \code{logical} qui indique si les sorties doivent etre agregees. Par defaut, sa valeur est a TRUE.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Epargne-class.R
##'
setGeneric(name = "revalo_epargne", def = function(epargne, revalo_cible, revalo_supp, cible, agreg_out = TRUE) {standardGeneric("revalo_epargne")})
setMethod(
    f = "revalo_epargne",
    signature = c(epargne = "Epargne", revalo_cible = "numeric", revalo_supp = "numeric", cible = "numeric"),
    definition = function(epargne, revalo_cible, revalo_supp, cible, agreg_out){


        ## ###########################
        ##   Extraction des donnnes
        ## ###########################
        name_ptf <- names(epargne@ptf)



        ## ######################################################
        ## ######################################################
        ##
        ##               Calculs des chargements
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnees
        pm_ptf_epargne   <- .subset2(epargne@ptf, which(name_ptf == "pm"))
        tx_chgt_administration <- .subset2(epargne@ptf, which(name_ptf == "chgt_administration"))

        # Calcul des chargements
        chgt_administration <- pm_ptf_epargne * tx_chgt_administration






        ## ######################################################
        ## ######################################################
        ##
        ##         Calcul des chargements appliques
        ##
        ## ######################################################
        ## ######################################################

        # Calcul des chargements en fonctionde la PB pouvant etre distribuee
        if(sum(chgt_administration) > revalo_cible) {
            revalo_cible <- 0
            chgt_appliques <- revalo_cible * chgt_administration / sum(chgt_administration)
        } else {
            revalo_cible <- revalo_cible - sum(chgt_administration)
            chgt_appliques <- chgt_administration
        }








        ## ######################################################
        ## ######################################################
        ##
        ##         Calcul des differentes revalorisations
        ##
        ## ######################################################
        ## ######################################################

        # Extraction des donnees
        pm_ptf_epargne   <- .subset2(epargne@ptf, which(name_ptf == "pm"))


        ## ###########################
        ## Revalorisation contractuelle
        ## ###########################

        # Extraction de donnees
        tmg_ptf_epargne  <- .subset2(epargne@ptf, which(name_ptf == "tmg"))

        # Revalorisation au TMG par model point
        revalo_tmg_mp <- pm_ptf_epargne * tmg_ptf_epargne




        ## ###########################
        ##    Revalorisation cible
        ## ###########################

        # Proportion a distribuer
        besoin_cible <- pmax(cible - tmg_ptf_epargne, 0) * pm_ptf_epargne

        # MP pour lesquels le cible est superieur au TMG
        id_sup <- which(besoin_cible > 0)


        if(length(id_sup) > 0L) {
            # Taux de revalorisation
            tx_revalo_cible <- (sum(revalo_tmg_mp[id_sup] + pm_ptf_epargne[id_sup]) + revalo_cible) / sum(pm_ptf_epargne[id_sup]) - 1

            # Revalorisation cible par model point
            revalo_cible_mp <- pmax(tx_revalo_cible - tmg_ptf_epargne, 0) * pm_ptf_epargne
        } else {

            # Au prorata de la pM
            revalo_cible_mp <- revalo_cible * pm_ptf_epargne / sum(pm_ptf_epargne)
        }



        ## ###########################
        ## Revalorisation supplementaire
        ## ###########################

        # Revalorisation supplementaire par model point
        revalo_supp_mp <- revalo_supp * pm_ptf_epargne / sum(pm_ptf_epargne)




        ## ###########################
        ##        PB distribuee
        ## ###########################

        # PB distribuee
        pb_distribuee <- revalo_cible_mp + revalo_supp_mp





        ## ######################################################
        ## ######################################################
        ##
        ##                  Mise a jour du PTF
        ##
        ## ######################################################
        ## ######################################################

        # Revalorisation totale
        revalo_tot <- revalo_tmg_mp + revalo_cible_mp + revalo_supp_mp

        # Calcul des nouvelles PM
        new_pm <- pm_ptf_epargne + revalo_tot

        # Calcul du taux de revalorisation
        tx_revalo <- revalo_tot / pm_ptf_epargne

        # Mise a 0 des PM nulles
        null_pm <- which(epargne@ptf$pm==0)
        if(length(null_pm)>0L)
            tx_revalo[null_pm] <- 0

        # Mise a jour des PM
        epargne@ptf$pm <- new_pm
        epargne@ptf$revalo_prec <- tx_revalo

        # Mise a 0 des "petites" PM
        small_pm <- which(epargne@ptf$pm<1e-8)
        if(length(small_pm)>0L) {
            epargne@ptf[small_pm, "pm"] <- 0
            epargne@ptf[small_pm, "nb_contr"] <- 0
        }





        # Output
        return(list(epargne = epargne,
                    revalorisation = list(tmg = sum_cond(x = revalo_tmg_mp, cond = agreg_out),
                                          cible = sum_cond(x = revalo_cible_mp, cond = agreg_out),
                                          supplementaire = sum_cond(x = revalo_supp_mp, cond = agreg_out)),
                    chargement = sum_cond(x = chgt_appliques, cond = agreg_out)))
    }
)
