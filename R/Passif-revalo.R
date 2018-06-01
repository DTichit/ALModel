##' Fonction \code{revalo_passif}
##'
##' Cette fonction permet de revaloriser le passif d'une compagnie d'assurance. Au cours de cette fonction sont effectuees : la regle des 8 ans.
##'
##' Les politiques de revalorisation appliquees dans cette fonction sont les suivantes :
##' \describe{
##' \item{Epargne}{ : Atteindre une revalorisation proched du tme afin de minimiser les rachats conjoncturels.}
##' }
##' Les besoins reglementaires sont tout d'abord decompté du montant devant etre obligatoirement distribue.
##'
##'
##' @name revalo_passif
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @param resultat est un \code{numeric}.
##' @param pvl est une \code{list} contenant les PVL par produits.
##' @param revalo_prestation est une \code{list} indiquant des montants de revalorisation obligatoires ayant deja ete distribue. Ils sont consideres comme des besoins de revalorisation contractuels.
##' Les montants doivent etre mis sous forme de liste et par produit.
##' @param an est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @seealso Calcul du besoin en revalorisation : \code{\link{besoin_revalo_ptf_passif}}.
##' @seealso Gestion de la regle des 8 ans sur la PPE : \code{\link{reprise_ppe_8ans}}.
##' @export
##' @include Passif-class.R PPE-reprise_ppe_8ans.R PTFPassif-besoin_revalo.R
##'
setGeneric(name = "revalo_passif", def = function(passif, resultat, pvl, revalo_prestation, an) {standardGeneric("revalo_passif")})
setMethod(
    f = "revalo_passif",
    signature = c(passif = "Passif", resultat = "numeric", pvl = "list", revalo_prestation = "list", an = "integer"),
    definition = function(passif, resultat, pvl, revalo_prestation, an){



        ## ######################################################
        ## ######################################################
        ##
        ##          Montants pouvant etre distribues
        ##
        ## ######################################################
        ## ######################################################


        ## ###########################
        ##          PPE totale
        ## ###########################

        # Extraction du montant de la PPE
        ppe_totale <- sum(passif@provision@ppe@ppe)



        ## ###########################
        ##    Montant total des PVL
        ## ###########################

        # Supprimer les PVL obligataires
        pvl[["obligation"]] <- 0

        # Somme des PVL
        pvl_totale <- sum_list(pvl, 1L)





        ## ######################################################
        ## ######################################################
        ##
        ##       Determination du besoin en revalorisation
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##    Appel de la fonction
        ## ###########################

        # Appel de la fonction
        besoin_revalo <- besoin_revalo_ptf_passif(ptf_passif = passif@ptf_passif, cible = list(epargne = passif@hyp_passif@cible$epargne[an]))


        ## ###########################
        ##      Besoins totaux
        ## ###########################

        # Somme des besoins contractuels
        besoin_contractuel <- sum_list(besoin_revalo[["besoin"]][["contractuel"]], 1L) + sum_list(revalo_prestation, 1L)

        # Somme des besoins cibles
        besoin_cible <- sum_list(besoin_revalo[["besoin"]][["cible"]], 1L)


        ## ###########################
        ##        Chargements
        ## ###########################

        # Somme des chargements
        chargements <- sum_list(besoin_revalo[["chargements"]], 1L)






        ## ######################################################
        ## ######################################################
        ##
        ##          Initialisation de parametres
        ##
        ## ######################################################
        ## ######################################################

        # Creation des variables *_restant
        besoin_contractuel_restant <- besoin_contractuel ; besoin_cible_restant <- besoin_cible
        pvl_restante <- pvl_totale
        resultat_restant <- resultat ; ppe_restante <- ppe_totale






        ## ######################################################
        ## ######################################################
        ##
        ##          Calcul des revalorisations appliquees
        ##
        ## ######################################################
        ## ######################################################


        ## ###########################
        ## 1ere Etape : Revalorisation contractuelle
        ## Besoins contractuels => obligatoires
        ## ###########################

        # Versement du TMG : prise sur le resultat
        if(resultat_restant >= besoin_contractuel_restant) {

            # Prise sur le resultat
            resultat_restant <- resultat_restant - besoin_contractuel_restant

            # Besoin contractuel restant
            besoin_contractuel_restant <- 0


        } else {

            # Besoin contractuel restant
            besoin_contractuel_restant <- besoin_contractuel_restant - resultat_restant

            # Mise du resultat a 0
            resultat_restant <- 0


            # Prise sur les PVL
            if(pvl_restante >= besoin_contractuel_restant) {

                # PVL restantes
                pvl_restante <- pvl_restante - besoin_contractuel_restant

                # Besoin contractuel restant
                besoin_contractuel_restant <- 0

            } else {

                # Besoin d'emprunter
                emprunt <- besoin_contractuel_restant - pvl_restante

                # PVL restantes
                pvl_restante <- 0

                # Besoin contractuel restant
                besoin_contractuel_restant <- 0

            }

        }

        # Montant qui sera attribue a la revalorisation contractuelle
        revalo_contractuelle <- besoin_contractuel

        # Montant de PVL a realiser
        pvl_a_realiser <- pvl_totale - pvl_restante






        ## ###########################
        ## 2eme Etape : revalorisation au taux cible
        ## Besoins cibles => Facultatifs
        ## ###########################

        # PB possible
        pb_possible <- resultat_restant + ppe_restante

        # Uniquement s'il y a de quoi verser
        if(pb_possible > 0) {

            # Calcul du besoin cible restant
            besoin_cible_restant <- max(besoin_cible - pb_possible, 0)

            # Calcul de la PB restante
            pb_possible_restante <- max(pb_possible - besoin_cible, 0)


            # Montant de la PB qui sera versee
            pb_versee <- pb_possible - pb_possible_restante



            ### ###
            ### Mise a jour des differents parametres
            ### 1 - Reprise sur la PPE
            ### 2 - Reprise sur le resultat
            ### ###

            # Modification du resultat restant
            resultat_restant <- max(resultat_restant - max(pb_versee - ppe_restante, 0), 0)

            # Modification de la PPE restante
            ppe_restante <- max(ppe_restante - pb_versee, 0)

        }

        # Montant que sera attribue a la revalorisation cible
        revalo_cible <- besoin_cible - besoin_cible_restant







        ## ###########################
        ## 3eme Etape : Gestion de la PPE
        ## Reprise du flux, dotation du resultat restant et reprise de la PPE 8 ans
        ## ###########################

        # Flux sur la PPE
        flux <- resultat_restant - (ppe_totale - ppe_restante)

        # Dotation ou reprise de la PPE en fonction du signe
        if(flux < 0)
            res_ppe <- reprise_ppe(passif@provision@ppe, -flux)
        else
            res_ppe <- dotation_ppe(passif@provision@ppe, flux)

        # Extraction de la PPE 8 ans
        res_ppe_8ans <- reprise_ppe_8ans(res_ppe[["ppe"]])

        # Mise a jour de l'objet PPE
        passif@provision@ppe <- res_ppe_8ans[["ppe"]]

        # Montant de la PPB 8 ans restant
        ppe_8ans_restante <- res_ppe_8ans[["ppe_8ans"]]

        # Flux sur la PPE
        flux_ppe <- res_ppe[["flux"]] - ppe_8ans_restante







        ## ###########################
        ## 4eme Etape : Attribution de la PPE 8 ans
        ## Besoins supplementaire => Facultatifs
        ## ###########################

        # Calcul de la revalorisation supplementaire
        revalo_supp <- ppe_8ans_restante









        ## ######################################################
        ## ######################################################
        ##
        ##    Calcul des revalorisations par produits modelises
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ## Revalorisation cible par produit
        ## ###########################

        # Calcul des revalo
        revalo_cible_prod <- sapply(names(besoin_revalo[["besoin"]][["cible"]]), simplify = FALSE, USE.NAMES = TRUE, function(x){
            if(besoin_cible >0)
                res <- besoin_revalo[["besoin"]][["cible"]][[x]] * revalo_cible / besoin_cible
            else
                res <- 0
            return(res)})



        ## ###########################
        ## Revalorisation supplementaire par produit
        ## ###########################

        # Proportion a attribuer par produit
        prod <- passif@hyp_passif@prop_pb$produit
        prop_pb <- passif@hyp_passif@prop_pb$proportion

        # Calcul de la revalo supplementaire en fonction des hypotheses
        revalo_supp_prod <- sapply(prod, function(x) prop_pb[which(prod == x)] * revalo_supp,
                                   simplify = FALSE, USE.NAMES = TRUE)







        ## ######################################################
        ## ######################################################
        ##
        ##              Revalorisation des PTF
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        res_revalo <- revalo_ptf_passif(ptf_passif = passif@ptf_passif, revalo_cible = revalo_cible_prod,
                                        revalo_supp = revalo_supp_prod, cible = list(epargne = passif@hyp_passif@cible$epargne[an]))

        # Mise a jour de l'objet
        passif@ptf_passif <- res_revalo[["ptf_passif"]]







        ## ######################################################
        ## ######################################################
        ##
        ##              Calcul des PM de cloture
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        pm_cloture <- calcul_pm(passif@ptf_passif)







        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Liste stockant l'ensemble des revalorisations
        revalo <- list(tmg = besoin_revalo[["besoin"]][["contractuel"]],
                       pb = list(cible = revalo_cible_prod,
                                 supp = revalo_supp_prod))






        # Output
        return(list(passif = passif,
                    revalorisation = revalo,
                    chargements_appliques = res_revalo[["chargements_appliques"]],
                    pvl_a_realiser = pvl_a_realiser,
                    besoin_emprunt = if.is_null(get0("emprunt"), 0),
                    flux_ppe = flux_ppe,
                    pm_cloture = pm_cloture,
                    besoin_cible = besoin_cible))
    }
)
