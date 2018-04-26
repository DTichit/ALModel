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
##' @param pb est un \code{numeric} representant le montant de PB a distribuer.
##' @param revalo_prestation est une \code{list} indiquant des montants de revalorisation obligatoires ayant deja ete distribue. Ils sont consideres comme des besoins de revalorisation contractuels.
##' Les montants doivent etre mis sous forme de liste et par produit.
##' @param an est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @seealso Calcul du besoin en revalorisation : \code{\link{besoin_revalo_ptf_passif}}.
##' @seealso Gestion de la regle des 8 ans sur la PPE : \code{\link{reprise_ppe_8ans}}.
##' @export
##' @include Passif-class.R PPE-reprise_ppe_8ans.R PTFPassif-besoin_revalo.R
##'
setGeneric(name = "revalo_passif", def = function(passif, pb, revalo_prestation, an) {standardGeneric("revalo_passif")})
setMethod(
    f = "revalo_passif",
    signature = c(passif = "Passif", pb = "numeric", revalo_prestation = "list", an = "integer"),
    definition = function(passif, pb, revalo_prestation, an){



        ## ######################################################
        ## ######################################################
        ##
        ##          Montant obligatoirement redistibue
        ##
        ## ######################################################
        ## ######################################################

        ## ###########################
        ##  Regle des 8 ans de la PPE
        ## ###########################

        # Appel de la fonction
        ppe_8ans <- reprise_ppe_8ans(passif@provision@ppe)

        # Mise a jour de l'objet PPE
        passif@provision@ppe <- ppe_8ans[["ppe"]]

        # Calcul du montant devant etre distribue obligatoirement
        revalo_oblig <- ppe_8ans[["ppe_8ans"]]

        # Flux sur la PPE
        flux_ppe <- -ppe_8ans[["ppe_8ans"]]





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
        besoin_contr <- do.call(sum, besoin_revalo[["besoin_contr"]])

        # Somme des besoins cibles
        besoin_cible <- do.call(sum, besoin_revalo[["besoin_cible"]])



        ## ###########################
        ##     Besoins prestations
        ## ###########################

        # Extraction des donnees
        besoin_prest <- do.call(sum, revalo_prestation)






        ## ######################################################
        ## ######################################################
        ##
        ##          Calcul des revalorisations appliquees
        ##
        ## ######################################################
        ## ######################################################


        ## ###########################
        ## 1ere Etape :
        ## Besoins contractuels => obligatoires
        ## ###########################

        # Calcul de la revalorisation devant encore etre applique
        res_revalo <- calcul_revalo(besoin = besoin_contr + besoin_prest, pb = pb, ppe = passif@provision@ppe, revalo_oblig = revalo_oblig)

        # Mise a jour des objets
        passif@provision@ppe <- res_revalo[["ppe"]][["ppe"]]
        pb <- res_revalo[["pb"]]
        revalo_oblig <- res_revalo[["revalo_oblig"]]

        # Flux sur la PPE
        flux_ppe <- flux_ppe - res_revalo[["ppe"]][["reprise"]]

        # Revalorisation cible
        revalo_contr <- res_revalo[["revalorisation"]]

        # Reste pour assouvir a la totalite des besoins contractuels
        reste_contr <- res_revalo[["reste"]]




        ## ###########################
        ## 2eme Etape :
        ## Besoins cibles => Uniquement si c'est possible
        ## ###########################

        # Initialisation de la revalorisation cible
        revalo_cible <- 0

        # Gestion des besoins cibles uniquement si l'on n'a pas deja du retard avec les besoins contractuels
        if((reste_contr == 0) & (besoin_cible > 0)){

            # Calcul de la revalorisation devant encore etre applique
            res_revalo <- calcul_revalo(besoin = besoin_cible, pb = pb, ppe = passif@provision@ppe, revalo_oblig = revalo_oblig)

            # Mise a jour des objets
            passif@provision@ppe <- res_revalo[["ppe"]][["ppe"]]
            pb <- res_revalo[["pb"]]
            revalo_oblig <- res_revalo[["revalo_oblig"]]

            # Flux sur la PPE
            flux_ppe <- flux_ppe - res_revalo[["ppe"]][["reprise"]]

            # Revalorisation cible totale
            revalo_cible <- res_revalo[["revalorisation"]]
        }


        # Revalorisation cible par produit
        revalo_cible_prod <- sapply(names(besoin_revalo[["besoin_cible"]]), simplify = FALSE, USE.NAMES = TRUE, function(x){
            if(besoin_cible >0)
                res <- besoin_revalo[["besoin_cible"]][[x]] * revalo_cible / besoin_cible
            else
                res <- 0
            return(res)})




        ## ###########################
        ## 3eme Etape :
        ## 1 - Revalorisation supplementaire
        ## ###########################

        # Proportion a attribuer par produit
        prod <- passif@hyp_passif@prop_pb$produit
        prop_pb <- passif@hyp_passif@prop_pb$proportion

        # Calcul de la revalo supplementaire en fonction des hypotheses
        revalo_supp_prod <- sapply(prod, function(x) prop_pb[which(prod == x)] * revalo_oblig,
                                   simplify = FALSE, USE.NAMES = TRUE)




        ## ###########################
        ## 3eme Etape :
        ## 2 - Dotation du reste de pb a la PPE
        ## ###########################

        if(pb > 0) {

            # Dotation de la PPE
            res_ppe <- dotation_ppe(ppe = passif@provision@ppe, montant = pb)

            # Mise a jour de l'objet
            passif@provision@ppe <- res_ppe[["ppe"]]

            # Flux
            flux_ppe <- flux_ppe + res_ppe[["dotation"]]
        }







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
        # revalo <- sapply(names(besoin_revalo[["besoin_cible"]]), USE.NAMES = TRUE, simplify = FALSE,
        #                  function(x) list(contr = besoin_revalo[["besoin_contr"]][[x]], cible = revalo_cible_prod[[x]],
        #                                   supp = revalo_supp_prod[[x]], prestation = revalo_prestation[[x]]))

        revalo <- list(contr = besoin_revalo[["besoin_contr"]],
                       cible = revalo_cible_prod,
                       supp = revalo_supp_prod,
                       prestation = revalo_prestation)







        ## ######################################################
        ## ######################################################
        ##
        ##              Resultat et Tresorerie
        ##
        ## ######################################################
        ## ######################################################

        # Extraction de differents montants
        total_revalo <- sum_list(revalo, 2L)

        # Mouvement sur la tresorerie
        mvt_treso <- -(total_revalo + flux_ppe)

        # Mouvement sur le resultat
        mvt_resultat <- -(0 + flux_ppe)






        # Output
        return(list(passif = passif,
                    revalorisation = revalo,
                    reste_contr = reste_contr,
                    flux_ppe = flux_ppe,
                    pm_cloture = pm_cloture,
                    besoin = list(besoin_contr = besoin_contr,
                                  besoin_cible = besoin_cible),
                    mouvement = list(treso = mvt_treso,
                                     resultat = mvt_resultat)))
    }
)
