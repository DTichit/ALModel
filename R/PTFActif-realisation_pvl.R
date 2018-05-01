##' Fonction \code{realisation_pvl_ptf_actif}
##'
##' Cette fonction permet de realiser des plus values lattentes.
##'
##' @name realisation_pvl_ptf_actif
##' @docType methods
##' @param ptf_actif est un objet de type \code{PTFActif}.
##' @param montant est un \code{numeric} indiquant le montant de plus value devant etre realise.
##' @author Damien Tichit pour Sia Partners
##' @include PTFActif-class.R
##'
setGeneric(name = "realisation_pvl_ptf_actif", def = function(ptf_actif, montant) {standardGeneric("realisation_pvl_ptf_actif")})
setMethod(
    f = "realisation_pvl_ptf_actif",
    signature = c(ptf_actif = "PTFActif", montant = "numeric"),
    definition = function(ptf_actif, montant){



        ## ######################################################
        ## ######################################################
        ##
        ##          Extraction des PMVL par PTF
        ##
        ## ######################################################
        ## ######################################################


        ## ###########################
        ##          PTF action
        ## ###########################

        # Appel de la fonction
        res_action <- realisation_pvl_action(action = ptf_actif@action, montant = 0)

        # Mise a jour de l'attribut
        ptf_actif@action <- res_action[["action"]]



        ## ###########################
        ##          PTF oblig
        ## ###########################

        # Appel de la fonction
        res_oblig <- realisation_pvl_obligation(obligation = ptf_actif@obligation, montant = 0)

        # Mise a jour de l'attribut
        ptf_actif@obligation <- res_oblig[["obligation"]]



        ## ###########################
        ##          PTF immo
        ## ###########################

        # Appel de la fonction
        res_immo <- realisation_pvl_immobilier(immobilier = ptf_actif@immobilier, montant = 0)

        # Mise a jour de l'attribut
        ptf_actif@immobilier <- res_immo[["immobilier"]]





        ## ######################################################
        ## ######################################################
        ##
        ##          Ajout des ventes a la tresorerie
        ##
        ## ######################################################
        ## ######################################################

        # Total vente
        somme_vente <- res_action[["vente"]] + res_oblig[["vente"]] + res_immo[["vente"]]

        # Mise a jour du solde de tresorerie
        ptf_actif@tresorerie@solde <- ptf_actif@tresorerie@solde + somme_vente





        ## ######################################################
        ## ######################################################
        ##
        ##              Agregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Liste agregeant l'ensemble des PMVL
        pvr <- list(action = res_action[["pvr"]],
                    obligation = res_oblig[["pvr"]],
                    immobilier = res_immo[["pvr"]])

        # Liste agregeant les ventes
        vente <- list(action = res_action[["vente"]],
                      obligation = res_oblig[["vente"]],
                      immobilier = res_immo[["vente"]])





        # Output
        return(list(ptf_actif = ptf_actif,
                    vente = vente,
                    pvr = pvr))
    }
)
