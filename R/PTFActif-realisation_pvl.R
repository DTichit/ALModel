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
        ##              Extraction des PVL
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        pvl <- extract_pmvl_ptf_actif(ptf_actif)[["pvl"]]

        # Supprimer les PVL obligataires
        pvl[["obligation"]] <- 0

        # PVL totales
        pvl_totales <- sum_list(pvl, 1L)

        # Proportion de PVL
        prop_pvl <- sapply(names(pvl), function(x) return(pvl[[x]]/pvl_totales), simplify = FALSE)



        ## ######################################################
        ## ######################################################
        ##
        ##          Realisation des PMVL par PTF
        ##
        ## ######################################################
        ## ######################################################


        ## ###########################
        ##          PTF action
        ## ###########################

        if(pvl[["action"]] > 0) {

            # Appel de la fonction
            res_action <- realisation_pvl_action(action = ptf_actif@action, montant = montant * prop_pvl[["action"]])

            # Mise a jour de l'attribut
            ptf_actif@action <- res_action[["action"]]
        }


        ## ###########################
        ##          PTF immo
        ## ###########################

        if(pvl[["immobilier"]] > 0) {

            # Appel de la fonction
            res_immobilier <- realisation_pvl_immobilier(immobilier = ptf_actif@immobilier, montant = montant * prop_pvl[["immobilier"]])

            # Mise a jour de l'attribut
            ptf_actif@immobilier <- res_immobilier[["immobilier"]]
        }





        ## ######################################################
        ## ######################################################
        ##
        ##          Ajout des ventes a la tresorerie
        ##
        ## ######################################################
        ## ######################################################

        # Vente par produit
        vente <- sapply(names(pvl), function(x) {

            if(pvl[[x]] > 0)
                out <- get0(paste("res", x, sep = "_"))[["vente"]]
            else
                out <- 0

            return(out)

        }, simplify = FALSE)

        # Somme sur les ventes
        somme_vente <- sum_list(vente, 1L)

        # Mise a jour du solde de tresorerie
        ptf_actif@tresorerie@solde <- ptf_actif@tresorerie@solde + somme_vente





        ## ######################################################
        ## ######################################################
        ##
        ##              Agregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Liste agregeant l'ensemble des PVR
        pvr <- sapply(names(pvl), function(x) {

            if(pvl[[x]] > 0)
                out <- get0(paste("res", x, sep = "_"))[["pvr"]]
            else
                out <- 0

            return(out)

        }, simplify = FALSE)





        # Output
        return(list(ptf_actif = ptf_actif,
                    vente = vente,
                    pvr = pvr))
    }
)
