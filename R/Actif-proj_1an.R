##' Fonction \code{proj_1an_actif}.
##'
##' Cette fonction permet de projeter horizon 1 an l'actif d'une compagnie d'assurance.
##'
##' @name proj_1an_actif
##' @docType methods
##' @param actif est un objet de type \code{\link{Actif}}.
##' @author Damien Tichit pour Sia Partners
##' @include Actif-class.R
##'
setGeneric(name = "proj_1an_actif", def = function(actif) {standardGeneric("proj_1an_actif")})
setMethod(
    f = "proj_1an_actif",
    signature = c(actif = "Actif"),
    definition = function(actif){



        ## ######################################################
        ## ######################################################
        ##
        ##              Revalorisation des actifs
        ##
        ## ######################################################
        ## ######################################################

        # Revalorisation du portfeuille
        res_revalo_actif <- revalo_ptf_actif(ptf_actif = actif@ptf_actif)

        # Mise a jour de l'objet
        actif@ptf_actif <- res_revalo_actif[["ptf_actif"]]




        ## ######################################################
        ## ######################################################
        ##
        ##            Gestion du protfeuille financier
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        res_proj_ptf <- proj_1an_ptf_actif(ptf_actif = actif@ptf_actif)

        # MaJ de l'objet
        actif@ptf_actif <- res_proj_ptf[["ptf_actif"]]




        ## ######################################################
        ## ######################################################
        ##
        ##        Evalutation des frais financiers
        ##
        ## ######################################################
        ## ######################################################

        # Calcul des frais financiers
        frais <- 0

        # Appel de la fonction
        warning("Frais financiers : Cette partie doit etre code !! : A voir si elle doit etre placee ici !")





        ## ######################################################
        ## ######################################################
        ##
        ##              Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Produits financiers
        prod_fin <- do.call(sum, res_proj_ptf[["flux"]][["prod_fin"]])

        # Vente d'actifs
        vente <- do.call(sum, res_proj_ptf[["flux"]][["vente"]])

        # PMVL
        pmvl <- do.call(sum, res_revalo_actif[["pmvl"]])




        # Output
        return(list(actif = actif,
                    flux = list(prod_fin = prod_fin,
                                vente = vente),
                    pmvl = pmvl,
                    frais = frais))
    }
)
