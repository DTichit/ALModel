##' Fonction \code{gestion_fin_projection_passif}
##'
##' Cette fonction permet de gerer la fin de projection du modele sur le passif de la compagnie d'assurance.
##'
##' @name gestion_fin_projection_passif
##' @docType methods
##' @param passif est un objet de type \code{\link{Passif}}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "gestion_fin_projection_passif", def = function(passif) {standardGeneric("gestion_fin_projection_passif")})
setMethod(
    f = "gestion_fin_projection_passif",
    signature = c(passif = "Passif"),
    definition = function(passif){



        ## ######################################################
        ## ######################################################
        ##
        ##    Extraction des differentes provisions techniques
        ##
        ## ######################################################
        ## ######################################################

        # Appel de la fonction
        provisions_techniques <- calcul_pt(passif = passif)


        # Extraction des PM
        pm <- sum_list(provisions_techniques[["pt"]][["pm"]], 1L)

        # Extraction de la PPE
        ppe <- provisions_techniques[["pt"]][["ppe"]]

        # Extraction de la PRE
        pre <- provisions_techniques[["pt"]][["pre"]]





        ## ######################################################
        ## ######################################################
        ##
        ##              Mise a jour des attributs
        ##
        ## ######################################################
        ## ######################################################

        # Mise a 0 des PM
        passif@ptf_passif@epargne@ptf$pm <- rep(0, nrow(passif@ptf_passif@epargne@ptf))

        # Mise a 0 de la PPE
        passif@provision@ppe@ppe <- rep(0, 8L)







        ## ######################################################
        ## ######################################################
        ##
        ##              Agregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Montant a verser en fin de projection
        fin_projection_assureurs    <- 0
        fin_projection_assures      <- pm + ppe





        # Output
        return(list(passif = passif,
                    fin_projection = list(assures = fin_projection_assures,
                                          assureurs = fin_projection_assureurs)))
    }
)
