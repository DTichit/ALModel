##' Fonction \code{gestion_fonds_propres}
##'
##' Cette fonction permet de gerer les fonds propres.
##'
##' @name gestion_fonds_propres
##' @docType methods
##' @param fp est un objet de type \code{FondsPropres}.
##' @param resultat est un \code{numeric} representant le resultat de l'annee.
##' @param emprunt est un \code{numeric}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Passif-class.R
##'
setGeneric(name = "gestion_fonds_propres", def = function(fp, resultat, emprunt) {standardGeneric("gestion_fonds_propres")})
setMethod(
    f = "gestion_fonds_propres",
    signature = c(fp = "FondsPropres", resultat = "numeric", emprunt = "numeric"),
    definition = function(fp, resultat, emprunt){




        ## ###########################
        ## Mise a jour du report a nouveau avec le resultat precedent
        ## ###########################

        # Mise a jour de l'attribut
        fp@report_a_nouveau <- fp@report_a_nouveau + fp@resultat_exercice




        ## ###########################
        ##   Mise a jour du resultat
        ## ###########################

        # Mise a jour de l'attribut
        fp@resultat_exercice <- resultat




        ## ###########################
        ## Calcul des nouveaux FP totaux
        ## ###########################

        # Somme des elements
        fp_totaux <- calcul_fonds_propres(fp = fp)[["total"]]





        ## ###########################
        ##   Mise a jour de l'emprunt
        ## ###########################

        # Ajout d'un montant d'emprunt
        if(fp_totaux < 0)
            emprunt_total <- emprunt + abs(fp_totaux)

        # Mise a jour de l'attribut
        fp@dette <- fp@dette + if.is_null(get0("emprunt_total"), emprunt)





        # Output
        return(list(fp = fp,
                    montant_emprunte = if.is_null(get0("emprunt_total"), emprunt)))


    }
)
