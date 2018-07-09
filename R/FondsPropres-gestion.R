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
        if((fp@report_a_nouveau <= 0) & (fp@resultat_exercice < 0))
            fp@report_a_nouveau <- fp@report_a_nouveau + fp@resultat_exercice
        else if((fp@report_a_nouveau <= 0) & (fp@resultat_exercice >= 0)) {

            temp <- fp@report_a_nouveau + fp@resultat_exercice

            fp@report_a_nouveau <- min(0, temp)
            fp@capitaux_propres <- fp@capitaux_propres + max(0, temp)

        } else
            fp@capitaux_propres <- fp@capitaux_propres + fp@resultat_exercice





        ## ###########################
        ##   Application de l'IS
        ## ###########################

        # Calcul de l'impot
        impots_societes <- fp@hypotheses$impots_societes * max(0, (resultat + fp@report_a_nouveau))




        ## ###########################
        ##   Application de la PS
        ## ###########################

        # Calcul de la participation aux salaries
        participation_salaries <- fp@hypotheses$participation_salaries * max(0, (resultat + fp@report_a_nouveau - impots_societes))




        ## ###########################
        ##   Mise a jour du resultat
        ## ###########################

        # Mise a jour de l'attribut
        fp@resultat_exercice <- resultat - impots_societes - participation_salaries





        ## ###########################
        ##   Mise a jour de l'emprunt
        ## ###########################

        # Somme des elements composant les FP
        fp_totaux <- calcul_fonds_propres(fp = fp)[["total"]]

        # Ajout d'un montant d'emprunt
        if(fp_totaux < 0)
            emprunt_total <- emprunt + abs(fp_totaux)

        # Mise a jour de l'attribut
        fp@dette <- fp@dette + if.is_null(get0("emprunt_total"), emprunt)





        ## ###########################
        ##      Flux sur la NAV
        ## ###########################

        # Calcul du montant
        flux_nav <- if.is_null(get0("emprunt_total"), emprunt) - (participation_salaries + impots_societes)



        # Output
        return(list(fp = fp,
                    flux_nav = flux_nav,
                    flux = list(montant_emprunte = if.is_null(get0("emprunt_total"), emprunt),
                                participation_salaries = participation_salaries,
                                impots_societes = impots_societes)))

    }
)
