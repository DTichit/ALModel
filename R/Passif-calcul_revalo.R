##' Fonction \code{calcul_revalo}
##'
##' Cette fonction permet de calculer la revalorisation applique en fonction du besoin, de la PB ainsi que de la PPE.
##'
##'
##' @name calcul_revalo
##' @docType methods
##' @param besoin est un \code{numeric}.
##' @param pb est un \code{numeric} representant le montant de PB restant.
##' @param ppe est un objet de type \code{\link{PPE}}.
##' @param revalo_oblig est un \code{numeric}. Il represente un montant de revalorisation obligatoire devant encore etre attribue.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include PPE-class.R
##'
setGeneric(name = "calcul_revalo", def = function(besoin, pb, ppe, revalo_oblig = 0) {standardGeneric("calcul_revalo")})
setMethod(
    f = "calcul_revalo",
    signature = c(besoin = "numeric", pb = "numeric", ppe = "PPE", revalo_oblig = "numeric"),
    definition = function(besoin, pb, ppe, revalo_oblig){



        ## ######################################################
        ## ######################################################
        ##
        ##                      Premiere etape :
        ##   Prise dans le montant de revalorisation obligatoire
        ##
        ## ######################################################
        ## ######################################################


        if(revalo_oblig > besoin) {

            # Mise a jour de la revalorisation obligatoire
            revalo_oblig <- revalo_oblig - besoin

            # Si la revalorisation obligatoire est superieur, la revalorisation est totale
            revalorisation <- besoin

            # Plus de besoin cible
            reste <- 0

        } else {

            # On prend le reste de l'obligatoire
            revalorisation <- revalo_oblig

            # Mise a jour du besoin restant
            reste <- besoin - revalo_oblig

            # Mise a jour de la revalorisation obligatoire
            revalo_oblig <- min(revalo_oblig, 0)

        }






        ## ######################################################
        ## ######################################################
        ##
        ##                      Deuxieme etape :
        ##              Prise dans le montant de PB
        ##
        ## ######################################################
        ## ######################################################


        if(reste > 0) {

            # On pioche dans la PB
            if(pb >= reste) {

                # Mise a jour du montant de PB
                pb <- pb - reste

                # La revalorisation est totale
                revalorisation <- besoin

                # Mise a jour du besoin restant
                reste <- 0

            } else {

                # Mise a jour de la revalorisation
                revalorisation <- revalorisation + pb

                # Mise a jour du reste
                reste <- reste - pb

                # Mise a jour du montant de PB
                pb <- 0

            }

        }






        ## ######################################################
        ## ######################################################
        ##
        ##          Troisieme etape :  Prise dans la PPE
        ##
        ## ######################################################
        ## ######################################################


        # S'il en manque, on pioche dans la PPE
        if(reste > 0) {

            # On pioche dans la PPE
            res_rep_ppe <- reprise_ppe(ppe, reste)

            # Mise jour de l'objet
            ppe <- res_rep_ppe[["ppe"]]

            # Montant repris
            montant_repris <- res_rep_ppe[["reprise"]]

            # La revalorisation est mise a jour
            revalorisation <- revalorisation + montant_repris

            # Mise a jour du reste
            reste <- reste - montant_repris


        }





        # Output
        return(list(ppe = ppe,
                    pb = pb,
                    revalorisation = revalorisation,
                    reste = reste,
                    revalo_oblig = revalo_oblig))
    }
)
