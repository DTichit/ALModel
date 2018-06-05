#include <Rcpp.h>
#include <string.h>
using namespace Rcpp;

//' Cette fonction permet de calculer les duration pour un portfeuille obligataire.
//'
//' @name calcul_duration_obligation
//' @docType methods
//' @param coupon un vecteur contenant \code{numeric} les coupons.
//' @param mat_res un vecteur \code{numeric} contenant les maturites residuelles.
//' @param valeur_remboursement un vecteur \code{numeric} contenant les valeurs de remboursement.
//' @param yield un vecteur contenant la courbe de taux utilisee.
//' @author Damien Tichit pour Sia Partners
//' @export


// [[Rcpp::export]]
NumericVector calcul_duration_obligation(NumericVector coupon, NumericVector mat_res, NumericVector valeur_remboursement, NumericVector yield) {

    int nb_oblig = coupon.size();
    float flux;
    float numerateur, denominateur;

    NumericVector duration(nb_oblig);


    // Boucle sur le nombre d'obligations
    for(int i=0; i<nb_oblig; i++) {

        // Initialisation
        numerateur = 0;
        denominateur = 0;

        // Boucle sur les maturites
        for(int j=0; j<mat_res(i); j++) {

            // Calcul des flux sur l'annee
            flux = coupon(i);

            // Ajout de la VR a la maturite residuelle
            if((j+1) == mat_res(i))
                flux += valeur_remboursement(i);

            // Actualisation des flux
            flux *= exp(-(j + 1) * yield(j));


            // Comptabilisation des coupons
            numerateur += (j + 1) * flux;
            denominateur += flux;

        };


        // Calcul de la duration
        duration(i) = numerateur / denominateur;


    };

    return duration;
};

