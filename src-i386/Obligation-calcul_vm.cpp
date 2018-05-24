#include <Rcpp.h>
#include <string.h>
using namespace Rcpp;

//' Cette fonction permet de calculer les valeurs de marche pour un portfeuille obligataire.
//'
//' @name calcul_vm_obligation
//' @docType methods
//' @param coupon un vecteur contenant \code{numeric} les coupons.
//' @param mat_res un vecteur \code{numeric} contenant les maturites residuelles.
//' @param valeur_remboursement un vecteur \code{numeric} contenant les valeurs de remboursement.
//' @param spread un vecteur \code{numeric} contenant les spread.
//' @param yield un vecteur contenant la courbe de taux utilisee.
//' @author Damien Tichit pour Sia Partners
//' @export
//' @include


// [[Rcpp::export]]
NumericVector calcul_vm_obligation(NumericVector coupon, NumericVector mat_res, NumericVector valeur_remboursement, NumericVector spread, NumericVector yield) {

    int nb_oblig = coupon.size();

    NumericVector vm(nb_oblig);


    // Boucle sur le nombre d'obligations
    for(int i=0; i<nb_oblig; i++) {

        if(mat_res(i) > 0) {

            // Initialisation
            vm(i) = 0;

            // Boucle sur les maturites
            for(int j=0; j<mat_res(i); j++) {

                // Comptabilisation des coupons
                vm(i) += coupon(i) * exp(-(j + 1) * (spread(i) + yield(j)));

                // Ajout de la VR a la maturite residuelle
                if((j+1) == mat_res(i))
                    vm(i) += valeur_remboursement(i) * exp(-(j + 1) * (spread(i) + yield(j)));

            }

        }

        else {
            vm(i) = valeur_remboursement(i) * exp(-spread(i));
        };

    };

    return vm;
};

