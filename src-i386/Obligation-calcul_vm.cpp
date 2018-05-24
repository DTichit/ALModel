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
//' @include Obligation-class.R


// [[Rcpp::export]]
NumericMatrix calcul_vm_obligation(NumericVector coupon, NumericVector mat_res, NumericVector valeur_remboursement, NumericVector zspread, NumericVector yield) {

    int mat_max = max(mat_res);
    int n_oblig = coupon.size();
    int len_yield = yield.size();
    bool yield_bool = (len_yield > 0);

    if(mat_max <= 1) mat_max = 2;

    NumericMatrix out(n_oblig, mat_max);


    for (int i=0; i<n_oblig; i++) {
        for (int j=0; j<mat_max; j++) {

            if((j+1) <= mat_res(i)) {
                out(i,j) = coupon(i);

                if((j+1) == mat_res(i))
                    out(i,j) += valeur_remboursement(i);
            }
            else {
                out(i,j) = 0; // Initialisation
            }


            if(yield_bool)
                out(i,j) *= exp(-(yield(j) + zspread(i)));

        }
    }

    return out;
}

