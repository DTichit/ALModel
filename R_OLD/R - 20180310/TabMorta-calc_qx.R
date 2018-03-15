##' Fonction \code{calc_qx}.
##'
##' Cette fonction permet de calculer des probas de deces pour un age et une table de mortalite donnes.
##' Il est possible d'indiquer plusieurs ages differents sous forme de vecteur.
##'
##' @name calc_qx
##' @docType methods
##' @param tab_morta est un objet de type \code{\link{TabMorta}}.
##' @param age est un \code{integer}.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include TabMorta-class.R
##'
setGeneric(name = "calc_qx", def = function(tab_morta, age) {standardGeneric("calc_qx")})
setMethod(
    f = "calc_qx",
    signature = c(tab_morta = "TabMorta", age = "integer"),
    definition = function(tab_morta, age){

        # Donnees de sorties
        table <- tab_morta@table
        col_names <- colnames(table)
        num_age <- which(col_names == "age")
        num_lx  <- which(col_names == "lx")

        # Extraction des donnes
        age_table <- .subset2(table, num_age)
        lx_table <- .subset2(table, num_lx)

        # Age maximum de la table
        age_max <- max(age_table)

        # Ages appliques
        age_ap   <- pmin(age, age_max)
        age_app1 <- pmin(age + 1L, age_max)

        # Numeros de ligne du lx et du lx+1
        row_lx   <- match(age_ap, age_table)
        row_lxp1 <- match(age_app1, age_table)

        # Extraction des lx
        lx   <- lx_table[row_lx]
        lxp1 <- lx_table[row_lxp1]

        # Calcul du qx
        qx    <- 1 - (lxp1 / lx)

        # Output
        return(qx)
    }
)
