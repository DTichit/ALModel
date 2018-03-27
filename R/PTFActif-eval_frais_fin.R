##' Fonction \code{eval_frais_fin}
##'
##' Cette fonction permet d'evaluer les frais relatifs au portfeuille actif d'une compagnie d'assurance. Il est possible de modeliser des frais par produits financiers
##' mais egalement des frais proportionnels aux VM.
##'
##' @name eval_frais_fin
##' @docType methods
##' @param ptf_actif est un objet de type \code{\link{PTFActif}}.
##' @param frais_fin est un objet de type \code{data.frame}.
##' @author Damien Tichit pour Sia Partners
##' @include Actif-class.R HypActif-class.R
##'
setGeneric(name = "eval_frais_fin", def = function(ptf_actif, frais_fin, prod_fin) {standardGeneric("eval_frais_fin")})
setMethod(
    f = "eval_frais_fin",
    signature = c(ptf_actif = "PTFActif", frais_fin = "data.frame", prod_fin = "list"),
    definition = function(ptf_actif, frais_fin, prod_fin) {


        warning("Est ce qu'il y a des frais sur la tresorerie ?")

        # Extraction des donnees
        produit <- names(prod_fin)



        ## ######################################################
        ## ######################################################
        ##
        ##                  Calcul des frais
        ##
        ## ######################################################
        ## ######################################################


        ## ###########################
        ## Frais sur les produits financiers
        ## ###########################

        frais_prod <- sapply(produit, function(x) return(frais_fin[which(frais_fin$produit == x), "frais_prod"] * prod_fin[[x]]),
                             USE.NAMES = TRUE, simplify = FALSE)



        ## ###########################
        ## Frais proportionnels aux VM
        ## ###########################

        # Calcul des VM totales
        alloc <- allocation_ptf_actif(ptf_actif)

        # Calcul des frais
        frais_vm <- sapply(produit, function(x) return(frais_fin[which(frais_fin$produit == x), "frais_vm"] * alloc[["vm_actif"]][[x]]),
                           USE.NAMES = TRUE, simplify = FALSE)






        ## ######################################################
        ## ######################################################
        ##
        ##                  Aggregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        frais <- list(prod = frais_prod,
                      vm = frais_vm)




        # Output
        return(list(frais = frais))
    }
)
