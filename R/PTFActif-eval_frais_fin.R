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



        ## ######################################################
        ## ######################################################
        ##
        ##                  Calcul des frais
        ##
        ## ######################################################
        ## ######################################################

        # Extraction du nom des produits
        produit <- as.character(frais_fin$produit)



        ## ###########################
        ## Frais sur les produits financiers
        ## ###########################

        # Extraction des frais
        frais_prod <- frais_fin$frais_prod

        frais_prod <- sapply(produit, function(x) return(frais_prod[which(produit == x)] * prod_fin[[x]]),
                             USE.NAMES = TRUE, simplify = FALSE)



        ## ###########################
        ## Frais proportionnels aux VM
        ## ###########################

        # Extraction des frais
        frais_vm <- frais_fin$frais_vm

        # Calcul des VM totales
        alloc <- allocation_ptf_actif(ptf_actif)[["vm_actif"]]

        # Calcul des frais
        frais_vm <- sapply(produit, function(x) return(frais_vm[which(produit == x)] * alloc[[x]]),
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
