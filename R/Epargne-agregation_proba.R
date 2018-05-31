##' Fonction \code{agregation_proba}.
##'
##' Cette fonction permet d'agreger les probas par PM ou par nombre de contrats.
##'
##' @name agregation_proba
##' @docType methods
##' @param epargne est un objet de type \code{\link{Epargne}}.
##' @param proba est un \code{data.frame} contenant les probabilites devant etres agregees.
##' @param variable est un \code{character}. Elle indique selon quelle variable l'agregation doit etre faite.
##' @author Damien Tichit pour Sia Partners
##' @export
##' @include Epargne-class.R
##'
setGeneric(name = "agregation_proba", def = function(epargne, proba, variable) {standardGeneric("agregation_proba")})
setMethod(
    f = "agregation_proba",
    signature = c(epargne = "Epargne", proba = "data.frame", variable = "character"),
    definition = function(epargne, proba, variable){


        ## ###########################
        ##  Extraction de donnees
        ## ###########################

        # Portfeuille
        name_ptf <- names(epargne@ptf)
        tmg_ptf <- .subset2(epargne@ptf, which(name_ptf == "tmg"))

        # Variable selon laquelle il faut agreger
        if(variable == "pm")
            variable_agreg <- .subset2(epargne@ptf, which(name_ptf == "pm"))
        else if(variable == "nb_contr")
            variable_agreg <- .subset2(epargne@ptf, which(name_ptf == "nb_contr"))
        else
            stop("[agregation_proba] : Variable non connue")



        # Extraction des probabilites
        proba <- 1 - proba[,-1L]

        # Calcul des nouveaux nombres de contrats au cours de la projection
        new_variable_agreg <- variable_agreg * cbind(rep(1, length(variable_agreg)), t(apply(proba, 1, cumprod)))

        # TMG uniques
        unique_tmg <- unique(tmg_ptf)





        ## ###########################
        ## Calcul des nouvelles probas
        ## ###########################

        # Initialisation du dataframe
        temp <- data.frame()

        # Calcul pour chacun des tmg
        for(tmg in unique_tmg) {

            #  Numeros de ligne correspondant au tmg
            num <- which(tmg_ptf == tmg)

            # Aggregation des probabilites
            new_proba <- sapply(1L:ncol(proba), function(id) {
                return(weighted.mean(x = proba[num, id], w = new_variable_agreg[num, id]))
            })

            # Stockage des probas
            temp <- rbind(temp, c(tmg = tmg, 1 - new_proba))
        }

        # Renommer les colonnes
        colnames(temp) <- c("tmg", 1L:ncol(proba))

        # Tri par tmg
        temp <- temp[order(temp$tmg),]


        # Output
        return(temp)
    }
)
