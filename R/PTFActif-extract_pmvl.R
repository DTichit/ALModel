##' Fonction \code{extract_pmvl_ptf_actif}
##'
##' Cette fonction permet de determiner les PMVL pouvant etre realises sur les differents portefeuille.
##'
##' @name extract_pmvl_ptf_actif
##' @docType methods
##' @param ptf_actif est un objet de type \code{PTFActif}.
##' @author Damien Tichit pour Sia Partners
##' @include PTFActif-class.R
##'
setGeneric(name = "extract_pmvl_ptf_actif", def = function(ptf_actif) {standardGeneric("extract_pmvl_ptf_actif")})
setMethod(
    f = "extract_pmvl_ptf_actif",
    signature = c(ptf_actif = "PTFActif"),
    definition = function(ptf_actif){



        ## ######################################################
        ## ######################################################
        ##
        ##          Extraction des PMVL par PTF
        ##
        ## ######################################################
        ## ######################################################


        ## ###########################
        ##    PMVL sur le PTF oblig
        ## ###########################

        pmvl_oblig <- extract_pmvl_obligation(ptf_actif@obligation)


        ## ###########################
        ##    PMVL sur le PTF action
        ## ###########################

        pmvl_action <- extract_pmvl_action(ptf_actif@action)


        ## ###########################
        ##    PMVL sur le PTF immo
        ## ###########################

        pmvl_immo <- extract_pmvl_immobilier(ptf_actif@immobilier)





        ## ######################################################
        ## ######################################################
        ##
        ##              Agregation des donnees
        ##
        ## ######################################################
        ## ######################################################

        # Liste agregeant l'ensemble des PMVL
        pmvl <- list(action = pmvl_action,
                     obligation = pmvl_oblig,
                     immobilier = pmvl_immo)

        # Liste agregeant les PVL
        pvl <- list(action = if.is_empty(pmvl_action[which(pmvl_action > 0)], 0),
                    obligation = if.is_empty(pmvl_oblig[which(pmvl_oblig > 0)], 0),
                    immobilier = if.is_empty(pmvl_immo[which(pmvl_immo > 0)], 0))

        # Liste agregeant les MVL
        mvl <- list(action = if.is_null(pmvl_action[which(pmvl_action <= 0)], 0),
                    obligation = if.is_null(pmvl_oblig[which(pmvl_oblig <= 0)], 0),
                    immobilier = if.is_null(pmvl_immo[which(pmvl_immo <= 0)], 0))




        # Output
        return(list(pmvl = pmvl,
                    pvl = pvl,
                    mvl = mvl))
    }
)
