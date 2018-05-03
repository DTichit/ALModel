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
        pmvl <- list(action = pmvl_action[["pmvl"]],
                     obligation = pmvl_oblig[["pmvl"]],
                     immobilier = pmvl_immo[["pmvl"]])

        # Liste agregeant les PVL
        pvl <- list(action = pmvl_action[["pvl"]],
                    obligation = pmvl_oblig[["pvl"]],
                    immobilier = pmvl_immo[["pvl"]])

        # Liste agregeant les MVL
        mvl <- list(action = pmvl_action[["mvl"]],
                    obligation = pmvl_oblig[["mvl"]],
                    immobilier = pmvl_immo[["mvl"]])




        # Output
        return(list(pmvl = pmvl,
                    pvl = pvl,
                    mvl = mvl))
    }
)
