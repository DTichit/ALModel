# ##################
# Tests Action
# ##################

context("Action")


# Creation des differents objets : PTF et PTF_Cible
action <- load_action("D:/damie/Documents/MEMOIRE/01_Modele/SiALM/tests/data/01_System/01_Actifs/01_Portefeuilles/Actions.csv")
action_cible <- load_action_cible("D:/damie/Documents/MEMOIRE/01_Modele/SiALM/tests/data/01_System/01_Actifs/02_Hypotheses/01_PortefeuillesCibles/01_Portfeuilles/Actions.csv")

# Collecte de certaines donnees
sum_vm_ptf <- sum(action@ptf$valeur_marche)
sum_vm_cible <- sum(action_cible@ptf$valeur_marche)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Rebalancement du PTF Action
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("rebalancement_action", {


    # 1 : Construction du PTF avec l'ensemble des actions
    action_test <- action
    ptf_complet <- rebalancement_action(action = action_test, action_cible = action_cible, alloc_cible = 300)$action
    expect_equal(ptf_complet@ptf$valeur_marche, c(200,85,12,3))
    expect_equal(ptf_complet@ptf$nombre, c(3,1.65,0.12,0.1))
    expect_equal(ptf_complet@ptf$dividende, c(1, 0.2, 0.05, 0.005))



    ## 2 : ACHAT
    # 2.1 : Manque 1 action cible
    action_test@ptf <- ptf_complet@ptf[-4L,]
    ptf_test <- rebalancement_action(action = action_test, action_cible = action_cible, alloc_cible = 300)$action
    expect_equal(ptf_complet@ptf, ptf_test@ptf)

    # 2.2 : Tout est present
    action_test@ptf <- ptf_complet@ptf
    ptf_test <- rebalancement_action(action = action_test, action_cible = action_cible, alloc_cible = 400)$action
    expect_equal(ptf_test@ptf$valeur_marche, c(200,85,92,23))
    expect_equal(ptf_test@ptf$dividende, c(1, 0.2, 0.05, 0.005))



    ## 3 : VENTE
    # 3.1 : Vente 1 action non-cible (inferieur a vm_1)
    action_test@ptf <- ptf_complet@ptf
    ptf_test <- rebalancement_action(action = action_test, action_cible = action_cible, alloc_cible = 200)$action
    expect_equal(ptf_test@ptf$valeur_marche, c(100,85,12,3))
    expect_equal(ptf_test@ptf$dividende, c(1, 0.2, 0.05, 0.005))

    # 3.2 : Vente 1+ actions non-cibles (superieur a vm_1)
    action_test@ptf <- ptf_complet@ptf
    ptf_test <- rebalancement_action(action = action_test, action_cible = action_cible, alloc_cible = 50)$action
    expect_equal(ptf_test@ptf$valeur_marche, c(35,12,3))
    expect_equal(ptf_test@ptf$dividende, c(0.2, 0.05, 0.005))

    # 3.2 : Vente toutes les actions non-cibles (egal aux vm cibles)
    action_test@ptf <- ptf_complet@ptf
    ptf_test <- rebalancement_action(action = action_test, action_cible = action_cible, alloc_cible = 15)$action
    expect_equal(ptf_test@ptf$valeur_marche, c(12,3))
    expect_equal(ptf_test@ptf$dividende, c(0.05, 0.005))

    # 3.2 : Vente toutes les actions non-cibles et actions cibles (superieur aux vm cibles)
    action_test@ptf <- ptf_complet@ptf
    ptf_test <- rebalancement_action(action = action_test, action_cible = action_cible, alloc_cible = 10)$action
    expect_equal(ptf_test@ptf$valeur_marche, c(8,2))
    expect_equal(ptf_test@ptf$dividende, c(0.05, 0.005))


})
