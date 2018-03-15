# ##################
# Tests Immobilier
# ##################

context("Immobilier")


# Creation des differents objets : PTF et PTF_Cible
immo <- load_immobilier("D:/damie/Documents/MEMOIRE/01_Modele/SiALM/tests/data/01_System/01_Actifs/01_Portefeuilles/Immobilier.csv")
immo_cible <- load_immobilier_cible("D:/damie/Documents/MEMOIRE/01_Modele/SiALM/tests/data/01_System/01_Actifs/02_Hypotheses/01_PortefeuillesCibles/01_Portfeuilles/Immobilier.csv")

# Collecte de certaines donnees
sum_vm_ptf <- sum(immo@ptf$valeur_marche)
sum_vm_cible <- sum(immo_cible@ptf$valeur_marche)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Rebalancement du PTF Immobilier
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("rebalancement_immobilier", {


    # 1 : Construction du PTF avec l'ensemble des immos
    immo_test <- immo
    ptf_complet <- rebalancement_immobilier(immo = immo_test, immo_cible = immo_cible, alloc_cible = 300)$immo
    expect_equal(ptf_complet@ptf$valeur_marche, c(200,85,12,3))
    expect_equal(ptf_complet@ptf$nombre, c(3,1.65,0.12,0.1))
    expect_equal(ptf_complet@ptf$loyer, c(1, 0.2, 0.05, 0.005))



    ## 2 : ACHAT
    # 2.1 : Manque 1 immo cible
    immo_test@ptf <- ptf_complet@ptf[-4L,]
    ptf_test <- rebalancement_immobilier(immo = immo_test, immo_cible = immo_cible, alloc_cible = 300)$immo
    expect_equal(ptf_complet@ptf, ptf_test@ptf)

    # 2.2 : Tout est present
    immo_test@ptf <- ptf_complet@ptf
    ptf_test <- rebalancement_immobilier(immo = immo_test, immo_cible = immo_cible, alloc_cible = 400)$immo
    expect_equal(ptf_test@ptf$valeur_marche, c(200,85,92,23))
    expect_equal(ptf_test@ptf$loyer, c(1, 0.2, 0.05, 0.005))



    ## 3 : VENTE
    # 3.1 : Vente 1 immo non-cible (inferieur a vm_1)
    immo_test@ptf <- ptf_complet@ptf
    ptf_test <- rebalancement_immobilier(immo = immo_test, immo_cible = immo_cible, alloc_cible = 200)$immo
    expect_equal(ptf_test@ptf$valeur_marche, c(100,85,12,3))
    expect_equal(ptf_test@ptf$loyer, c(1, 0.2, 0.05, 0.005))

    # 3.2 : Vente 1+ immos non-cibles (superieur a vm_1)
    immo_test@ptf <- ptf_complet@ptf
    ptf_test <- rebalancement_immobilier(immo = immo_test, immo_cible = immo_cible, alloc_cible = 50)$immo
    expect_equal(ptf_test@ptf$valeur_marche, c(35,12,3))
    expect_equal(ptf_test@ptf$loyer, c(0.2, 0.05, 0.005))

    # 3.2 : Vente toutes les immos non-cibles (egal aux vm cibles)
    immo_test@ptf <- ptf_complet@ptf
    ptf_test <- rebalancement_immobilier(immo = immo_test, immo_cible = immo_cible, alloc_cible = 15)$immo
    expect_equal(ptf_test@ptf$valeur_marche, c(12,3))
    expect_equal(ptf_test@ptf$loyer, c(0.05, 0.005))

    # 3.2 : Vente toutes les immos non-cibles et immos cibles (superieur aux vm cibles)
    immo_test@ptf <- ptf_complet@ptf
    ptf_test <- rebalancement_immobilier(immo = immo_test, immo_cible = immo_cible, alloc_cible = 10)$immo
    expect_equal(ptf_test@ptf$valeur_marche, c(8,2))
    expect_equal(ptf_test@ptf$loyer, c(0.05, 0.005))


})
