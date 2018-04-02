# ##################
# Tests Immobilier
# ##################

context("Immobilier")


# Creation des differents objets : PTF et PTF_Cible
immo <- new("Immobilier", data.frame(valeur_marche = 100, valeur_comptable = 95))

# Collecte de certaines donnees
sum_vm_ptf <- sum(immo@ptf$valeur_marche)
sum_vc_ptf <- sum(immo@ptf$valeur_comptable)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Rebalancement du PTF Immobilier
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("rebalancement_immo", {


    ## 1 : ACHAT
    immo_test <- immo
    ptf_rebal <- rebalancement_immobilier(immo = immo_test, alloc_cible = sum_vm_ptf + 200)
    expect_equal(ptf_rebal$immo@ptf$valeur_marche, sum_vm_ptf + 200)
    expect_equal(ptf_rebal$immo@ptf$valeur_comptable, 285)
    expect_equal(ptf_rebal$pmvr, 0)



    ## 2 : Vente
    immo_test <- immo
    ptf_rebal <- rebalancement_immobilier(immo = immo_test, alloc_cible = sum_vm_ptf/2)
    expect_equal(ptf_rebal$immo@ptf$valeur_marche, sum_vm_ptf/2)
    expect_equal(ptf_rebal$immo@ptf$valeur_comptable, sum_vc_ptf/2)
    expect_equal(ptf_rebal$pmvr, 2.5)
})
