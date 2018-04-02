# ##################
# Tests Action
# ##################

context("Action")


# Creation des differents objets : PTF et PTF_Cible
action <- new("Action", data.frame(valeur_marche = 100, valeur_comptable = 95))

# Collecte de certaines donnees
sum_vm_ptf <- sum(action@ptf$valeur_marche)
sum_vc_ptf <- sum(action@ptf$valeur_comptable)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Rebalancement du PTF Action
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("rebalancement_action", {


    ## 1 : ACHAT
    action_test <- action
    ptf_rebal <- rebalancement_action(action = action_test, alloc_cible = sum_vm_ptf + 200)
    expect_equal(ptf_rebal$action@ptf$valeur_marche, sum_vm_ptf + 200)
    expect_equal(ptf_rebal$action@ptf$valeur_comptable, 285)
    expect_equal(ptf_rebal$pmvr, 0)



    ## 2 : Vente
    action_test <- action
    ptf_rebal <- rebalancement_action(action = action_test, alloc_cible = sum_vm_ptf/2)
    expect_equal(ptf_rebal$action@ptf$valeur_marche, sum_vm_ptf/2)
    expect_equal(ptf_rebal$action@ptf$valeur_comptable, sum_vc_ptf/2)
    expect_equal(ptf_rebal$pmvr, 2.5)
})
