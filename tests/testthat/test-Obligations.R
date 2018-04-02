# ##################
# Tests Obligations
# ##################

context("Obligation")


# Creation des differents objets : PTF et PTF_Cible
oblig <- new("Obligation", data.frame(id_mp = paste0("ob-", 1L:2L), cible = FALSE, mat_res = c(6L, 12L), valeur_comptable = c(90, 400),
                                     valeur_marche = c(100, 440), coupon = c(0.002, 0.005), nominal = c(100, 400)))

oblig_cible <- new("Obligation", data.frame(id_mp = paste0("ob-", 1L:3L), cible = TRUE, mat_res = c(3L, 5L, 7L), valeur_marche = c(100, 125, 120),
                                           coupon = c(0.005, 0.01, 0.02), nominal = c(100, 100, 100), prop = c(0.2, 0.5, 0.3)))



# Collecte de certaines donnees
sum_vm_ptf <- sum(oblig@ptf$valeur_marche)
sum_vc_cible <- sum(oblig_cible@ptf$valeur_comptable)
sum_vm_cible <- sum(oblig_cible@ptf$valeur_marche)
sum_vc_ptf <- sum(oblig@ptf$valeur_comptable)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Rebalancement du PTF Immobilier
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("rebalancement_obligation", {


    ## 1 : ACHAT
    # 1.1 : Achat sans intersections entre les 2 PTFs
    oblig_test <- oblig
    oblig_cible_test <- oblig_cible
    ptf_rebal <- rebalancement_obligation(oblig = oblig_test, oblig_cible = oblig_cible_test, alloc_cible = sum_vm_ptf + 100)
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_marche), sum_vm_ptf + 100)
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_comptable), sum_vc_ptf + 100)
    expect_equal(ptf_rebal$oblig@ptf$mat_res, c(3L, 5L, 6L, 7L, 12L))
    expect_equal(ptf_rebal$oblig@ptf$valeur_marche, c(20, 50, 100, 30, 440))
    expect_equal(ptf_rebal$oblig@ptf$valeur_comptable, c(20, 50, 90, 30, 400))
    expect_equal(ptf_rebal$oblig@ptf$nominal, c(20, 40, 100, 25, 400))
    expect_equal(ptf_rebal$oblig@ptf$coupon, c(0.005, 0.01, 0.002, 0.02, 0.005))
    expect_equal(ptf_rebal$pmvr, 0)



    # 1.2 : Achat avec 1 intersection entre les 2 PTFs
    oblig_test <- oblig
    oblig_test@ptf[1L, "mat_res"] <- 7L
    oblig_cible_test <- oblig_cible
    ptf_rebal <- rebalancement_obligation(oblig = oblig_test, oblig_cible = oblig_cible_test, alloc_cible = sum_vm_ptf + 100)
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_marche), sum_vm_ptf + 100)
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_comptable), sum_vc_ptf + 100)
    expect_equal(ptf_rebal$oblig@ptf$mat_res, c(3L, 5L, 7L, 12L))
    expect_equal(ptf_rebal$oblig@ptf$valeur_marche, c(20, 50, 130, 440))
    expect_equal(ptf_rebal$oblig@ptf$valeur_comptable, c(20, 50, 120, 400))
    expect_equal(ptf_rebal$oblig@ptf$nominal, c(20, 40, 125, 400))
    expect_equal(ptf_rebal$oblig@ptf$coupon, c(0.005, 0.01, 0.0056, 0.005))
    expect_equal(ptf_rebal$pmvr, 0)



    # 1.2 : Achat avec full intersection entre les 2 PTFs
    oblig_test <- oblig
    oblig_test@ptf[, "mat_res"] <- c(3L, 7L)
    oblig_cible_test <- oblig_cible
    ptf_rebal <- rebalancement_obligation(oblig = oblig_test, oblig_cible = oblig_cible_test, alloc_cible = sum_vm_ptf + 100)
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_marche), sum_vm_ptf + 100)
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_comptable), sum_vc_ptf + 100)
    expect_equal(ptf_rebal$oblig@ptf$mat_res, c(3L, 5L, 7L))
    expect_equal(ptf_rebal$oblig@ptf$valeur_marche, c(120, 50, 470))
    expect_equal(ptf_rebal$oblig@ptf$valeur_comptable, c(110, 50, 430))
    expect_equal(ptf_rebal$oblig@ptf$nominal, c(120, 40, 425))
    expect_equal(ptf_rebal$oblig@ptf$coupon, c(0.0025, 0.01, 0.005882353))
    expect_equal(ptf_rebal$pmvr, 0)






    ## 2 : VENTE
    # 2.1 : Vente sur une seule ligne
    oblig_test <- oblig
    oblig_cible_test <- oblig_cible
    ptf_rebal <- rebalancement_obligation(oblig = oblig_test, oblig_cible = oblig_cible_test, alloc_cible = sum_vm_ptf - 40)
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_marche), sum_vm_ptf - 40)
    expect_equal(ptf_rebal$oblig@ptf$mat_res, oblig_test@ptf$mat_res)
    expect_equal(ptf_rebal$oblig@ptf$valeur_marche, c(60, 440))
    expect_equal(ptf_rebal$oblig@ptf$valeur_comptable, c(54, 400))
    expect_equal(ptf_rebal$oblig@ptf$nominal, c(60, 400))
    expect_equal(ptf_rebal$oblig@ptf$coupon, oblig_test@ptf$coupon)
    expect_equal(ptf_rebal$pmvr, 4)



    # 2.2 : Vente complete de la premiere ligne
    oblig_test <- oblig
    oblig_cible_test <- oblig_cible
    ptf_rebal <- rebalancement_obligation(oblig = oblig_test, oblig_cible = oblig_cible_test, alloc_cible = sum_vm_ptf - 100)
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_marche), sum_vm_ptf - 100)
    expect_equal(ptf_rebal$oblig@ptf$mat_res, oblig_test@ptf$mat_res[-1L])
    expect_equal(ptf_rebal$oblig@ptf$valeur_marche, oblig_test@ptf$valeur_marche[-1L])
    expect_equal(ptf_rebal$oblig@ptf$valeur_comptable, oblig_test@ptf$valeur_comptable[-1L])
    expect_equal(ptf_rebal$oblig@ptf$nominal, oblig_test@ptf$nominal[-1L])
    expect_equal(ptf_rebal$oblig@ptf$coupon, oblig_test@ptf$coupon[-1L])
    expect_equal(ptf_rebal$pmvr, 10)



    # 2.2 : Vente complete de la premiere ligne & une partie de la seconde
    oblig_test <- oblig
    oblig_cible_test <- oblig_cible
    ptf_rebal <- rebalancement_obligation(oblig = oblig_test, oblig_cible = oblig_cible_test, alloc_cible = sum_vm_ptf - 150)
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_marche), sum_vm_ptf - 150)


})
