# ##################
# Tests Obligations
# ##################

context("Obligation")


# Creation des differents objets : PTF et PTF_Cible
oblig <- new("Obligation", data.frame(id_mp = paste0("ob-", 1L:2L), cible = FALSE,
                                      maturite = c(6L, 12L), duree_detention = c(1L, 2L),
                                      valeur_nette_comptable = c(90, 400), nominal = c(100, 400),
                                      valeur_achat = c(90, 400), valeur_remboursement = c(90, 400),
                                      valeur_marche = c(100, 440), coupon = c(0.002, 0.005), tri = c(0, 0), spread = c(0, 0),
                                      type = c("souv", "souv"), rating = c(1, 1)))

oblig_cible <- new("Obligation", data.frame(id_mp = paste0("ob-", 1L:3L), cible = TRUE, maturite = c(3L, 5L, 7L), valeur_marche = c(100, 125, 120),
                                            valeur_remboursement = c(100, 100, 100),
                                           coupon = c(0.005, 0.01, 0.02), nominal = c(100, 100, 100), prop = c(0.2, 0.5, 0.3),
                                           type = c("souv", "souv", "souv"), rating = c(1, 1, 1)))


# Collecte de certaines donnees
sum_vm_ptf <- sum(oblig@ptf$valeur_marche)
sum_vnc_ptf <- sum(oblig@ptf$valeur_nette_comptable)
sum_vm_cible <- sum(oblig_cible@ptf$valeur_marche)


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
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_nette_comptable), sum_vnc_ptf + 100)
    expect_equal(ptf_rebal$oblig@ptf$maturite, c(6L, 12L, 3L, 5L, 7L))
    expect_equal(ptf_rebal$oblig@ptf$duree_detention, c(1L, 2L, 1L, 1L, 1L))
    expect_equal(ptf_rebal$oblig@ptf$valeur_nette_comptable, c(90, 400, 20, 50, 30))
    expect_equal(ptf_rebal$oblig@ptf$nominal, c(100, 400, 20, 40, 25))
    expect_equal(ptf_rebal$oblig@ptf$valeur_achat, c(90, 400, 20, 50, 30))
    expect_equal(ptf_rebal$oblig@ptf$valeur_remboursement, c(90, 400, 20, 40, 25))
    expect_equal(ptf_rebal$oblig@ptf$valeur_marche, c(100, 440, 20, 50, 30))
    expect_equal(ptf_rebal$oblig@ptf$coupon, c(0.002, 0.005, 0.005, 0.01, 0.02))
    expect_equal(ptf_rebal$pmvr, 0)
    expect_equal(ptf_rebal$flux, 100)





    ## 2 : VENTE
    # 2.1 : Vente sur une seule ligne
    oblig_test <- oblig
    oblig_cible_test <- oblig_cible
    ptf_rebal <- rebalancement_obligation(oblig = oblig_test, oblig_cible = oblig_cible_test, alloc_cible = sum_vm_ptf - 40)
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_marche), sum_vm_ptf - 40)
    expect_equal(oblig@ptf[2L,], ptf_rebal$oblig@ptf[2L,])
    expect_equal(ptf_rebal$oblig@ptf$maturite, oblig_test@ptf$maturite)
    expect_equal(ptf_rebal$oblig@ptf$duree_detention, oblig_test@ptf$duree_detention)
    expect_equal(ptf_rebal$oblig@ptf$coupon, oblig_test@ptf$coupon)
    expect_equal(ptf_rebal$oblig@ptf$valeur_nette_comptable[1L], 54)
    expect_equal(ptf_rebal$oblig@ptf$nominal[1L], 60)
    expect_equal(ptf_rebal$oblig@ptf$valeur_achat[1L], 54)
    expect_equal(ptf_rebal$oblig@ptf$valeur_remboursement[1L], 54)
    expect_equal(ptf_rebal$oblig@ptf$valeur_marche[1L], 60)
    expect_equal(ptf_rebal$pmvr, 4)
    expect_equal(ptf_rebal$flux, -40)



    # 2.2 : Vente complete de la premiere ligne
    oblig_test <- oblig
    oblig_cible_test <- oblig_cible
    ptf_rebal <- rebalancement_obligation(oblig = oblig_test, oblig_cible = oblig_cible_test, alloc_cible = sum_vm_ptf - oblig_test@ptf$valeur_marche[1L])
    expect_equal(ptf_rebal$oblig@ptf, oblig@ptf[-1L,])
    expect_equal(ptf_rebal$pmvr, 10)
    expect_equal(ptf_rebal$flux, -oblig_test@ptf$valeur_marche[1L])



    # 2.2 : Vente complete de la premiere ligne & une partie de la seconde
    oblig_test <- oblig
    oblig_cible_test <- oblig_cible
    ptf_rebal <- rebalancement_obligation(oblig = oblig_test, oblig_cible = oblig_cible_test, alloc_cible = sum_vm_ptf - 150)
    expect_equal(sum(ptf_rebal$oblig@ptf$valeur_marche), sum_vm_ptf - 150)


})
