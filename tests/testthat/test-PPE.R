# ##################
# Tests Immobilier
# ##################

context("PPE")


# Creation des differents objets : PPE
montant_ppe <- c(100, 50, 50, 30, 20, 20, 10, 10)
ppe <- new("PPE", montant_ppe)

# PPE total
sum_ppe <- sum(ppe@ppe)



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Test dela dotation
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("dotation", {


    # Test de la dotation
    res <- dotation_ppe(ppe, 50)
    expect_equal(res$ppe@ppe, c(150, montant_ppe[-1L]))


})




#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Test de la reprise
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("reprise", {


    # 1 - Montant inferieur au total
    res <- reprise_ppe(ppe, 50)
    expect_equal(res$ppe@ppe, c(100, 50, 50, 30, 10, 0, 0, 0))
    expect_equal(res$reprise, 50)


    # 2 - Montant superieur au total
    res <- reprise_ppe(ppe, sum_ppe + 100)
    expect_equal(res$ppe@ppe, rep(0, 8L))
    expect_equal(res$reprise, sum_ppe)




})




#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Test de la reprise_ppb_8ans
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("reprise_ppb_8ans", {


    # 1 - Test de la fonction
    res <- reprise_ppe_8ans(ppe = ppe)
    expect_equal(res$ppe@ppe, c(montant_ppe[-8L], 0))
    expect_equal(res$ppe_8ans, montant_ppe[8L])




})
