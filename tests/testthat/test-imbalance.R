test_that("imbalance measure is 0 for balanced vectors",{
	expect_equal(imbalance(factor(c("A","B","B","A"),levels=c("A","B"))),0)
	expect_equal(imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(A=50,B=50)),0)
	expect_equal(imbalance(factor(c("A","B","A","B","A","A"),levels=c("A","B")),treatment.ratios=c(A=2,B=1)),0)
	expect_equal(imbalance(factor(c("A","B","A","B","A","A"),levels=c("B","A")),treatment.ratios=c(A=2,B=1)),0)
	expect_equal(imbalance(factor(c("A","B","A","B","A","A"),levels=c("B","A")),treatment.ratios=c(A=100,B=50)),0)
	expect_equal(imbalance(factor(c(1,2,2,1,2,2),levels=c(2,1)),treatment.ratios=c('1'=1,'2'=2)),0)
})

test_that("imbalance measures for unbalanced vectors",{
	expect_equal(imbalance(factor(c("A","B","B"),levels=c("A","B"))),1)
	expect_equal(imbalance(factor(c("B","B","A"),levels=c("A","B"))),1)
	expect_equal(imbalance(factor( c("A","B"), levels=c("B","A") ), treatment.ratios=c(A=2,B=1)),1)
	expect_equal(imbalance(factor( c("A","B"), levels=c("A","B") ), treatment.ratios=c(A=3,B=4)),1)
	expect_equal(imbalance(factor( c("A","B"), levels=c("B","A") ), treatment.ratios=c(A=1,B=5)),4)
	expect_equal(imbalance(factor( c("A","B","B"), levels=c("A","B","C") ), treatment.ratios=c(A=2,B=3,C=5)),20)
})
