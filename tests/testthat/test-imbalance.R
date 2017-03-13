# Test for correct format of the input data:
# ---------------------

# Check that all required inputs are there
test_that("imbalance returns an error when not all required inputs are specified.",{
	expect_error(imbalance())
	expect_error(imbalance(c("A","B","B","A")))
	expect_error(imbalance(treatment.ratios=c(A=1,B=2)))
	expect_error(imbalance(c(A=1,B=2)))
})

# Check that inputs are of the right structure (if not, there should be erros from .check.treatments() and .check.treatment.ratios()
test_that("imbalance returns an error when inputs are not in the correct format.",{

	# Errors if treatments is not a vector 
		expect_error(imbalance(matrix(2,2,2),treatment.ratios=c(A=1,B=2)),
			"treatments must be a vector or a factor!")

	# Errors if treatment.ratios is unnamed, not a numeric vector 
	# or has non-integer/negative values:
		expect_error(imbalance(c("A","B","B","A"), matrix(1,3,3)),
			"treatment.ratios must be a vector!")
		expect_error(imbalance(c("A","B","B","A"), c(A="a",B="b")))
		expect_error(imbalance(c("A","B","B","A"), c(A="1",B="2")))
		expect_error(imbalance(c("A","B","B","A"), c(1,1)),
			"The vector treatment.ratios must be named after the treatments!")
		expect_error(imbalance(c("A","B","B","A"), c(A=1,B=0.25)),
			"Please use integers to specify treatment ratios!")
		expect_error(imbalance(c("A","B","B","A"), c(A=3.25,B=1)),
			"Please use integers to specify treatment ratios!")
		expect_error(imbalance(c("A","B","B","A"), c(A=1,B=-2)),
			"Each treatment ratio must be positive!")
})

# Check that elements in 'treatments' correspond to the names of 'treatment.ratios'
test_that("all elements in 'treatments' are present in 'treatment.ratios' ",{

	# Different treatment names in 'treatments' vs 'treatment.ratios'
		expect_error(imbalance(c(1,2,2,4),c(A=1,B=1,C=1)),
			"Not all treatments can be found in treatment.ratios.")
	
	# There is a treatment that is absent from 'treatment.ratios'
		expect_error(imbalance(c(1,2,2,4),c("1"=1,"2"=1)),
			"Not all treatments can be found in treatment.ratios.")
		expect_error(imbalance(c("banana","apple","banana","apple","pear"),c(apple=1,banana=1)),
			"Not all treatments can be found in treatment.ratios.")

	# Treatments is a factor, but the levels do not correspond to names of treatment.ratios
		expect_error(imbalance(factor(c(1,2,2,1),levels=c(1,2,3)),c("1"=1,"2"=2)))
})


# Test for correct output format
# ---------------------
test_that("Output is numeric and has length 1",{
	expect_equal(is.numeric(imbalance(c("apple"),c(apple=1,banana=1))),TRUE)
	expect_equal(is.numeric(imbalance(factor(c("A","B","B","A"),levels=c("A","B")),
		treatment.ratios=c(A=1,B=1))),TRUE)
	expect_length(imbalance(c("placebo","placebo","X","placebo","X"),c(placebo=1,X=2)),1)
	expect_length(imbalance(c(3,4,3,3,4,5),c("3"=1,"5"=1,"4"=1)),1)
})


# Test for function calls that should be equivalent
# ---------------------


#Encoding type of treaments as factor() character() or numeric() doesn't matter
test_that("Encoding type of treatments ( - eg as numeric(), factor() or character() ) does not affect the output.",{
	expect_equal(imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(A=1,B=1)),
		imbalance(factor(c(2,1,1,2),levels=c(2,1)),treatment.ratios=c("1"=1,"2"=1)))
	expect_equal(imbalance(c(1,2,3,2), treatment.ratios=c("1"=1,"2"=2,"3"=3)),
		imbalance(c("apple","pear","banana","pear"), treatment.ratios=c(apple=1,pear=2,banana=3)))	
})


#Output not changed when treatment.ratios is multiplied with a constant value:
test_that("multiplying all values in treatment.ratios with a constant does not alter output",{
	expect_equal(imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(A=1,B=1)),
		imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(A=50,B=50)))
	expect_equal(imbalance(c(2,1,1,2),treatment.ratios=c("1"=1,"2"=2)),
		imbalance(c(2,1,1,2), treatment.ratios=c("1"=4,"2"=8)))
	expect_equal(imbalance(c("pear","apple","pear"), treatment.ratios=c("pear"=2,"apple"=3)),
		imbalance(c("pear","apple","pear"), treatment.ratios=c("pear"=40,"apple"=60)))
	expect_equal(imbalance(c("pear"), treatment.ratios=c("pear"=4,"apple"=1)),
		imbalance(c("pear"), treatment.ratios=c("pear"=40,"apple"=10)))
})

#Output not changed when treatments have a different order within 'treatments' vector
test_that("Changing the order of the treatments within the treatments vector does not alter output",{
	expect_equal(imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(A=1,B=1)),
		imbalance(factor(c("A","A","B","B"),levels=c("A","B")),treatment.ratios=c(A=1,B=1)))
	expect_equal(imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(A=1,B=1)),
		imbalance(factor(c("A","B","A","B"),levels=c("A","B")),treatment.ratios=c(A=1,B=1)))
	expect_equal(imbalance(c("pear","apple","pear"), treatment.ratios=c("pear"=2,"apple"=3)),
		imbalance(c("pear","pear","apple"), treatment.ratios=c("pear"=2,"apple"=3)))
	expect_equal(imbalance(c(1,2,1,2,1),treatment.ratios=c("1"=1,"2"=5)),
		imbalance(c(2,2,1,1,1),treatment.ratios=c("1"=1,"2"=5)))
})

#Output not changed when order of treatments is changed within treatment.ratios
test_that("Changing the order of the treatments within treatment.ratios does not alter output",{
	expect_equal(imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(A=1,B=2)),
		imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(B=2,A=1)))
	expect_equal(imbalance(c("pear","apple","pear"), treatment.ratios=c("pear"=2,"apple"=3)),
		imbalance(c("pear","apple","pear"), treatment.ratios=c("apple"=3,"pear"=2)))
	expect_equal(imbalance(c(1,2,1,2,1),treatment.ratios=c("1"=1,"2"=5)),
		imbalance(c(1,2,1,2,1),treatment.ratios=c("2"=5,"1"=1)))
	expect_equal(imbalance(c("placebo","placebo","X","placebo","X"),c(placebo=1,X=2)),
		imbalance(c("placebo","placebo","X","placebo","X"),c(X=2,placebo=1)))
})

#In a 1:1 ratio, it doesn't matter which treatment is present less
test_that("In a 1:1 ratio, it it doesn't matter which treatment is present less.",{
	expect_equal(imbalance(factor(c("A","B","B"),levels=c("A","B")),treatment.ratios=c(A=1,B=1)),
		imbalance(factor(c("A","A","B"),levels=c("A","B")),treatment.ratios=c(B=1,A=1)))
	expect_equal(imbalance(c("pear"), treatment.ratios=c("pear"=3,"apple"=3)),
		imbalance(c("apple"), treatment.ratios=c("pear"=3,"apple"=3)))
	expect_equal(imbalance(c(1,2,2,2), treatment.ratios=c("2"=50,"1"=50)),
		imbalance(c(1,2,1,1), treatment.ratios=c("2"=50,"1"=50)))
})


# Test for specific output values
# ---------------------

# Function imbalance should return an imbalance of 0 when the vector is balanced. 
test_that("imbalance measure is 0 for balanced vectors",{
	expect_equal(imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(A=1,B=1)),0)
	expect_equal(imbalance(factor(c("A","B","A","B","A","A"),levels=c("A","B")),treatment.ratios=c(A=2,B=1)),0)
	expect_equal(imbalance(factor(c(1,2,2,1,2,2,2,2,1),levels=c(2,1)),treatment.ratios=c('1'=1,'2'=2)),0)
	expect_equal(imbalance(c("placebo","X","X","placebo","X"),treatment.ratios=c(placebo=2,X=3)),0)
})

# Imbalance is not 0 for unbalanced vectors
test_that("imbalance measures for unbalanced vectors",{
	expect_equal(imbalance(factor( c("A","B","B") ,levels=c("A","B") ), treatment.ratios=c(A=1,B=1)),1)
	expect_equal(imbalance(factor( c("B","B","A"), levels=c("A","B") ), treatment.ratios=c(A=1,B=1)),1)
	expect_equal(imbalance(factor( c("A","B"), levels=c("B","A") ), treatment.ratios=c(A=2,B=1)),1)
	expect_equal(imbalance(factor( c("A","B"), levels=c("A","B") ), treatment.ratios=c(A=3,B=4)),1)
	expect_equal(imbalance(factor( c("A","B"), levels=c("B","A") ), treatment.ratios=c(A=1,B=5)),4)
	expect_equal(imbalance(factor( c("A","B","B"), levels=c("A","B","C") ), treatment.ratios=c(A=2,B=3,C=5)),20)
})


##OTHER:
# ---------------------

# Not all the treatments in the treatment.ratios have to be present in 'treatments'. 
test_that("Not all treatments need to be present in the treatments vector",{
		expect_equal(imbalance(c("apple"),c(apple=1,banana=1)),1)
		expect_equal(imbalance(c("apple"),c(banana=1,apple=1)),1)
		expect_equal(imbalance(c("banana"),c(banana=1,apple=1)),1)
		expect_equal(imbalance(factor(c(1,2,2,1),levels=c(1,2,3)),c("1"=1,"2"=1,"3"=1)),2)
		expect_equal(imbalance(factor(c(1,2,2,1),levels=c(1,2,3)),c("2"=1,"1"=1,"3"=1)),2)
		expect_equal(imbalance(c(1,2,2,1),c("2"=1,"1"=1,"3"=1)),2)
})
