# Test for correct format of the input data:
# ---------------------

# Check that all required inputs are there: at least patient.data and treatment.ratios
test_that("assign.next.treatment returns an error when not all required inputs are specified.",{
	expect_error(assign.next.treatment()) 			# all arguments missing
	expect_error(assign.next.treatment(pdata1)) 		# missing treatment.ratios
	expect_error(assign.next.treatment(c("1"=1,"2"=1))) 	# missing patient.data
})

# Check that inputs are of the right structure
test_that("assign.next.treatment returns an error when inputs are not in the correct format.",{

	# Data for this test
	pdata1 <- data.frame(gender=c("M","M","F","M"),stage=c(3,3,3,4))
	t1 <- c(1,2,1)
	tr1 <- c("1"=1,"2"=1)
	t1b <- c("A","B","A")
	tr1b <- c(A=1,B=1)

	# Errors if patient.data is not a dataframe with at least one row:
	expect_error(assign.next.treatment(data.frame(), tr1b, t1b),
		"patient.data must be a dataframe with at least one row!")
	expect_error(assign.next.treatment(c(gender="F",stage=3),tr1,t1),
		"patient.data must be a dataframe with at least one row!")

	# Errors if treatment.ratios is not a numeric vector, unnamed
	# or has non-integer/negative values:
	expect_error(assign.next.treatment(pdata1, matrix(1,3,3), t1),
		"treatment.ratios must be a vector!")
	expect_error(assign.next.treatment(pdata1, c("1"="a","2"="b"), t1)) #non-numeric treatment.ratio
	expect_error(assign.next.treatment(pdata1, c("1"="1","2"="2"), t1)) #non-numeric treatment.ratio
	expect_error(assign.next.treatment(pdata1, c(1,1), t1),
		"The vector treatment.ratios must be named after the treatments!")
	expect_error(assign.next.treatment(pdata1, c(A=1,B=0.25), t1b),
		"Please use integers to specify treatment ratios!")
	expect_error(assign.next.treatment(pdata1, c(A=3.25,B=1), t1b),
		"Please use integers to specify treatment ratios!")
	expect_error(assign.next.treatment(pdata1, c(A=1,B=-2), t1b),
		"Each treatment ratio must be positive!")

	# Errors if previous.treatments is not a vector
	expect_error(assign.next.treatment(pdata1, tr1, matrix(2,2,2)),
			"treatments must be a vector or a factor!")

})

# Check input compatibility:
# 	- 'patient.data' must have one row more than 'treatments' has elements. 
#	- all treatments present in 'treatments' must be present in 'treatment.ratios'
#	  (but the reverse is not necessarily true)
test_that("assign.next.treatment returns an error when inputs are not compatible with each other.",{

	# Data for tests:
	pdata1 <- data.frame(gender=c("M","M","F","M"),stage=c(3,3,3,4))

	# Error if previous.treatments doesn't have length = ( nrow(patient.data)-1 )
	expect_error(assign.next.treatment(pdata1,c("1"=1,"2"=1),numeric()),
		"The number of previous treatments must equal the number of patients - 1!")
	expect_error(assign.next.treatment(pdata1,c("1"=1,"2"=1),c(1,2,1,1)),
		"The number of previous treatments must equal the number of patients - 1!")
	expect_error(assign.next.treatment(pdata1,c(A=1,B=1),c("A","B")),
		"The number of previous treatments must equal the number of patients - 1!")

	# if the treatments in previous.treatments do not correspond to the names in treatment.ratios
	expect_error(assign.next.treatment(pdata1,c(A=1,B=1),c(1,2,1)),
		"Not all treatments can be found in treatment.ratios.")
	expect_error(assign.next.treatment(pdata1,c("1"=1,"2"=1),c("A","B","A")),
		"Not all treatments can be found in treatment.ratios.")
	expect_error(assign.next.treatment(pdata1,c("1"=1,"2"=2),factor(c(1,2,2,1))))
})


# Test for correct output format
# ---------------------
test_that("Output is character and has length 1",{

	# Data for tests:
	pdata <- data.frame(gender=c("F","M","F","M","F","F"),stage=c(3,3,3,4,4,4),SNP=c("pos","neg","pos","neg","pos","neg"))

	# check that output is a character vector and has length 1
	expect_equal(is.character( assign.next.treatment(pdata, c(A=1,B=1), c("A","A","A","A","B")) ),TRUE)
	expect_length(assign.next.treatment(pdata, c(A=1,B=1), c("A","A","A","A","B")),1)
})

# Test for function calls that should be equivalent
# ---------------------

#Output not changed when treatment.ratios is multiplied with a constant value:
test_that("multiplying all values in treatment.ratios with a constant does not alter output",{

	# Data for tests:
	pdata1 <- data.frame(gender=c("M","M","F","M"),stage=c(3,3,3,4))
	pdata2 <- data.frame(f1=c("a","b","c","a","b","c"), f2=c(1,1,2,2,1,1), f3 = c("x","x","x","a","a","a") )
	ft <- factor( c("A","A","A","B","B"), levels=c("B","A") )

	#the tests:
	expect_equal( assign.next.treatment( pdata1, c(A=1,B=2), c("A","B","A"), p=0 ),
		assign.next.treatment( pdata1, c(B=100,A=50), c("A","B","A") , p=0  ))
	expect_equal( assign.next.treatment( pdata1, c("1"=1,"2"=1), c(1,1,1) , p=0 ),
		assign.next.treatment( pdata1, c("2"=20,"1"=20), c(1,1,1), p=0  ))
	expect_equal( assign.next.treatment( pdata2, c(A=2,B=3), ft , p=0 ), 
		assign.next.treatment( pdata2, c(A=40,B=60), ft , p=0 ) )
})

#Output not changed when order of treatments is changed within treatment.ratios
test_that("Changing the order of the treatments within treatment.ratios does not alter output",{

	# Data for tests:
	pdata1 <- data.frame(gender=c("M","M","F","M"),stage=c(3,3,3,4))
	pdata2 <- data.frame(f1=c("a","b","c","a","b","c"), f2=c(1,1,2,2,1,1), f3 = c("x","x","x","a","a","a") )
	ft <- factor( c("A","A","A","B","B"), levels=c("B","A") )

	#the tests:
	expect_equal( assign.next.treatment( pdata1, c(A=1,B=2), c("A","B","A"), p=0 ),
		assign.next.treatment( pdata1, c(B=2,A=1), c("A","B","A") , p=0  ))
	expect_equal( assign.next.treatment( pdata1, c("1"=2,"2"=3), c(1,1,1) , p=0 ),
		assign.next.treatment( pdata1, c("2"=3,"1"=2), c(1,1,1), p=0  ))
	expect_equal( assign.next.treatment( pdata2, c(A=2,B=5), ft , p=0 ), 
		assign.next.treatment( pdata2, c(B=5,A=2), ft , p=0 ) )
})


#The order in which the previous patients came in doesn't matter:
test_that("Changing the order in which previous patients came in does not affect the output.",{

	#Data:
	pdata <- data.frame(f1=c("a","b","c","a","b","c"), f2=c(1,1,2,2,1,1), f3 = c("x","x","x","a","a","a") )
	tr <- c(1,1,1,2,2)

	new.order <- sample(seq(1,length(tr)),length(tr))
	pdata_shuf <- pdata[c(new.order,length(tr)+1),]
	tr_shuf <- tr[new.order]

	expect_equal( assign.next.treatment( pdata, c("1"=1,"2"=1), tr, p=0),
		assign.next.treatment(pdata_shuf, c("1"=1,"2"=1), tr_shuf, p=0) )

})


# Test for specific output values
# ---------------------

# Some trivial examples: 
test_that("When only one treatment has not been included yet, this will be next (p=0).",{
	pdata1 <- data.frame(gender=c("M","M","F","M"),stage=c(3,3,3,4))
	expect_equal( assign.next.treatment( pdata1, c(A=1,B=1), c("A","A","A"), p=0) , "B")
	expect_equal( assign.next.treatment( pdata1, c(B=1,A=1), c("A","A","A"), p=0) , "B")
	expect_equal( assign.next.treatment( pdata1, c(A=1,B=1), c("B","B","B"), p=0), "A")
	expect_equal( assign.next.treatment( pdata1, c(B=1,A=1), c("B","B","B"), p=0), "A")
	expect_equal( assign.next.treatment( pdata1, c(B=1,A=1), factor(c("A","A","A"),levels=c("A","B")),p=0),"B")
	expect_equal( assign.next.treatment( pdata1, c(B=1,A=1), factor(c("A","A","A"),levels=c("B","A")),p=0),"B")
})

test_that("When two treatments have not been included yet, the one with the highest value in treatment.ratios will be chosen first.",{
	#example 1
	pdata1 <- data.frame(gender=c("M","M","F","M"),stage=c(3,3,3,4))
	tr <- c("A","A","A")
	expect_equal( assign.next.treatment( pdata1, c(A=1,B=1,C=2), tr, p=0) , "C")

	#example 2: this is the first patient
	pdata2 <- data.frame(gender="M", stage=3)
	expect_equal( assign.next.treatment( pdata2, c(A=1,B=2), p=0), "B")
})

test_that("When there is no imbalance, one of the treatment is still chosen.",{

	pdata <- data.frame(a=rep(1,3)) 	#all patients are the same.
	TR <- c(A=1,B=1)
	prev.tr <- c("A","B")			#previous treatments reflect treatment.ratios
	next.tr <- assign.next.treatment( pdata, TR, prev.tr, p=0)
	expect_equal( is.element(next.tr, names(TR)), TRUE)

})

# Example from paper:
test_that("Example from minimization paper yields treatment 3.",{

	#Implementation 1: 3 treatments 1,2,3
		# Data for tests:
		pdata <- data.frame(	F1 = c(rep(1,9), rep(2,8),rep(1,10), rep(2,7),rep(1,9), rep(2,7)),
					F2 = c(rep(1,8), rep(2,9), rep(1,6), rep(2,11), rep(1, 7), rep(2, 9)),
					F3 = c(rep(1,8), rep(2,4), rep(3,5), rep(1,8), rep(2,5), rep(3,4), 
						rep(1,8), rep(2,3), rep(3,5)))
		pdata <- rbind(pdata,c(1,2,2))
		tr <- c(rep(1,17),rep(2,17), rep(3,16))

		#the test:
		expect_equal( assign.next.treatment( pdata, c("1"=1,"2"=1,"3"=1), tr, p=0 ),"3")

	#Implementation 2: set tr control=(1,2), X=(3) with ratio 2:1
		tr2 <- c(rep("control",2*17), rep("X",16))
		expect_equal( assign.next.treatment( pdata, c("control"=2,"X"=1), tr2, p=0), "X")

})

