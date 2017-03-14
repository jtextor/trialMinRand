#' Treatment Imbalance
#' 
#' Provides a measure of treatment imbalance with respect to a desired ratio of
#' treatments.
#'
#' @param treatments a factor or a character/numeric vector containing a sequence of treatments.
#' @param treatment.ratios a named vector containing the desired treatment ratios.
#' Treatment.ratios should at least contain all unique elements of the treatments vector.
#'
#' @details
#' A treatment vector has imbalance 0 if the treatments occur in the desired ratio.
#' Otherwise, the imbalance is positive.
#' 
#' @examples
#' # A balanced treatment vector
#' imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(A=1,B=1))
#' # Still balanced if ratio is given as 50:50 instead of 1:1
#' imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(A=50,B=50))
#' # Treatments can also be provided as a numeric vector
#' imbalance(c(1,2,2,1),treatment.ratios=c("1"=1,"2"=1))
#' # Treatments can be provided as a character vector
#' imbalance(c("A","B","B","A"), treatment.ratios=c(A=1,B=1))
#' 
#' @export
imbalance <- function( treatments, treatment.ratios ) {

	#Check input	
	.check.treatments( treatments )
	.check.treatment.ratios( treatment.ratios )
	.check.treatment.names( treatments, treatment.ratios )

	#Count how often each treatment occurs in the current vector.
	#Add the treatments not in there to the table with a 'zero'.
	xt <- setNames(rep(0,length(names(treatment.ratios))),names(treatment.ratios))
	xt[names(table( treatments ))] <- table( treatments )

	#Assign each treatment a weight such that multiplying the elements of treatment.ratios with the
	#elements of treatment.weights yields an equal, integer number for all treatments. To get
	#treatment.weights that are also integer, this number can be the smallest common multiple (scm)
	#of the elements in treatment.ratios. Dividing the scm by the treatment.ratios vector then yields the
	#vector with weights:
	treatment.weights <- .scm(treatment.ratios)/treatment.ratios[names(xt)]

	#Now multiply treatment counts with treatment weights. The difference of the range yields the (weighted)
	#imbalance:
	weighted.xt <- xt*treatment.weights
	diff(range(weighted.xt))
}

# The greatest common divisor of a vector of integer numbers.
.gcd <- function( x ){

	#Calculate gcd with "Euclidian algorithm"
	if( length(x) < 2 ){
		return(x)
	}
	if( length(x) > 2 ){
		return( .gcd( c( x[1], .gcd( x[-1] ) ) ) )
	}

	x <- sort(x)
	xd <- diff(x)
	if( xd==0 ){
		return(x[1])
	}
	.gcd( c(x[1], xd ) )
}

# The smallest common multiple of a vector of integer numbers.
.scm <- function( x ){
	if( length(x) < 2 ){
		return(x)
	}
	if( length(x) > 2 ){
		return( .scm( c( x[1], .scm( x[-1] ) ) ) )
	}
	return( prod(x)  / .gcd(x) )
}

# Format check for treatment ratios:
.check.treatment.ratios <- function(x){
	if( !is.vector(x) ){
		stop("treatment.ratios must be a vector!")
	}
	if( !is.numeric(x) ){
		stop("treatment.ratios must be a (named) numeric vector!")
	}
	if( !all( unname(x) ==  as.integer(x) ) ){
		stop("Please use integers to specify treatment ratios! (E.g., c(A=2,B=1) instead of c(A=1,B=0.5))")
	}
	if( any( x <= 0 ) ){
		stop("Each treatment ratio must be positive!")
	}

	if( is.null(names(x)) ){
		stop("The vector treatment.ratios must be named after the treatments! (E.g., c(A=2,B=1) instead of c(1,2))")

	}
}

# Format check for the actual treatments.
.check.treatments <- function(x){
	if( !is.vector(x) & !is.factor(x) ){
		stop("treatments must be a vector or a factor!")
	}
}

# Last check: names in treatment.ratios must correspond to the actual treatment names. 
.check.treatment.names <- function(treatments,treatment.ratios){

	tnames <- names(treatment.ratios)
	if(any(!is.element(treatments,tnames))){
		stop("Not all treatments can be found in treatment.ratios. Please check that treatments are spelled correctly and that treatment.ratios is complete.")
	}
	if(is.factor(treatments)){
		if(any(!is.element(levels(treatments),tnames))){
			stop("Treatment is a factor, but levels do not correspond to the names in treatment.ratios.")
		}
	}	

}



#' @importFrom stats runif setNames
#' @importFrom utils tail
NULL

#' Assign Next Treatment for Patient
#'
#' This function assigns the next treatment of a patient,
#' given the previous treatments.
#'
#' @param patient.data a data frame containing the strata for each patient.
#' The last row of this data frame is assumed to contain the strata for the
#' new patient, who will receive the next treatment. Strata can be given
#' as a factor, or as columns that can be interpreted as a factor.
#' 
#' @param previous.treatments a factor or numeric/character vector containing 
#' the previous treatments. 'previous.treatments' should have an element for
#' each row in patient.data except the last (which represents the current patient
#' for whom no treatment has been determined yet). 
#'
#' @param treatment.ratios a named vector containing the desired ratios
#' in which treatments are to be given.
#'
#' @param p probability with which a random treatment will be chosen 
#' (with the desired ratio) instead of choosing the treatment that minimizes
#' imbalance.
#'
#' @examples
#' # Define patient data
#' d <- data.frame( F1=factor(c(1,2,2),levels=1:2),
#'   F2=factor(c(1,1,2),levels=1:2) )
#' # Define previous treatments
#' tr <- factor( c("A","A"), levels=c("A","B") )
#' # Call function to obtain next treatment
#' next.tr <- assign.next.treatment( d, c(A=2,B=1), tr )
#'
#' @export
assign.next.treatment <- function(
	patient.data, treatment.ratios, previous.treatments=character(0), p=0.3 ){
	
	# CHECK INPUT:
	# patient.data must be a dataframe.
	if (class(patient.data)!="data.frame"){
		stop("patient.data must be a dataframe with at least one row!")
	}

	if ( nrow(patient.data) == 0 ){
		stop("patient.data must be a dataframe with at least one row!")
	}

	# treatment.ratios should be a named vector of non-negative integers.
	.check.treatment.ratios( treatment.ratios )

	# previous.treatments should be a vector of length nrow(patient.data) -1 and
	# should only contain treatments specified in the treatment.ratios vector.
	.check.treatments( previous.treatments )
	previous.treatments <- as.character(previous.treatments)

	if( length(previous.treatments) != nrow(patient.data)-1 ){
		stop(paste("The number of previous treatments must equal the number of patients - 1! Input data has",length(previous.treatments),"previous treatments and",nrow(patient.data),"patients."))
	}
	.check.treatment.names( previous.treatments, treatment.ratios )
	

	# CALCULATION
	treatments <- names(treatment.ratios)
	N <- length(treatments)

	# Compute the total imbalances for each treatment.
	factors <- names(patient.data)
	imbalances <- sapply( treatments, function(tr){ 
		sum( sapply( factors, function(f){
			f.values <- patient.data[[f]]
			imbalance( c(previous.treatments,tr)[f.values==tail(f.values,1)],
				treatment.ratios)
			} ) ) 
	} )

	if( runif(1) > p && diff(range(imbalances))>0 ){
		# Sort treatments in increasing order by imbalance, randomly breaking ties.
		treatments <- treatments[order(imbalances, sample(N))]
		# Return treatment with smallest imbalance.
		r <- treatments[1]
	} else {
		# Pick treatment at random, using biased coin to attain desired
		# treatment ratio.
		r <- sample( rep(treatments,treatment.ratios[treatments]), 1 )
	}
	r
}

simulate.trial <- function( initial.data, initial.treatments, N,... ){
	d <- initial.data
	tr <- initial.treatments
	while( nrow(d) < N ){
		d <- rbind(d,apply(d,2,function(x)sample(x,1)))
		tr <- .c.fac( tr, assign.next.treatment(d,tr,...) )
	}
	list( patient.data=d, treatments=tr )
}

simulate.random.trial <- function( initial.data, initial.treatments, N ){
	d <- initial.data
	tr <- initial.treatments
	while( nrow(d) < N ){
		d <- rbind(d,apply(d,2,function(x)sample(x,1)))
		tr <- .c.fac( tr, sample(as.factor(levels(tr)),1) )
	}
	list( patient.data=d, treatments=tr )
}

total.imbalance <- function( patient.data, treatments, ... ){
	apply( patient.data, 2, function(f){
		sum( by( treatments, f, function(x) imbalance(x,...) ) )
	})
}

