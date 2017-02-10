#' Treatment Imbalance
#' 
#' Provides a measure of treatment imbalance with respect to a desired ratio of
#' treatments.
#'
#' @param treatments a factor containing a sequence of treatments.
#' @param treatment.ratios a named vector containing the desired treatment ratios.
#' By default, each treatment is given in the same frequency.
#'
#' @details
#' A treatment vector has imbalance 0 if the treatments occur in the desired ratio.
#' Otherwise, the imbalance is positive.
#' 
#' @examples
#' # A balanced treatment vector
#' imbalance(factor(c("A","B","B","A"),levels=c("A","B")))
#' # Still balanced if ratio is given as 50:50 instead of the default 1:1
#' imbalance(factor(c("A","B","B","A"),levels=c("A","B")),treatment.ratios=c(A=50,B=50))
#' 
#' @export
imbalance <- function( treatments, treatment.ratios=setNames(rep(1,length(treatments)),levels(treatments)) ) {
	.check.treatments( treatments )
	.check.treatment.ratios( treatment.ratios )
	xt <- table( treatments )
	xt <- xt*.scm(treatment.ratios)/treatment.ratios[names(xt)]
	diff(range(xt))
}

# Concatenate two factors to get a new factor.
.c.fac <- function( ... ){
	unlist( list( ... ) )
}

# The greatest common divisor of a vector of integer numbers.
.gcd <- function( x ){
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

# Parameter check for treatment ratios:
# All positive integers.
.check.treatment.ratios <- function(x){
	if( !all.equal( unname(x), as.integer(x) ) ){
		stop("Please use integers to specify treatment ratios! (E.g., c(A=2,B=1) instead of c(A=1,B=0.5))")
	}
	if( any( x <= 0 ) ){
		stop("Each treatment ratio must be positive!")
	}
	if( !all.equal( names(x), sort(names(x)) ) ){
		stop("Give treatment options and labels in alphabetic order!")
	}
}

# Parameter check for the actual treatments.
.check.treatments <- function(x){
	if( !is.factor(x) ){
		stop("treatments must be a factor!")
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
#' @param previous.treatments a factor containing the previous treatments.
#' This factor should contain
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
#' next.tr <- assign.next.treatment( d, tr, c(A=2,B=1) )
#'
#' @export
assign.next.treatment <- function(
	patient.data, previous.treatments, treatment.ratios=NULL, p=0.3 ){
	# Process and check input.
	if( !is.factor(previous.treatments) ){
		stop("previous.treatments must be a factor with >= 2 levels!")
	}
	if( nlevels(previous.treatments) < 2 ){
		stop("previous.treatments must be a factor with >= 2 levels!")
	}
	if( length(previous.treatments) != nrow(patient.data)-1 ){
		stop("The number of treatments must equal the number of patients - 1!")
	}
	.check.treatment.ratios( treatment.ratios )

	treatments <- as.factor(levels(previous.treatments))
	N <- length(treatments)

	if( is.null( treatment.ratios ) ){
		treatment.ratios <- setNames(rep(1,N),levels(previous.treatments))
	}

	# Compute the total imbalances for each treatment.
	factors <- names(patient.data)
	imbalances <- sapply( treatments, function(tr){ 
		sum( sapply( factors, function(f){
			f.values <- patient.data[[f]]
			imbalance(.c.fac(previous.treatments,tr)[f.values==tail(f.values,1)],
				treatment.ratios)
		} ) ) } )

	if( runif(1) > p ){
		# Sort treatments in increasing order by imbalance, randomly breaking ties.
		treatments <- treatments[order(imbalances, sample(N))]
		# Return treatment with smallest imbalance.
		r <- treatments[1]
	} else {
		# Pick treatment at random, using biased coin to attain desired
		# treatment ratio.
		#print( treatments )
		#print( treatment.ratios[(treatments)] )
		#print( rep(treatments,treatment.ratios[(treatments)]) )

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

