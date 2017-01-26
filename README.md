# trialMinRand

This is a barebone, no-frills R package that implements
a minimization randomization algorithm for clinical trials.
The package implements minimization as described in:

Pocock, Stuart J.; Simon, Richard:
__Sequential Treatment Assignment with Balancing for Prognostic Factors in the Controlled Clinical Trial.__
_Biometrics_, __31__(1):103-115, 1975. https://dx.doi.org/10.2307/2529712


## Example

This is a basic example which shows you how to solve a common problem:

```R
# Define patient data
d <- data.frame( F1=factor(c(1,2,2),levels=1:2),
  F2=factor(c(1,1,2),levels=1:2) )

# Define previous treatments
tr <- factor( c("A","A"), levels=c("A","B") )

# Call function to obtain next treatment. With probability 
# 0.7, this is going to be a treatment that minimizes imbalance.
# With probability 0.3, it is going to be a random treatment.
# Both imbalance and the distribution of random treatments are 
# defined using a 2:1 ratio for the treatments A and B.
next.tr <- assign.next.treatment( d, tr, c(A=2,B=1), p=0.3 )
```
