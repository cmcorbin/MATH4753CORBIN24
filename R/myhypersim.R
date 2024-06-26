#' @title Hyper Geometric Simulator
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve hist layout mtext par polygon segments text
#' @importFrom stats dnorm dpois pnorm quantile rpois
#' @param iter The number of times the simulation iterates
#' @param N The total number of items
#' @param r The amount of whites(1's)
#' @param n The number of times a "marble is drawn"
#'
#' @return A barplot showcasing the probability of different number of successes based off iter number of iterations where there are N total marbles, r white marbles, and n marbles drawn
#' @export
#'
#' @examples
#' \dontrun{myhyper(iter=1000,n=19, N=20,r=12)}
myhyper=function(iter=100,N=20,r=12,n=5){
# make a matrix to hold the samples
#initially filled with NA's
sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
#Make a vector to hold the number of successes over the trials
succ=c()
for( i in 1:iter){
#Fill each column with a new sample
sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
#Calculate a statistic from the sample (this case it is the sum)
succ[i]=sum(sam.mat[,i])
}
#Make a table of successes
succ.tab=table(factor(succ,levels=0:n))
#Make a barplot of the proportions
barplot(succ.tab/(iter), col=rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
succ.tab/iter
}
