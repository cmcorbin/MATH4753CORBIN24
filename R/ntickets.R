#' @title Number of Tickets Calculator
#'
#' @importFrom stats pbinom
#' @param N number of seats in plane
#' @param gamma probability of overbooking
#' @param p probability of show
#'
#' @return The number of seats a plane should sell without there being too many passengers to seat
#' @export
ntickets = function(N=400, gamma=0.02, p=0.95){

  upper = round(N*1.1, digits = 0) #upper bound for n(discrete so need to round)
  bounds <- seq(N,upper) #generate vector of n values
  disfun = 1 - gamma-pbinom(N,bounds, p) #calculate discrete function using given function
  index = which.min(abs(disfun)) #find the index of smallest disfun value
  nd = bounds[index] #find the correlated value, this is nd

  #dev.new(width=40, height=4) #allows for wider plot but rmd no like
  plot(bounds, disfun, type="o", pch=21, xlab="n", ylab="Objective", bg="Blue", main=paste0("Objective VS n: optimal tickets sold \n(",nd,
                                                                                            ") gamma = ",gamma, " N = ",N," discrete"))
  abline(v=nd,col="Red") #add vertical line at nd
  abline(h=0, col="Red") #add horizontal line at 0.0

  #Create function for continuous using the given formula on project
  confun = function(x){
    1-gamma-pnorm(N+0.5, x*p, sqrt(x*p*(1-p)))
  }

  #Use the recommended stats::uniroot function to find when confun=0
  ncuniroot = stats::uniroot(confun,interval= c(N,N*1.1))

  nc = ncuniroot$root #uniroot returns list we need $root

  #dev.new(width=40, height=4)
  curve(confun, xlim=c(N,N*1.1), type="l", pch=21, xlab="n", ylab="Objective", main=paste0("Objective VS n: optimal tickets sold \n(",nc,
                                                                                           ") gamma = ",gamma, " N = ",N," continous"))
  abline(v=nc,col="Blue") #add vertical line at nd
  abline(h=0, col="Blue") #add horizontal line at 0.0

  list(nd=nd, nc=nc, N=N, gamma=gamma, p=p) #return the requested value in a list
}
