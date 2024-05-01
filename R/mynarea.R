#' @@title mynarea
#'
#' @param a the right bound
#' @param mu mu(mean)
#' @param sigma sigma(sd)
#'
#' @return A Plot of a curve and a shaded section between -inf to a with the calculated area between returned
#' @export
#'
#' @examples
#' \dontrun{mynarea(4, 10, 3)}
mynarea = function(a, mu, sigma){
  curve(dnorm(a,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +3*sigma))
  list(mu = mu, sigma = sigma)
  xcurve=seq(-Inf, a, length=1000)
  ycurve=dnorm(xcurve, mu, sigma)
  polygon(x=c(-Inf,xcurve,a), y =c(0,ycurve,0),col="Red")
  prob=round(pnorm(a,mu,sigma) - pnorm(-Inf,mu,sigma), 4)
  list(area = prob)
}
