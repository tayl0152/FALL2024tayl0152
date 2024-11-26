#' MyNCurve
#'
#' Function developed by SNT for Lab 6 MATH 4753
#'
#' @param mu - mean of normal distribution
#' @param sigma - sd of normal distribution
#' @param a - Y ~ N(Y less than a)
#'
#' @return This function returns a plot showing probability of Y ~ N(Y less than a), and calculated probability
#' @importFrom rlang .data
#' @export
#'
#' @examples
#'\dontrun{myncurve(1,1,2)}
myncurve <- function(mu, sigma,a){
  curve(dnorm(.data$x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mu,sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="red")
  prob=pnorm(a,mu,sigma)-pchisq(-Inf,mu, sigma)
  prob=round(prob,4)
  prob
}


