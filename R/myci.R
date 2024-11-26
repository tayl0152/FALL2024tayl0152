#' MyCI
#'
#' Function developed by SNT for Lab 11 MATH 4753
#'
#' @param x = sample
#' @param a = conf interval, as a decimal
#' @param n = sample size
#'
#' @return range of values
#' @importFrom graphics axis
#' @importFrom stats qt sd
#' @export
#'
#' @examples
#' \dontrun{myci(x,n=30)}
myci <- function(x,a=.95,n){
  t=qt(a+.5*(1-a),n-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(n)
  ci[2]=mean(x)+t*sd(x)/sqrt(n)
  ci
}
