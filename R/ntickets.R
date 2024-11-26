#' Project 1: Plane Sales
#'
#' Function created by SNT for Project 1 MATH 4753
#'
#' @param N = total number of seat on plane
#' @param gamma = acceptable probbility of overselling
#' @param p = probability passenger arrives
#'
#' @return This function returns the listed parameters: nd, nc, N, gamma, p and generates a discrete binomial plot, continuous normal plot
#' @importFrom graphics abline barplot curve points polygon text
#' @importFrom grDevices rainbow
#' @importFrom stats dnorm pbinom pchisq pnorm qbinom qnorm
#' @export
#'
#' @examples
#' ntickets(400,.02,.95)
ntickets <- function(N,gamma,p){
  # N = Number of seats on plane
  # gamma = acceptable probability of overbooking
  # p = probability that passenger arrives to fly
  # n = number of passengers who won't arrive/number of tickets to oversell by
  # nd = total ticket sales, discrete binomial calculation
  # nc = total ticket sales, continuous normal calculation
  #Task1:Number of Tickets Discrete
  # p = gamma
  # size = N
  # prob = p
  # quantile function = n = number of passengers who won't arrive/additional passengers
  # solves for probable number of passengers who don't arrive and adds to total seats to oversell by that amount
  nd = N + qbinom(gamma,N,1-p)
  #Task2: Number of Tickets Normal
  # gamma = p
  qn = p
  pn = 1-p
  mean = N*pn
  sd = sqrt(N*qn*pn)
  nc = N + qnorm(gamma,mean, sd)
  #Task3: Named List
  nticketslist = list("nd:tickets to sell, discrete" = nd, "nc:tickets to sell, continuous" = nc, "N:seats available on plane" = N, "p:probability passegners arrives to fly" = p, "gamma:acceptable oversell probability" = gamma)
  #Task4: Plots
  OBJD = function(x){pbinom(x-N,N,1-p)-gamma}
  labeld = nd
  displot <- plot(OBJD,type = "b", xlim = c(N-(N*.05),N+(N*.1)), ylim = round(c(0,1),1), main = "Objective v n: Optimal Tickets Sold, discrete", xlab = "n", ylab = "Objective" )
  abline(h=0,col="red")
  abline(v=nd,col="red")
  points(nd,0,col="black")
  text(nd,0,labeld,pos =3)
  OBJN = function(x){pnorm(x-N, mean, sd)-gamma}
  labelc = nc
  contplot <- curve(OBJN, xlim = c(N-(N*.05),N+(N*.1)), ylim = c(0,1), main = "Objective v n: Optimal Tickets Sold, continuous", xlab = "n", ylab = "Objective")
  abline(h=0,col="blue")
  abline(v=nc, col = "blue")
  points(nc,0,col="black")
  text(nc,0,labelc,pos =3)
  #RETURN
  return(nticketslist)
  return(displot)
  return(contplot)
}
