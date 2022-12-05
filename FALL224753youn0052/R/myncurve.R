#' Plots a dnorm function given mu and sigma, then
#' calculates the probability of P(Y<=A), and displays it
#'
#' @param mu E(x) for dnorm
#' @param sigma sd for dnorm
#' @param a the x value to calculate the area until
#'
#' @importFrom stats dnorm pnorm
#'
#' @example
#' \dontrun{myncurve(10, 5, 6)}
#'
#' @export
myncurve = function(mu, sigma, a){
  x = NULL
  curve(dnorm(x = x, mu, sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        ylab = "dnorm density", main = paste("dnorm(x, ", mu, ", ", sigma, ")", sep = ""))

  xcurve = seq(mu - 4 * sigma, a, length = 1000)
  ycurve = dnorm(xcurve, mu, sigma)

  polygon(c(mu - 4 * sigma, xcurve, a), c(0, ycurve, 0), col = "Pink")

  prob = pnorm(a, mu, sigma)
  prob = round(prob, 3)

  text(mu / 2, mean(ycurve), paste("Area = ", prob, sep = ""))
}
