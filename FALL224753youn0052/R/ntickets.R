#' calculates the number of tickets (n) to be sold on a given flight,
#' then prints a named list containing nd (n-discrete),
#' nc (n-continuous), N, p and gamma (all below) -
#' then creates a plot of the objective function Vs n.
#'
#' @param N number of seats on the flight
#' @param gamma probability plane will be overbooked
#' @param p probability someone will show up for the flight.
#'
#' @importFrom stats pbinom pnorm
#' @importFrom graphics par
#'
#' @example
#' \dontrun{ntickets(N = 400, gamma = 0.02, p = 0.95)}
#'
#' @export
ntickets = function(N, gamma, p) {
  #binomal sequence, and pbinom calc
  n_binom = seq(N, floor(1.15 * N - 10), by = 1)
  binom_temp = 1 - gamma - pbinom(N, n_binom, p)
  #put in data frame and retrieve little n
  df_binom = data.frame(n_binom, binom_temp)
  mindex_binom = which.min(abs(df_binom$binom_temp))

  #normal approximation sequence, and pnorm calc
  n_norm = seq(N, floor(1.15 * N - 10), by = 0.001)
  norm_approx = 1 - gamma - pnorm(N + 0.5, n_norm * p, sqrt(n_norm * p * (1 - p)))
  #put in data frame and retrieve little n
  df_norm = data.frame(n_norm, norm_approx)
  mindex_norm = which.min(abs(df_norm$norm_approx))

  par(mar = c(1, 1, 1, 1))
  #graphing
  layout(matrix(c(1, 2), nrow = 2))


  plot(x = df_binom$n_binom,
       y = df_binom$binom_temp,
       type = 'b',
       ylab = "Objective",
       xlab = "n",
       main = paste("Optimal tickets sold (",
                    df_binom[mindex_binom,]$n_binom,
                    ") when N = ", N,
                    ",\n gamma = ", gamma,
                    ", and p = ", p, " (discrete)", sep = ""),
       col = "BlueViolet")
  points(x = df_binom[mindex_binom,]$n_binom, y = 0, pch = 21, bg = "CadetBlue4", cex = 1)
  abline(h = 0, col = "CadetBlue4")
  abline(v = df_binom[mindex_binom,]$n_binom, col = "CadetBlue4")

  plot(x = df_norm$n_norm,
       y = df_norm$norm_approx,
       type = 'l',
       ylab = "Objective",
       xlab = "n",
       main = paste("Optimal tickets sold (",
                    df_norm[mindex_norm,]$n_norm,
                    ") when N = ", N,
                    ",\n gamma = ", gamma,
                    ", and p = ", p, " (continuous approx.)", sep = ""),
       col = "BlueViolet")
  points(x = df_norm[mindex_norm,]$n_norm, y = 0, pch = 21, bg = "CadetBlue4", cex = 1)
  abline(h = 0, col = "CadetBlue4")
  abline(v = df_norm[mindex_norm,]$n_norm, col = "CadetBlue4")

  #return list
  returnable = list(nd = df_binom[mindex_binom,]$n_binom,
                    nc = df_norm[mindex_norm,]$n_norm,
                    N = N,
                    p = p,
                    gamma = gamma)
  return(returnable)
}
