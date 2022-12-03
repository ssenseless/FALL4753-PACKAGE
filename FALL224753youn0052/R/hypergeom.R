#' Computes the an approximation
#' of the probability mass function
#' of the hyper geometric distribution.
#'
#' Try saying that 5 times fast.
#'
#' Primarily performs the approximation
#' more accurately given higher 'iterations' values.
#'
#' @param iterations the number of iterations to be performed to calculate averages
#' @param num_trials the number of trials (without substitution) to be performed
#' @param total_y the total successes
#' @param total_obj_num total number of objects in the set
#'
#' @importFrom graphics abline barplot curve hist layout points polygon segments text
#' @importFrom grDevices rainbow
#' @example
#' \dontrun{hypergeom(iterations = 10, num_trials = 5, total_y = 12, total_obj_num = 20)}
#'
#' @export
hypergeom = function(iterations, num_trials, total_y, total_obj_num) {
  #create vector in readable format.
  #ex. let total_y = 2, total_obj_num = 5,
  #prob_vect = c(1, 1, 0, 0, 0)
  prob_vect = c(rep(1, total_y), rep(0, total_obj_num - total_y))

  #make a matrix containing the samples, rows equal
  #to trials, columns equal to iterations, filled with NA
  sample_matrix = matrix(NA, nrow = num_trials, ncol = iterations, byrow = TRUE)

  #make a matrix that will contain the frequencies
  #in each sample, 2 rows (Y/N),
  #columns equal to iterations, filled with NA
  frequency_matrix = matrix(NA, nrow = 2, ncol = iterations, byrow = TRUE)

  for(i in 1:iterations) {
    #fill each column with samples
    sample_matrix[,i] = sample(prob_vect, size = num_trials)
    #collect frequencies
    frequency_matrix[,i] = table(factor(sample_matrix[,i], levels = 1))
  }
  #factor back out into usable data
  frequency_matrix_org = table(factor(frequency_matrix, levels = c(1:num_trials)))

  #normalize range s.t. values add to 1
  frequency_avg = frequency_matrix_org / sum(frequency_matrix_org)

  #plot
  barplot(frequency_avg, col = rainbow(num_trials))
  frequency_avg
}
