generate_data = function(n, p) {
  cov = rnorm(n*p)
  cov.mat = matrix(cov, nrow = n, ncol = p)
  vec = rnorm(n)
  result = list(cov.mat, vec)
  names(result) = c("covariates", "responses")
  return(result)
}

model_select = function(covariates, responses, cutoff) {
  result = lm(responses ~ covariates)
  pvals = summary(result)$coefficients[,4]
  pvals.cutoff.index = which(pvals <= cutoff) - 1
  if(length(pvals.cutoff.index) > 0) {
    final.result = lm(responses ~ covariates[, pvals.cutoff.index])
    final.pvals = summary(final.result)$coefficients[,4][-1]
  }
  else final.pvals = vector()
  return(final.pvals)
}

