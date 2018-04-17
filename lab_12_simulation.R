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
  pvals = summary(result)$coefficients[,4][-1]
  pvals.cutoff.index = which(pvals <= cutoff)
  
  if(length(pvals.cutoff.index) > 0) {
    final.result = lm(responses ~ covariates[, pvals.cutoff.index])
    final.pvals = summary(final.result)$coefficients[,4][-1]
  }
  else final.pvals = vector()
  
  return(final.pvals)
}

run_simulation = function(n_trials, n, p, cutoff) {
  all.pvals = vector()
  
  for(i in 1:n_trials) {
    for(j in 1:length(n)) {
      for(k in 1:length(p)) {
        data = generate_data(n[j], p[k])
        pvals = model_select(data$covariates, data$responses, cutoff)
        all.pvals = c(all.pvals, pvals)
      }
    }
  }
  
  hist(all.pvals, main = "P-values", ylab = "Frequency", xlab = "P-value")
}