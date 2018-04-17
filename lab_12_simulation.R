generate_data = function(n, p) {
  cov = rnorm(n*p)
  cov.mat = matrix(cov, nrow = n, ncol = p)
  vec = rnorm(n)
  result = list(cov, vec)
  names(result) = c("covariates", "responses")
  return(result)
}

generate_data(10, 10)

