generate_data = function(n, p) {
  cov = rnorm(n*p)
  cov.mat = matrix(cov, nrow = n, ncol = p)
  vec = rnorm(n)
  result = list(cov.mat, vec)
  names(result) = c("covariates", "responses")
  return(result)
}

generate_data(10, 10)

