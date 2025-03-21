#' Estimates the regression coefficients in the tau-truncated Poisson regression model
#' @description
#'  \code{ptruncReg} : this function fits the tau-truncated 
#'  unicomponent Poisson regression model and estimates the
#'   regression coefficients. Note that the tau-truncated Poisson 
#'   regression model is used to analyze count data that are truncated at a known upper limit, denoted as tau.
#'
#' @param y The vector of dependent (or response) variable
#' @param X a data frame or matrix containing the explanatory variables for the regression
#' @param tau the right-truncation threshold
#' @param tol the tolerance
#' @param add_intercept a Boolean that specifies if an intercept should be added to the model or not
#'
#' @details
#'  The package implements a unicomponent Poisson regression model, truncated 
#'  at a known upper limit tau. The estimation is performed by maximizing 
#'  the likelihood function. This model is particularly useful for analyzing 
#'  count data with a ceiling effect.
#'
#' @note
#' It is important to ensure that the count data and truncation values are correctly 
#' specified. The model assumes that the data follow a tau-truncated unicomponent 
#' Poisson distribution.
#'
#' @returns
#' \code{$coeff }   : a matrix containing the maximum likelihood estimates of the regression coefficients
#'
#' \code{$var_cov}  : The matrix of variance covariance
#'
#' \code{$AIC}    : the value of the Akaike criterion
#'
#' \code{$BIC}      : the value of the BIC criterion
#'
#' \code{$n_iter}   : The number of iterations the algorithm runs until convergence
#'
#'
#' @import stats
#'
#' @export
#'
#' @examples
#'
#'  library(Ex1)
#' # On éxécute la commande suivante  pour faire la régression
#'  ptruncReg(data[,1], data[,-1], tau=8, tol=1e-8, add_intercept = TRUE)
#'
ptruncReg = function(y, X, tau, tol = 1e-8, add_intercept = TRUE)
{
  X = model.matrix( ~ ., data = as.data.frame(X))
  
  if (add_intercept == FALSE)
  {
    X = X[, -1]
  }
  
  
  start = coefficients(glm(y ~ . - 1, data = as.data.frame(X), family = poisson))
  
  GrHess = function(b)
  {
    N  = rep(0, nrow(X))
    D  = N
    NN = N
    for (k in 1:(tau + 1))
    {
      N = N + ((k - 1) * exp((k - 1) * (X %*% b)) / factorial(k - 1))
      D = D + (exp((k - 1) * (X %*% b)) / factorial(k - 1))
      NN = NN + (((k - 1)^2) * exp((k - 1) * (X %*% b)) / factorial(k -
                                                                      1))
    }
    
    V = diag(as.vector((N / D)^2 - NN / D)) %*% X
    return(list(
      gr = apply(as.vector(y - N / D) * X, 2, sum),
      hess = t(X) %*% V
    ))
  }
  
  L = function(beta)
  {
    return(sum(log(dpois(
      y, exp(X %*% beta)
    ) / ppois(
      tau, exp(X %*% beta)
    ))))
  }
  
  NR_func = function(f, x0, tol)
  {
    iter = 1
    D    = f(x0)
    x1   = as.vector(x0 - (solve(D$hess)) %*% (D$gr))
    while (max(abs(x1 / x0 - 1)) >= tol)
    {
      x0   = x1
      D    = f(x0)
      x1   = as.vector(x0 - (solve(D$hess)) %*% (D$gr))
      iter = iter + 1
    }
    lik = L(x1)
    AIC = 2 * ncol(X) - 2 * lik
    BIC = ncol(X) * log(nrow(X)) - 2 * lik
    return(
      list(
        coeff = x1,
        gradient = D$gr,
        var_cov = -solve(D$hess),
        nbIter = iter,
        likehood = L(x1),
        AIC = AIC,
        BIC = BIC,
        int = add_intercept ,
        var_names = colnames(X)
      )
    )
  }
  
  return(NR_func(GrHess, start, tol))
}
