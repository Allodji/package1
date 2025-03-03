
#' Moyenne d'une variable
#'
#' @param x a vector
#'
#' @returns a number
#' @export
#'
#' @examples
#' moyenne(iris$Sepal.Length)
#' 
moyenne<-function(x){
  sum(x, na.rm = TRUE)/length(x)
}

#' Variance
#'
#' @param x vector
#'
#' @returns a number
#' @export
#'
#'
#' @examples
#' variance(iris$Sepal.Length)
#' 
variance<-function(x){
   (sum(x**2)/(length(x))-(moyenne(x))**2)
}