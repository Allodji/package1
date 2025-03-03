
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
