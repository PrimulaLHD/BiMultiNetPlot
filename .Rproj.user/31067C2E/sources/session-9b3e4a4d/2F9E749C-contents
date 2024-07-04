
#' Matrix to data frame
#'
#' @param network a matrix
#'
#' @return a data frame
#' @export
#'
#' @examples
#' net <- matrix(c(5,3,5,3,1,1,
#'                 3,3,3,3,0,0,
#'                 3,0,0,0,1,0,
#'                 0,0,3,1,0,0), byrow = TRUE, nrow = 4, ncol = 6,
#'                dimnames = list(paste('P',1:4,sep=''),paste('A',1:6,sep='')))
#' mat2df(net)
#'
mat2df <- function(network){
  df <- as.data.frame.table(network)
  names(df)[1:2] <- c("Var1", "Var2")
  df
}
