#' Deposit money
#' @param account Your bank account
#' @param amount Amount to deposit to account
#' @return Updated account
#' @export
deposit <- function(account, amount) {
  if (amount <= 0) stop("Deposit must be positive")
  account$balance <- account$balance + amount
  return(account)
}