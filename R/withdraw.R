#' Withdraw money
#' @param account Your bank account
#' @param amount Amount to withdraw
#' @return Updated account
#' @export
withdraw <- function(account, amount) {
  if (amount <= 0) stop("Withdrawal must be positive")
  if (account$balance < amount) stop("Withdrawl cannot exceed balance") 
  account$balance <- account$balance - amount
  return(account)
}