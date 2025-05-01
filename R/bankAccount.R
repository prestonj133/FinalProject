
#' Create a new bank account
#'
#' @param name The account name
#' @param balance Initial balance with a default of 0
#' @return A bankAccount object
#' @export
create_account <- function(name, balance) {
  if (balance < 0) stop("Must have initial balance greater than 0")
  account <- list(
    name = name,
    balance = balance,
  )
  class(account) <- "bankAccount"
  return(account)
}

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

#' Check balance
#' @param account Your bank account
#' @return Current balance
#' @export
check_balance <- function(account) {
  return(account$balance)
}

#' Add interest
#' @param account Your bank account
#' @param rate Interest rate in decimal format
#' @return Updated account
#' @export
add_interest <- function(account, rate) {
  if (rate < 0) stop("Interest rate cannot be negative")
  interest <- account$balance * rate
  account$balance <- account$balance + interest
  return(account)
}

#' Transfer money
#' @param from_account Account where money is coming from
#' @param to_account Account where the money is going to
#' @param amount Amount of money to be transferred
#' @return Updated accounts
#' @export
transfer <- function(from_account, to_account, amount) {
  from_account <- withdraw(from_account, amount)
  to_account <- deposit(to_account, amount)
  return(list(from_account = from_account, to_account = to_account))
}

