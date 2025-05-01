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