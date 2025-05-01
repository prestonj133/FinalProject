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
    balance = balance
  )
  class(account) <- "bankAccount"
  return(account)
}

devtools::document()
