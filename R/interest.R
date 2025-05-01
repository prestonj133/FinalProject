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