#' Seasonal adjustment
#'
#' Makes seasonally adjusted column from [period, value] dataframe.
#' @name make_seasonal
#' @param val Value column
#' @param period Period column
#' @param imputed_values Return imputed values
#' @keywords hud clean seasonal seasonality
#' @importFrom zoo read.zoo as.yearmon
#' @importFrom seasonal seas
#' @export
#' @examples
#' df %>% mutate(value_sadj = make_seasonal(value, period))
make_seasonal <- function(val, period, imputed_values = FALSE) {
  if (any(period != sort(period)) {
    message("This function returns seasonally adjusted values sorted by date, so if the table receiving them has unsorted dates, the seasonal values will not match original values.")
    stop("Input dates are not sorted!")
  }

  raw_ts <-
    data.frame(period, val) %>%
    zoo::read.zoo(FUN = zoo::as.yearmon) %>%
    as.ts()

  # Impute missing data for the seasonal calculation
  s_res <-
    raw_ts %>%
    zoo::na.StructTS() %>%
    seasonal::seas()

  if (imputed_values) {
    s_res$data[, "seasonaladj"]
  } else {
    s_res$data[!is.na(raw_ts), "seasonaladj"]
  }
}
