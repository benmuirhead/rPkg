#' CountNA
#'
#' This function accepts a data frame and counts the number of NAs
#' @param data A data frame
#' @export
#' @examples
#' countNA(insertNA(genProportions(ncol=10, nrow=10)  , .1))


countNA = function(data) {
  na_count <-
    sapply(data, function(y)
      sum(length(which(is.na(
        y
      )))))
  na_count <- data.frame(na_count)
  na_count$na_count_percent = round((100 * na_count$na_count) / lengths(data)[1], 2)
  na_count = na_count[order(na_count$na_count, decreasing = TRUE),]
  return(na_count)

  #na_missing = md.pattern(data)
  #na_missing

  # print(na_missing2)
}
