fetch_precip_bins <- function(precip_config){
  bin <- precip_config$precip_bin
  cols <- precip_config$precip_cols
  right_break <- c(seq(bin, by = bin, length.out = (length(cols) - 1)), Inf)
  left_break <- seq(0, by = bin, length.out = length(cols))
  
  left_break[1] <- 1
  
  data.frame(left_break  = left_break, right_break = right_break, col = cols) %>%
    mutate(break_factor = as.factor(sprintf("(%s,%s]", left_break, right_break)))
}
