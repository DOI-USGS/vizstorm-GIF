fetch_precip_bins <- function(precip_config){
  # read in the config info
  breaks_chr <- c("0", as.character(precip_config$precip_breaks), "100000")
  breaks_num <- as.numeric(breaks_chr)
  colors <- precip_config$precip_cols
  transparent <- paste0(substr(colors[1], 1, 7), "00") # make a transparent color
  stopifnot(length(breaks_chr) == length(colors) + 2)

  # create a table of bins, colors, and bin labels. this should be the final
  # version here; prep_precip_fun and prep_legend_fun should just use what's
  # here as much as possible rather than modifying the colors, labels, etc.
  data_frame(
    left_break = breaks_num[-length(breaks_num)], # the bin on the left won't get a color
    left_break_chr = breaks_chr[-length(breaks_chr)], # the bin on the left won't get a color
    right_break = breaks_num[-1], # the last bin starts at the highest break and goes to ~Inf (but Inf itself gives an error in raster plotting)
    right_break_chr = breaks_chr[-1], # the last bin starts at the highest break and goes to ~Inf (but Inf itself gives an error in raster plotting)
    col = c(transparent, colors), # first bin is transparent
    label = ifelse(
      left_break == breaks_num[1],
      NA, # precip in the first bin is transparent and gets no legend entry
      ifelse(
        right_break < tail(breaks_num, 1),
        sprintf("%s-%s", left_break_chr, right_break_chr),
        sprintf("%s+", left_break)))) %>%
    select(-left_break_chr, -right_break_chr)
}
