#' @title sig_heatmap
#' @description create significant heatmap
#' @param all_patients_result result table
#' @param title title
#' @return pheatmap
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sig_heatmap
#' @export
#'
sig_heatmap <- function(all_patients_result, title) {
  my_fun <- function(p) {
    asterisks_vec = p
    asterisks_vec[p<=0.05 & p>0.01] = "*"
    asterisks_vec[p<=0.01 & p > 0.001] = "**"
    asterisks_vec[p<=0.001 & p >= 0] = "***"
    asterisks_vec[p>0.05] = ""
    paste(asterisks_vec)
  }
  asterisks = all_patients_result
  asterisks[] <- lapply(all_patients_result, my_fun) #replace significant values with asterisks
  print("this is the new version 2")


  all_patients_result = -log10(all_patients_result)
  paletteFunc <- colorRampPalette(c("white","navy"));

  palette <- paletteFunc(100)

  print(
    pheatmap(all_patients_result,
             cluster_rows = T,
             cluster_cols = T,
             show_rownames = TRUE,
             color = palette,
             # breaks = seq(0,20,0.2),
             number_color = "grey30",
             main = title,
             display_numbers = asterisks,
             fontsize_row = 8,
             # legend_breaks =breaks,
             # legend_labels = breaks_labels
    )
  )

}
