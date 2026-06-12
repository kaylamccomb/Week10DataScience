#' create_clusters()
#'
#' @param onsets List of onset time stamps from temporal data.
#'
#' @returns Histogram of IEIs. Cut-off point for determining which events get clustered.
#' @export
#'
#' @examples
#' create_clusters(c(0,1,5,7,9,24))
create_clusters <- function(onsets){
  #collect list of inter-event intervals
  onsets <- sort(na.omit(onsets))

  if (length(onsets) < 3) {
    return(NA_real_)
  }

  IEI <- diff(onsets)

  IEI <- IEI[IEI > 0]

  if (length(IEI) < 2) {
    return(NA_real_)
  }

  IEI <- data.frame(IEI)

  #create histogram of IEIs

  cutoff_70 <- quantile(IEI, probs = 0.70, na.rm = TRUE)

  IEI_histogram <- ggplot2::ggplot(IEI, ggplot2::aes(x = IEI)) +
    ggplot2::geom_histogram(fill = "grey70", color = "white") +
    ggplot2::geom_vline(xintercept = cutoff_70,
               color = "red4",
               linewidth = 1) +
    ggplot2::annotate(
      "text",
      x = cutoff_70,
      y = Inf,
      label = paste0("70/30 split"),
      color = "red4",
      vjust = 1.5,
      hjust = -0.1
    )

  print(IEI_histogram)
  print(paste0("Cluster events together with IEIs less than ", round(cutoff_70,3)))

}
