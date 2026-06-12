#' burstiness()
#'
#' @param onsets List of onset time stamps from temporal data.
#'
#' @returns A burstiness value B. Qualifier of bursty, periodic, or random.
#' @export
#'
#' @examples
#' burstiness(c(0,1,5,8,10))
burstiness <- function(onsets){
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

  #calculate mean
  mean <- mean(IEI)
  #calculate sd
  sd <- sd(IEI)
  #calculate burstiness metric
  B <- (sd - mean) / (sd + mean)

  print(B)

  ifelse(
    B < -0.33, "Periodic",
    ifelse(B > 0.33, "Bursty", "Random")
  )

}
