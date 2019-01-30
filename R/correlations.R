#' Compute Nonlinear Correlation
#'
#' Compute nonlinear correlation using spatial sampling. Local linear
#' correlations are computed at the samples and combined.
#' @param x A numeric vector. NAs are not allowed. The length of the vector should be more than 10.
#' @param y A numeric vector. NAs are not allowed. Length should be same as x.
#' @param refine Optional. Default value 0.5. Increase the value to increase
#' the granularity of local correlation computation.
#' @param plt Optional. Default value FALSE. Set TRUE to return ggplot2 object
#' for the data correlation visualization.
#' @return The output is a list containing the nonlinear correlation \code{cor.estimate}, \code{adjusted.p.value}, and \code{cor.plot}. \code{cor.estimate}
#' is between 0 and 1 (a negative nonlinear correlation is undefined). The
#' \code{adjusted.p.value} shows the statistical significance of the
#' \code{cor.estimate}. If \code{adjusted.p.value > 0.05}, the nonlinear
#' correlation estimate can be considered as noise (statistically not
#' significant). If \code{plt = T}, \code{ggplot2} object is return for
#' plotting the identified local linear correlations in the data.
#' @keywords nonlinear correlation
#' @seealso \code{\link{cor}}
#' @export
#' @examples
#' library(nlcor)
#' plot(x1, y1)
#' c <- nlcor(x1, y1)
#' c
#' c <- nlcor(x2, y2, plt = T)
#' print(c$cor.plot)
#' c <- nlcor(x3, y3, refine = 0.9, plt = T)
#' c$cor.estimate
#' c$adjusted.p.value
#' print(c$cor.plot)
#'
nlcor <- function(x, y, refine = 0.5, plt = F) {

  if(refine >= 1.0) {
    stop("Value of refine cannot be >= 1.0.")
  }

  maxCor <- 0
  adjusted.p.value <- NA
  segment.cor <- NULL
  best.s <- NULL

  s.size <- FindSegmentSize(l = length(x), refine = refine) # * refine

  for(s in seq(s.size, 1, s.size * (1 - refine))) {
    sampleCor <- SampleCor(x, y, s)
    netCor <- NetCor(cors = sampleCor$cor, pvalues = sampleCor$pvalue)

    if(netCor$cor.estimate > maxCor) {
      maxCor <- netCor$cor.estimate
      adjusted.p.value <- netCor$adjusted.p.value
      segment.cor <- netCor$segment.cor
      best.s <- s
    }
  }

  if(plt) {
    cor.plot <- PlotNlcor(x = x,
                          y = y,
                          segment.cor = segment.cor,
                          s = best.s)
    return(list(cor.estimate = maxCor,
                adjusted.p.value = adjusted.p.value,
                cor.plot = cor.plot
               )
            )
  } else {
    return(list(cor.estimate = maxCor,
                adjusted.p.value = adjusted.p.value
                )
            )
  }
}


#' Correlation from spatial sampling.
#'
#' Compute nonlinear correlation from local linear correlations
#' at some spatial sampling
#' @param x A numeric vector. NAs are not allowed.
#' @param y A numeric vector. NAs are not allowed. Length should be same as x.
#' @param s The sample size as percent of the vector length. A float number between 0 and 1.
#' @return \code{list(cor, pvalue)} containing the correlations and its pvalue for each segment.
#' @keywords nonlinear correlation, sample
#' @examples
#' SampleCor(x, y, s = 0.2)
#'
SampleCor <- function(x, y, s) {

  df <- data.frame(x, y)
  df <- df[order(x), ] # We sort x to sample it spatially.

  l <- length(x)

  segments <- Segment(l = l, s = s)

  out <- list(cor = NULL,
              pvalue = NULL)

  for(seg in segments) {
    segcor <- cor.test(df$x[seg], df$y[seg])
    out$cor <- c(out$cor, segcor$estimate[[1]])
    out$pvalue <- c(out$pvalue, segcor$p.value)
  }
  return(out)
}


#' Find net correlation from the "linear" segments.
#'
#' Find net correlation from the "linear" segments.
#' @param cors A vector correlations.
#' @param pvalues A vector of pvalues corresponding to the correlations in the \code{cors} vector.
#' @param p.threshold The overall threshold of p value, also known as the significance level.
#' @return The net correlation estimate, \code{cor.estimate}, and a list containing
#' the adjusted correlations and pvalues for each "linear" segment.
#' @examples
#' cors <- c(-0.70, 0.93, -0.79, 0.91)
#' pvalues <- c(0.004, 0.0006, 0.0007, 0.009)
#' NetCor(cors, pvalues)
#'
NetCor <- function(cors, pvalues, p.threshold = 0.05) {
  adjusted.p.threshold <- p.threshold / length(cors)

  for(i in 1:length(cors)) {
    if(pvalues[i] > adjusted.p.threshold || is.na(pvalues[i])) {
      pvalues[i] <- NA
      cors[i] <- 0
    }
  }

  return(list(cor.estimate = mean(abs(cors)),
              adjusted.p.value = 1- prod((1 - pvalues), na.rm = T),  # Approximation of p-value assuming segment independence
              segment.cor = list(cor = cors,
                                 p.value = pvalues)))
}
