#' Plotting the nonlinear correlation.
#'
#' Plot nonlinear correlation.
#' @param x A numeric vector. NAs are not allowed.
#' @param y A numeric vector. NAs are not allowed. Length should be same as x.
#' @param segment.cor A list Segment-wise correlation and associated p.value
#' @param s The sample size as percent of the vector length. A float number between 0 and 1.
#' @return ggplot plot object
#' @keywords plot
#' @export
#' @examples
#' library(ggplot2)
#' segment.cor <- list(cor = c(-0.77, 0.52, 0.91, 0.11, 0.43),
#'  p.value = c(0.00000012, 0.0002332, 0.0041, 0.01123, 0.52))
#' PlotNlcor(x, y, segment.cor = segment.cor, s = 0.2)
#'
PlotNlcor <- function(x, y, segment.cor, s) {
  df <- data.frame(x, y)
  df <- df[order(x), ] # We sort x to sample it spatially.

  df.fit <- data.frame()

  l <- length(x)

  segments <- Segment(l = l, s = s)
  seg <- segments[[1]]

  if(length(segments) > 1) {
    for(i in 2:length(segments)) {
      if(sign(segment.cor$cor[i - 1]) == sign(segment.cor$cor[i])) { # The correlation direction is same
        seg <- c(seg, segments[[i]])
      } else { # Direction changes.
        if(segment.cor$cor[i - 1] != 0) {
          fit <- stats::lm(y ~ x, data = df[seg, c("x", "y")])

          df.fit <- rbind(df.fit,
                          data.frame(x = df[seg, "x"],
                                     fit = fit$fitted))  # Adding an NA row for disjoint line plot
          df.fit[seg[length(seg)], "fit"] <- NA
          seg <- segments[[i]]  # Reset
        }
      }
    }

    last <- length(segments)
    if(sign(segment.cor$cor[last - 1]) == sign(segment.cor$cor[last])) {
      if(segment.cor$cor[last - 1] != 0) {
        fit <- stats::lm(y ~ x, data = df[seg, c("x", "y")])  # The last seg above is valid here

        df.fit <- rbind(df.fit,
                        data.frame(x = df[seg, "x"],
                                   fit = fit$fitted))  # Removing one value for better disjoint line visuals
        df.fit[seg[length(seg)], "fit"] <- NA
      }
    } else {
      if(segment.cor$cor[last] != 0) {
        fit <- stats::lm(y ~ x, data = df[segments[[last]], c("x", "y")])  # The last seg above is valid here
        df.fit <- rbind(df.fit,
                        data.frame(x = df[seg, "x"],
                                   fit = fit$fitted))
      }
    }
  } else if(length(segments) == 1) {
    fit <- stats::lm(y ~ x, data = df[seg, c("x", "y")])

    df.fit <- rbind(df.fit,
                    data.frame(x = df[seg, "x"],
                               fit = fit$fitted))  # Adding an NA row for disjoint line plot
    df.fit[seg[length(seg)], "fit"] <- NA
  }


  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(data = df.fit, ggplot2::aes(x = x, y = fit, colour = "red")) +
    ggplot2::xlab("x") +
    ggplot2::ylab("y") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(colour = "#000000"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(size = 12),
          axis.title = ggplot2::element_text(size = 12),
          legend.position = "none"
          )
  return(p)
}
