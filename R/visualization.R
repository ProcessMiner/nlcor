PlotNlcor <- function(x, y, segment.cor, s) {
  df <- data.frame(x, y)
  df <- df[order(x), ] # We sort x to sample it spatially.

  df.fit <- data.frame()

  l <- length(x)

  segments <- Segment(l = l, s = s)
  seg <- segments[[1]]

  for(i in 2:length(segments)) {
    if(sign(segment.cor$cor[i - 1]) == sign(segment.cor$cor[i])) { # The correlation direction is same
      seg <- c(seg, segments[[i]])
    } else { # Direction changes.
      if(segment.cor$cor[i - 1] != 0) {
        fit <- lm(y ~ x, data = df[seg, c("x", "y")])

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
      fit <- lm(y ~ x, data = df[seg, c("x", "y")])  # The last seg above is valid here

      df.fit <- rbind(df.fit,
                      data.frame(x = df[seg, "x"],
                                 fit = fit$fitted))  # Removing one value for better disjoint line visuals
      df.fit[seg[length(seg)], "fit"] <- NA
    }
  } else {
    if(segment.cor$cor[last] != 0) {
      fit <- lm(y ~ x, data = df[segments[[last]], c("x", "y")])  # The last seg above is valid here
      df.fit <- rbind(df.fit,
                      data.frame(x = df[seg, "x"],
                                 fit = fit$fitted))
    }
  }

  p <- ggplot() +
    geom_point(data = df, aes(x = x, y = y)) +
    geom_line(data = df.fit, aes(x = x, y = fit, colour = "red")) +
    xlab("x") +
    ylab("y") +
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "#000000"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position = "none"
          )
  return(p)
}
