## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(nlcor)
library(ggplot2)
library(reshape2)

## ---- Figure-1.1, fig.show='hold', fig.align = 'center', fig.height = 4, fig.width = 6----
plot(x1, y1)

## ------------------------------------------------------------------------
cor(x1, y1)

## ---- Figure-1.2, fig.show='hold', fig.align = 'center', fig.height = 4, fig.width = 6----
c <- nlcor(x1, y1, plt = T)
c$cor.estimate
c$adjusted.p.value
print(c$cor.plot)

## ---- Figure-2.1, fig.show='hold', fig.align = 'center', fig.height = 4, fig.width = 6----
plot(x2, y2)

## ------------------------------------------------------------------------
cor(x2, y2)

## ---- Figure-2.2, fig.show='hold', fig.align = 'center', fig.height = 4, fig.width = 6----
c <- nlcor(x2, y2, plt = T)
c$cor.estimate
c$adjusted.p.value
print(c$cor.plot)

## ---- Figure-3.1, fig.show='hold', fig.align = 'center', fig.height = 4, fig.width = 6----
plot(x3, y3)

## ------------------------------------------------------------------------
cor(x3, y3)

## ---- Figure-3.2, fig.show='hold', fig.align = 'center', fig.height = 4, fig.width = 6----
c <- nlcor(x3, y3, plt = T)
c$cor.estimate
c$adjusted.p.value
print(c$cor.plot)

## ---- Figure-3.3, fig.show='hold', fig.align = 'center', fig.height = 4, fig.width = 6----
c <- nlcor(x3, y3, refine = 0.999, plt = T)
c$cor.estimate
c$adjusted.p.value
print(c$cor.plot)

## ---- Figure-3.4, fig.show='hold', fig.align = 'center', fig.height = 4, fig.width = 6----
c <- nlcor(x1, y1, plt = T, line_thickness = 2.5, line_opacity = 0.8)
print(c$cor.plot)

