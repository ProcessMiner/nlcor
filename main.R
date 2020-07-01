install.packages("devtools")
library(devtools)
# install_github("ProcessMiner/nlcor")
devtools::load_all()

library(nlcor)
library(ggplot2)
plot(x, y)

# x <- seq(0,3*pi,length.out=100)
# y <- sin(x) + rnorm(n = 100, sd = 0.2)
# plot(x, y)
#
# simplecyclic <- data.frame(x = x, y = y)
#
# saveRDS(simplecyclic, 'data/simplecyclic.rda')

cor(x, y)
nonlinear.cor <- nlcor(x, y, plt = T)
nonlinear.cor

nonlinear.cor <- nlcor(x1, y1, plt = T)
print(nonlinear.cor$cor.plot)

nonlinear.cor <- nlcor(x2, y2, plt = T)
print(nonlinear.cor)

nonlinear.cor <- nlcor(x3, y3, plt = T)
print(nonlinear.cor)

# Refine argument
nonlinear.cor <- nlcor(x3, y3, refine = 0.9, plt = T)
print(nonlinear.cor)

# Correlation line arguments
nonlinear.cor <- nlcor(x1, y1, plt = T, line_thickness = 2.5, line_opacity = 0.8)
print(nonlinear.cor$cor.plot)
