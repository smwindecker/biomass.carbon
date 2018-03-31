library(plyr)


t_3 <- read.table('munge/mean_traits.txt')



# biomass trait correlations
png('output/correlations.png', width = 760, height = 860)
par(mfrow = c(4, 3),     # 3x4 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(2, 2, 1, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row

corplot <- function (xval, yval) {
  plot(xval, yval, xlab = '', ylab = '')
  mylm <- lm(yval ~ xval)
  abline(mylm, col = 'black')
  newx <- seq(min(xval), max(xval), length.out = 500)
  prd <- predict(mylm, newdata = data.frame(xval = newx), interval = c("confidence"),
                 level = 0.90, type="response")
  lines(newx, prd[, 2], col = 'black', lty = 2)
  lines(newx, prd[, 3], col = 'black', lty = 2)
}

corplot(cov$P.HC, cov$SLA)
title(ylab = 'Specific Leaf Area (m2/g)', xpd = NA)
corplot(cov$P.CL, cov$SLA)
corplot(cov$P.LG, cov$SLA)

corplot(cov$P.HC, cov$LDMC)
title(ylab = 'Leaf Dry Matter Content (mg/g)', xpd = NA)
corplot(cov$P.CL, cov$LDMC)
corplot(cov$P.LG, cov$LDMC)

corplot(cov$P.HC, cov$LNC)
title(ylab = 'Leaf Nitrogen Content (mg/g)', xpd = NA)
corplot(cov$P.CL, cov$LNC)
corplot(cov$P.LG, cov$LNC)

corplot(cov$P.HC, cov$LCC)
title(xlab = 'Pseudo Hemicellulose (mg/g)', xpd = NA)
title(ylab = 'Leaf Carbon Content (mg/g)', xpd = NA)
corplot(cov$P.CL, cov$LCC)
title(xlab = 'Pseudo Cellulose (mg/g)', xpd = NA)
corplot(cov$P.LG, cov$LCC)
title(xlab = 'Pseudo Lignin (mg/g)', xpd = NA)

dev.off()



