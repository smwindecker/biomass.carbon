# unused figure code

n.pca <- prcomp(cov, scale. = TRUE)
# PCA1
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
png("output/pca_by_gf.png", units="in", width=5, height=4, res=200)
g <- ggbiplot(n.pca, obs.scale = 1, var.scale = 1,
              groups = t_1$gf, ellipse = TRUE,
              circle = TRUE) +
  scale_color_discrete(name = '')+
  theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
dev.off()

## sparse PCA
library(nsprcomp)
library(elasticnet)

sparse.pca <- spca(cov, K = 2, type = 'predictor', para = c(3,3))
nscumcomp(cov, ncomp = 2, k = 7, gamma = 1e3, scale. = TRUE)


plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, ylab="", xlab="",
     cex.axis=0.75, ylim=c(-6, 5), xlim=c(-5, 5),las=1)
plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
