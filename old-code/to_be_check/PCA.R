
# PCA

n <- read.table('munge/inundated_data.txt')
n <- n[!n$species == 'Phragmites australis',]
# PCA all variables, then take them out and see how patterns change
# observe patterns then select groupings

# test on subset then validate on the rest?

# PCAs performed on log10 transformed traits
n$tSLA <- log(n$SLA)
n$tLDMC <- log(n$LDMC)
n$tCN <- log(n$CN)
covariates <- c('tSLA', 'tLDMC', 'tCN', 'pcomp_1', 'pcomp_2', 'pcomp_3')

# PCA 1

n.pca <- prcomp(cov, scale. = TRUE)
print(n.pca)
plot(n.pca, type = 'l')
summary(n.pca)

# VISUAL DISPLAY 1
biplot(n.pca)



# PCA WITH TRAINING
# split data into 2 parts for pca training (75%) and prediction (25%)
set.seed(1)
samp <- sample(nrow(n), nrow(n)*0.75)
n.train <- n[samp,]
n.valid <- n[-samp,]

# conduct PCA on training dataset
pca <- prcomp(n.train[, which(colnames(n) %in% covariates)], retx=TRUE, center=TRUE, scale=TRUE)
expl.var <- round(pca$sdev^2/sum(pca$sdev^2)*100) # percent explained variance

# prediction of PCs for validation dataset
pred <- predict(pca, newdata=n.valid[, which(colnames(n) %in% covariates)])
pc <- c(1,2) # principal components to plot

v.pred <- as.data.frame(pred)

# VISUAL DISPLAY 3
###Plot result
COLOR <- c(2:4)
PCH <- c(1,16)

png("output/pca_with_training.png", units="in", width=5, height=4, res=200)
op <- par(mar=c(4,4,1,1), ps=10)
plot(pca$x[,pc], col=COLOR[n.train$gf], cex=PCH[1],
     xlab=paste0("PC ", pc[1], " (", expl.var[pc[1]], "%)"),
     ylab=paste0("PC ", pc[2], " (", expl.var[pc[2]], "%)")
)
points(pred[,pc], col=COLOR[n.valid$gf], pch=PCH[2])
legend("topright", legend=levels(n$gf), fill = COLOR, border = COLOR)
legend("topleft", legend=c("training data", "validation data"), col=1, pch=PCH)
par(op)
dev.off()


# PCA approach 3
#################################################################################
# Sandra script

#################### PCA on 6 traits #####################################
# select the traits

cov2 <- scale(cov, center = T, scale = T)

# use princomp for PCA for being consistent with scaling of scores in Sandra's analysis
prin <- princomp((cov2), cor = TRUE, scores = TRUE)
pc12 <- prin$scores[,1:2]
ll <- prin$loadings

# ????? check for consistent results
#pc12[,1]<-pc12[,1]*-1


################ KERNEL DENSITY ESTIMATION ##############################
library(vegan)
library(ks)

H <- Hpi(x=pc12)      # optimal bandwidth estimation
est<- kde(x=pc12, H=H, compute.cont=TRUE)     # kernel density estimation

# set contour probabilities for drawing contour levels
cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)

fit<-envfit(pc12, cov2) # use envfit for drawing arrows, can be also done using trait loadings
fit2<-fit$vectors$arrows*-1 # drawing line segments in arrow opposites direction for pretty layout

fit2<-fit$vectors$arrows*-1
#  not sure why these numbers.....
fit2[1,]<-fit2[1,]*4.5 # SLA
fit2[2,]<-fit2[2,]*5.5 # LDMC
fit2[3,]<-fit2[3,]*4.2 # CN


#pdf("output/pca_heatmap.pdf", width=8, height=6)

png("output/pca_heatmap.png", units="in", width=5, height=4, res=200)
par(mar=c(4,4,2,2))
plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, ylab="", xlab="",
     cex.axis=0.75, ylim=c(-6, 5), xlim=c(-5, 5),las=1)
plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
points( pc12[,], pch=16, cex=0.25, col="black")
plot(fit, cex=0.90, col=1, labels=list(vectors = c("HC", "CL", "LG", "SLA", "LDMC", "CN")))
segments(0,0, fit2[,1], fit2[,2], col=1, lty=2, lwd=1)
mtext("PC1", cex=0.75, side=1, line=0.5, adj=1)
mtext("PC2", cex=0.75, side=2, line=0.5, at=4.7) #, las=2)
dev.off()

