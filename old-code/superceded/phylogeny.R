
## phylogeny script

library(phytools)
library(ade4)
library(adephylo)
library(scales)
library(xtable)

# phy tree
phylo <- read.tree('raw/phy_data/RAxML_bestTree.tre')

phylo$tip.label <- c('Sphagnum', 'Nymphaea alba', 'Lycopus', 'Myriophyllum', 
                     'Crassula helmsii', 'Acacia dealbata', 'Eucalyptus camaldulensis', 
                     'Melaleuca', 'Alternanthera', 'Rumex crispus', 
                     'Persicaria decipiens', 'Meuhlenbeckia', 'Typha domingensis', 
                     'Juncus', 'Baumea articulata', 'Baumea rubiginosa', 
                     'Carex', 'Eleocharis acuta', 'Cyperus eragrostis', 'Gahnia', 
                     'Paspalum distichum', 'Phragmites australis', 'Restio tetraphyllus', 
                     'Cycnogeton procerum', 'Marsilea drummondii'
                     )

trt <- as.data.frame(phylo$tip.label)
colnames(trt)[1] <- 'tip_label'
trait_list <- c('SLA', 'LDMC', 'LNC', 'LCC', 'HC', 'CL', 'LG')
trt[,trait_list] <- NA
  
t_1 <- read.table('munge/mean_traits.txt')
t_1$genus <- gsub(' .*$', '', t_1$species)
t_1$HC <- t_1$HC_1 + t_1$HC_2
t_1$HC[is.na(t_1$HC)] <- t_1$HC_2[is.na(t_1$HC)]

for (j in trait_list) {
  for (i in unique(trt$tip_label)) {
    if (i %in% t_1$species) {
      trt[i, j] <- t_1[t_1$species == i, j]
    }
    if (i %in% t_1$genus) {
      trt[i, j] <- mean(t_1[t_1$genus == i, j])
    }
  }
}

trt <- trt[is.na(trt$tip_label), -which(names(trt) %in% 'tip_label')]
trt_matrix <- dist(trt)

# this one uses patristic
tre_matrix <- distTips(phylo)

# this one uses nNodes >> mantel results differ
# tre_matrix <- distTips(phylo, tips = phylo$tip.label, method = 'nNodes')

mantel_tests <- data.frame('Trait' = NA)
for (i in unique(trait_list)) {
  x <- unique(trait_list)
  index <- which(x %in% i)
  trt_matrix <- dist(trt[, which(names(trt) %in% i)])
  result <- mantel.rtest(trt_matrix, tre_matrix, nrepet = 9999)
  mantel_tests[index, 'Trait'] <- i
  mantel_tests[index, 'Mantel test observation'] <- round(result$obs, 2)
  mantel_tests[index, 'p-value'] <- round(result$pvalue, 2)
}

mantel_tests$Trait[mantel_tests$Trait == 'SLA'] <- 'Specific litter area'
mantel_tests$Trait[mantel_tests$Trait == 'LDMC'] <- 'Litter dry matter content'
mantel_tests$Trait[mantel_tests$Trait == 'LNC'] <- 'Litter nitrogen'
mantel_tests$Trait[mantel_tests$Trait == 'LCC'] <- 'Litter carbon'
mantel_tests$Trait[mantel_tests$Trait == 'HC'] <- 'Litter hemicelluloses'
mantel_tests$Trait[mantel_tests$Trait == 'CL'] <- 'Litter cellulose'
mantel_tests$Trait[mantel_tests$Trait == 'LG'] <- 'Litter lignin'

mantel_tests[mantel_tests$Trait == 'Specific litter area',] <- 
  paste0('\\textbf{', mantel_tests[mantel_tests$Trait == 'Specific litter area',], '}')

mantel_results <- xtable(mantel_tests)
print(mantel_results,
      include.rownames = FALSE,
      include.colnames = FALSE,
      only.contents = TRUE,
      comment = FALSE,
      sanitize.text.function = identity,
      hline.after = NULL,
      file = 'docs/mantel_results.tex')

for (i in trait_list) {
  trt_obj <- as.matrix(trt)[,i]
  png(paste0('figs/phylo_', i, '.png'))
  obj <- contMap(phylo, trt_obj)
  
  # length of the required color ramp
  n <- length(obj$cols)
  
  PRGn <- c('#762a83', '#af8dc3', '#e7d4e8', '#f7f7f7', 
            '#d9f0d3', '#7fbf7b', '#1b7837')
  
  # change colour scheme
  obj$cols[1:n] <- colorRampPalette(PRGn)(n)

  plot(obj)
  dev.off()
}

