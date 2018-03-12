#' Produce phylogeny outputs
#'
#' @param df dataframe of mean trait values
#' @param genbank_accessions_file file path for genbank accession codes
#' @param nwk_file file path for .nwk file
#' @param figure_folder folder path for plot output
#' @param table_folder folder path for table output
#' @return saved phylogeny files
#' @importFrom xtable xtable
#' @importFrom reshape2 dcast
#' @importFrom ape read.tree
#' @importFrom stats dist
#' @importFrom adephylo distTips
#' @importFrom ade4 mantel.rtest
#' @importFrom phytools contMap
#' @importFrom grDevices colorRampPalette
#'
#' @export

phylogeny <- function (df, genbank_accessions_file, nwk_file, figure_folder, table_folder) {
  
  accessions <- read.table(genbank_accessions_file, header = TRUE)
  accessions$Species <- paste(accessions$Species, accessions$Name)
  accessions$Gene <- NA
  accessions[1:25, 'Gene'] <- 'rbcl'
  accessions[27:47, 'Gene'] <- 'matK'
  colnames(accessions)[1] <- 'Code'
  accessions <- accessions[- which(is.na(accessions$Gene)), - which(names(accessions) %in% 'Name')]
  
  codes <- reshape2::dcast(accessions, Species ~ Gene, value.var = 'Code')
  codes$Species <- paste0('\\textit{', codes$Species, '}')
  rownames(codes) <- codes$Species
  codes <- codes[, - which(names(codes) %in% 'Species')]
  
  codes <- xtable::xtable(codes)
  print(codes,
        include.rownames = TRUE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.rownames.function = identity,
        hline.after = NULL,
        file = paste0(table_folder, 'GenBankAccessions.tex'))
  
  phylo <- ape::read.tree(nwk_file)
  
  phylo$tip.label <- c('Meuhlenbeckia', 'Rumex crispus', 'Persicaria decipiens', 
                       'Alternanthera', 'Lycopus', 'Myriophyllum', 'Crassula helmsii', 
                       'Eucalyptus camaldulensis', 'Melaleuca', 'Acacia dealbata', 
                       'Eleocharis acuta', 'Cyperus eragrostis', 'Carex', 
                       'Gahnia', 'Baumea articulata', 'Baumea rubiginosa', 
                       'Juncus', 'Paspalum distichum', 'Phragmites australis', 
                       'Restio tetraphyllus', 'Typha domingensis', 'Cycnogeton procerum', 
                       'Nymphaea alba', 'Marsilea drummondii', 'Sphagnum'
  )
  
  trt <- as.data.frame(phylo$tip.label)
  colnames(trt)[1] <- 'tip_label'
  trt[,trait_list] <- NA
  
  t_mean$genus <- gsub(' .*$', '', t_mean$species)
  t_mean$HC <- t_mean$HC_1 + t_mean$HC_2
  t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
  
  trait_list <- c('SLA', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')
  for (j in trait_list) {
    for (i in unique(trt$tip_label)) {
      if (i %in% t_mean$species) {
        trt[i, j] <- t_mean[t_mean$species == i, j]
      }
      if (i %in% t_mean$genus) {
        trt[i, j] <- mean(t_mean[t_mean$genus == i, j])
      }
    }
  }
  
  trt <- trt[is.na(trt$tip_label), -which(names(trt) %in% 'tip_label')]
  trt_matrix <- stats::dist(trt)
  
  # this one uses patristic
  tre_matrix <- adephylo::distTips(phylo)
  
  # this one uses nNodes >> mantel results differ
  # tre_matrix <- distTips(phylo, tips = phylo$tip.label, method = 'nNodes')
  
  mantel_tests <- data.frame('Trait' = NA)
  for (i in unique(trait_list)) {
    x <- unique(trait_list)
    index <- which(x %in% i)
    trt_matrix <- stats::dist(trt[, which(names(trt) %in% i)])
    result <- ade4::mantel.rtest(trt_matrix, tre_matrix, nrepet = 9999)
    mantel_tests[index, 'Trait'] <- i
    mantel_tests[index, 'Mantel test observation'] <- round(result$obs, 2)
    mantel_tests[index, 'p-value'] <- round(result$pvalue, 2)
  }
  
  mantel_tests$Trait[mantel_tests$Trait == 'SLA'] <- 'Specific litter area'
  mantel_tests$Trait[mantel_tests$Trait == 'DMC'] <- 'Litter dry matter content'
  mantel_tests$Trait[mantel_tests$Trait == 'N'] <- 'Litter nitrogen'
  mantel_tests$Trait[mantel_tests$Trait == 'C'] <- 'Litter carbon'
  mantel_tests$Trait[mantel_tests$Trait == 'HC'] <- 'Litter hemicelluloses'
  mantel_tests$Trait[mantel_tests$Trait == 'CL'] <- 'Litter cellulose'
  mantel_tests$Trait[mantel_tests$Trait == 'LG'] <- 'Litter lignin'
  
  mantel_tests[mantel_tests$Trait == 'Specific litter area',] <- 
    paste0('\\textbf{', mantel_tests[mantel_tests$Trait == 'Specific litter area',], '}')
  
  mantel_results <- xtable::xtable(mantel_tests)
  print(mantel_results,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.text.function = identity,
        hline.after = NULL,
        file = paste0(table_folder, 'mantel_results.tex'))
  
  for (i in trait_list) {
    trt_obj <- as.matrix(trt)[,i]
    png(paste0(figure_folder, 'phylo_', i, '.png'))
    obj <- phytools::contMap(phylo, trt_obj)
    
    # length of the required color ramp
    n <- length(obj$cols)
    
    PRGn <- c('#762a83', '#af8dc3', '#e7d4e8', '#f7f7f7', 
              '#d9f0d3', '#7fbf7b', '#1b7837')
    
    # change colour scheme
    obj$cols[1:n] <- grDevices::colorRampPalette(PRGn)(n)
    
    plot(obj)
    dev.off()
  }
  
}