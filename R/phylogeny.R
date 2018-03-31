#' Produce phylogeny outputs
#'
#' @param df dataframe of mean trait values
#' @param genbank_accessions_file file path for genbank accession codes
#' @param nwk_file file path for .nwk file
#' @param output_folder folder path for plot output
#' @return saved phylogeny files
#' @importFrom xtable xtable
#' @importFrom reshape2 dcast
#' @importFrom ape read.tree
#' @importFrom stats dist
#' @importFrom adephylo distTips
#' @importFrom ade4 mantel.rtest
#' @importFrom phytools contMap
#' @importFrom grDevices colorRampPalette png plot dev.off
#' @importFrom utils read.table
#'
#' @export

phylogeny <- function (df, genbank_accessions_file, nwk_file, figure_folder, table_folder) {
  
  # read accessions file
  accessions <- read.table(genbank_accessions_file, header = TRUE)
  
  # gather species name
  accessions$Species <- paste(accessions$Species, accessions$Name)
  
  # specify gene used for sequence
  accessions$Gene <- NA
  accessions[1:25, 'Gene'] <- 'rbcl'
  accessions[27:47, 'Gene'] <- 'matK'
  colnames(accessions)[1] <- 'Code'
  accessions <- accessions[- which(is.na(accessions$Gene)), - which(names(accessions) %in% 'Name')]
  
  # wide versions of accession codes
  codes <- reshape2::dcast(accessions, Species ~ Gene, value.var = 'Code')
  
  # specify italics for latex
  codes$Species <- paste0('\\textit{', codes$Species, '}')
  rownames(codes) <- codes$Species
  codes <- codes[, - which(names(codes) %in% 'Species')]
  
  # create xtable
  codes <- xtable::xtable(codes)
  
  print(codes,
        include.rownames = TRUE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.rownames.function = identity,
        hline.after = NULL,
        file = paste0(output_folder, 'GenBankAccessions.tex'))
  
  # read tree file
  phylo <- ape::read.tree(nwk_file)
  
  # rewrite the tip labels so that those that aren't my species only show Genus
  phylo$tip.label <- c('Meuhlenbeckia', 'Rumex crispus', 'Persicaria decipiens', 
                       'Alternanthera', 'Lycopus', 'Myriophyllum', 'Crassula helmsii', 
                       'Eucalyptus camaldulensis', 'Melaleuca', 'Acacia dealbata', 
                       'Eleocharis acuta', 'Cyperus eragrostis', 'Carex', 
                       'Gahnia', 'Baumea articulata', 'Baumea rubiginosa', 
                       'Juncus', 'Paspalum distichum', 'Phragmites australis', 
                       'Restio tetraphyllus', 'Typha domingensis', 'Cycnogeton procerum', 
                       'Nymphaea alba', 'Marsilea drummondii', 'Sphagnum')
  
  trt <- as.data.frame(phylo$tip.label)
  colnames(trt)[1] <- 'tip_label'
  
  # list of traits
  trait_list <- c('SLA', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')
  
  # create empty columns for each traits
  trt[, trait_list] <- NA
  
  # add genus
  t_mean$genus <- gsub(' .*$', '', t_mean$species)
  
  # overall HC
  t_mean$HC <- t_mean$HC_1 + t_mean$HC_2
  t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
  
  # input trait value into matrix from dataframe
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
  
  trt <- trt[is.na(trt$tip_label), - which(names(trt) %in% 'tip_label')]
  
  # patristic distance matrix
  tre_matrix <- adephylo::distTips(phylo)
  
  # nNodes distance matrix
  # tre_matrix <- adephylo::distTips(phylo, tips = phylo$tip.label, method = 'nNodes')
  
  # initialise mantel test dataframe
  mantel_tests <- data.frame('Trait' = NA)
  
  # 
  for (i in unique(trait_list)) {
    x <- unique(trait_list)
    index <- which(x %in% i)
    trt_matrix <- stats::dist(trt[, which(names(trt) %in% i)])
    result <- ade4::mantel.rtest(trt_matrix, tre_matrix, nrepet = 9999)
    mantel_tests[index, 'Trait'] <- i
    mantel_tests[index, 'Mantel test observation'] <- round(result$obs, 2)
    mantel_tests[index, 'p-value'] <- round(result$pvalue, 2)
  }
  
  # expand trait names for table
  mantel_tests$Trait[mantel_tests$Trait == 'SLA'] <- 'Specific litter area'
  mantel_tests$Trait[mantel_tests$Trait == 'DMC'] <- 'Litter dry matter content'
  mantel_tests$Trait[mantel_tests$Trait == 'N'] <- 'Litter nitrogen'
  mantel_tests$Trait[mantel_tests$Trait == 'C'] <- 'Litter carbon'
  mantel_tests$Trait[mantel_tests$Trait == 'HC'] <- 'Litter hemicelluloses'
  mantel_tests$Trait[mantel_tests$Trait == 'CL'] <- 'Litter cellulose'
  mantel_tests$Trait[mantel_tests$Trait == 'LG'] <- 'Litter lignin'
  
  # bold significant SLA
  mantel_tests[mantel_tests$Trait == 'Specific litter area',] <- 
    paste0('\\textbf{', mantel_tests[mantel_tests$Trait == 'Specific litter area',], '}')
  
  # create xtable
  mantel_results <- xtable::xtable(mantel_tests)
  
  print(mantel_results,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.text.function = identity,
        hline.after = NULL,
        file = paste0(output_folder, 'mantel_results.tex'))
  
  for (i in trait_list) {
    trt_obj <- as.matrix(trt)[,i]
    
    png(paste0(output_folder, 'phylo_', i, '.png'))
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