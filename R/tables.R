## Tables

# Produce pca plot and loadings table
pca_loadings <- function (prin, output_file) {
  
  # isolate first two axes 
  loadings <- as.data.frame(prin$loadings[,1:2])
  
  # expand trait names 
  loadings$trait[row.names(loadings) == 'LAM'] <- 'Litter area per mass'
  loadings$trait[row.names(loadings) == 'DMC'] <- 'Litter dry matter content'
  loadings$trait[row.names(loadings) == 'N'] <- 'Litter nitrogen'
  loadings$trait[row.names(loadings) == 'C'] <- 'Litter carbon'
  loadings$trait[row.names(loadings) == 'HC'] <- 'Litter hemicelluloses'
  loadings$trait[row.names(loadings) == 'CL'] <- 'Litter cellulose'
  loadings$trait[row.names(loadings) == 'LG'] <- 'Litter lignin'
  
  loadings <- loadings[,c('trait', 'Comp.1', 'Comp.2')]
  
  # create xtable
  pca_loadings <- xtable::xtable(loadings)
  
  print(pca_loadings,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        hline.after = NULL,
        file = output_file)
  
}

# Produce table of GenBank Accessions
phylo_accessions <- function (genbank_accessions_file, output_file) {
  
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
        file = output_file)
}

# Produce table of estimates of Mantel test of tree distance and trait distance 
phylo_mantel <- function (phylo, tips, output_file) {
  
  # patristic distance matrix
  tre_matrix <- adephylo::distTips(phylo)
  
  # nNodes distance matrix
  # tre_matrix <- adephylo::distTips(phylo, tips = phylo$tip.label, method = 'nNodes')
  
  # initialise mantel test dataframe
  mantel_tests <- data.frame('Trait' = NA)
  
  # list of traits
  trait_list <- c('LAM', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')
  
  # change this so not in for loop?
  for (i in unique(trait_list)) {
    x <- unique(trait_list)
    index <- which(x %in% i)
    trt_matrix <- stats::dist(tips[, which(names(tips) %in% i)])
    result <- ade4::mantel.rtest(trt_matrix, tre_matrix, nrepet = 9999)
    mantel_tests[index, 'Trait'] <- i
    mantel_tests[index, 'Mantel test observation'] <- round(result$obs, 2)
    mantel_tests[index, 'p-value'] <- round(result$pvalue, 2)
  }
  
  # expand trait names for table
  mantel_tests$Trait[mantel_tests$Trait == 'LAM'] <- 'Litter area per mass'
  mantel_tests$Trait[mantel_tests$Trait == 'DMC'] <- 'Litter dry matter content'
  mantel_tests$Trait[mantel_tests$Trait == 'N'] <- 'Litter nitrogen'
  mantel_tests$Trait[mantel_tests$Trait == 'C'] <- 'Litter carbon'
  mantel_tests$Trait[mantel_tests$Trait == 'HC'] <- 'Litter hemicelluloses'
  mantel_tests$Trait[mantel_tests$Trait == 'CL'] <- 'Litter cellulose'
  mantel_tests$Trait[mantel_tests$Trait == 'LG'] <- 'Litter lignin'
  
  # bold significant LAM
  mantel_tests[mantel_tests$Trait == 'Litter area per mass',] <- 
    paste0('\\textbf{', mantel_tests[mantel_tests$Trait == 'Litter area per mass',], '}')
  
  # create xtable
  mantel_results <- xtable::xtable(mantel_tests)
  
  print(mantel_results,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.text.function = identity,
        hline.after = NULL,
        file = output_file)
}

# Produce parameter values table
tga_param_table <- function (parameters, output_file) {
  
  parameters <- parameters[order(parameters$species),]
  
  # add italics latex code
  parameters$species <- paste0('\\textit{', parameters$species, '}')
  parameters <- parameters[ ,c('species',
                               'height_0', 'height_1', 'height_2', 'height_3', 
                               'position_0', 'position_1', 'position_2', 'position_3', 
                               'skew_0', 'skew_1', 'skew_2', 'skew_3', 
                               'width_0', 'width_1', 'width_2', 'width_3')]
  
  # produce xtable
  parameters <- xtable::xtable(parameters)
  xtable::digits(parameters) <- c(0, 0, 4, 4, 4, 4, 0, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 0)
  
  # save as .tex file
  print(parameters,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.text.function = identity,
        hline.after = NULL,
        file = output_file)
}

# Produce output trait table
traits_table <- function (traits_df, output_file) {
  
  # modify growth form labels
  traits_df$gf_old <- as.character(traits_df$gf_old)
  traits_df$gf_old[traits_df$gf_old == 'G'] <- 'Graminoid'
  traits_df$gf_old[traits_df$gf_old == 'F'] <- 'Forb'
  traits_df$gf_old[traits_df$species == 'Marsilea drumondii'] <- 'Fern'
  traits_df$gf_old[traits_df$gf_old == 'NV'] <- 'Non-vascular'
  traits_df$gf_old[traits_df$gf_old == 'S'] <- 'Shrub'
  traits_df$gf_old[traits_df$gf_old == 'T'] <- 'Tree'
  
  # round trait values
  traits_df$LAM <- sprintf("%.2f", round(traits_df$LAM, 2))
  traits_df$DMC <- sprintf("%.0f", round(traits_df$DMC, 0))
  traits_df[,c('N', 'C')] <- lapply(round(traits_df[,c('N', 'C')], 1), sprintf, fmt = "%.1f")
  traits_df[,c('HC_1', 'HC_2', 'CL', 'LG')] <- round(traits_df[,c('HC_1', 'HC_2', 'CL', 'LG')], 1)
  
  # for each biomass trait, paste mean and ci of weights together
  for (i in c('HC_1', 'HC_2', 'CL', 'LG')) {
    
    for (j in unique(as.character(traits_df$species))) {
      
      if (!is.na(traits_df[(traits_df$value_type == 'mean' & traits_df$species == j), i])) {
        
        traits_df[(traits_df$value_type == 'mean' & traits_df$species == j), i] <- paste0(
          traits_df[(traits_df$value_type == 'mean' & traits_df$species == j), i], ' [',
          traits_df[(traits_df$value_type == '2.5%' & traits_df$species == j), i], ', ',
          traits_df[(traits_df$value_type == '97.5%' & traits_df$species == j), i], ']')
        
      }
    }
  }
  
  # subset to only include modified mean rows
  trt_table <- traits_df[traits_df$value_type == 'mean', ]
  exclude_rows <- c('species_code', 'sp_abrev', 'value_type')
  trt_table <- trt_table[, -which(names(trt_table) %in% exclude_rows)]
  
  trt_table <- trt_table[c('family', 'species', 'gf', 'gf_old', 'LAM', 'DMC', 'N', 'C', 'HC_1', 'HC_2', 'CL', 'LG')]
  trt_table <- trt_table[with(trt_table, order(family, species)),]
  
  # add italics specification for latex
  trt_table$species <- paste0('\\textit{', trt_table$species, '}')
  
  # create xtable
  trt_table <- xtable::xtable(trt_table)
  
  print(trt_table,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.text.function = identity,
        hline.after = NULL,
        file = output_file)
}
