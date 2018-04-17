# Tables

#' Produce pca plot and loadings table
#'
#' @param prin pca output
#' @param output_file file path for loadings table
#' @return saved loadings table
#' @importFrom xtable xtable
#'
#' @export

pca_loadings <- function (prin, output_file) {
  
  # isolate first two axes 
  loadings <- as.data.frame(prin$loadings[,1:2])
  
  # expand trait names 
  loadings$trait[row.names(loadings) == 'SLA'] <- 'Specific litter area'
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

#' Produce table of GenBank Accessions
#'
#' @param genbank_accessions_file file path for accessions info
#' @param output_file file path to save table
#' @importFrom utils read.table
#' @importFrom reshape2 dcast
#' @importFrom xtable xtable
#' @return saved table file
#'
#' @export

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

#' Produce table of estimates of Mantel test of tree distance and trait distance 
#'
#' @param phylo phylogenetic tree
#' @param tips traits
#' @param output_file where to save mantel table
#' @importFrom adephylo distTips
#' @importFrom ade4 mantel.rtest
#' @importFrom stats dist
#' @importFrom xtable xtable
#' @return saved mantel table results
#'
#' @export

phylo_mantel <- function (phylo, tips, output_file) {
  
  # patristic distance matrix
  tre_matrix <- adephylo::distTips(phylo)
  
  # nNodes distance matrix
  # tre_matrix <- adephylo::distTips(phylo, tips = phylo$tip.label, method = 'nNodes')
  
  # initialise mantel test dataframe
  mantel_tests <- data.frame('Trait' = NA)
  
  # list of traits
  trait_list <- c('SLA', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')
  
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
        file = output_file)
  
}

#' Produce parameter values table
#'
#' @param species_deconvoluted_list list of deconvolved 
#' @param species_data dataframe with species info
#' @param output_file file path for output plot
#' @return saved parameters tables
#' @importFrom xtable xtable
#' @importFrom dplyr bind_rows
#' @importFrom utils write.table
#'
#' @export

tga_param_table <- function(species_deconvoluted_list, species_data, output_file) {
  
  # bind parameter outputs of species_deconvolved function
  parameter_estimates <- dplyr::bind_rows(lapply(1:length(species_deconvoluted_list), function(x) {
    return(species_deconvoluted_list[[x]]$params)
  }))
  
  # save parameter output
  write.table(parameter_estimates, 'data/tga_parameters.txt')
  
  # set significant digits
  parameter_estimates[, c('h1', 'h2', 'h3', 'h0')] <- signif(parameter_estimates[, c('h1', 'h2', 'h3', 'h0')], 4)
  parameter_estimates[, c('p1', 'p2', 'p3', 'p0')] <- signif(parameter_estimates[, c('p1', 'p2', 'p3', 'p0')], 3)
  parameter_estimates[, c('s1', 's2', 's3', 's0')] <- signif(parameter_estimates[, c('s1', 's2', 's3', 's0')], 2)
  parameter_estimates[, c('w1', 'w2', 'w3', 'w0')] <- signif(parameter_estimates[, c('w1', 'w2', 'w3', 'w0')], 2)
  
  # combine with species data
  parameters <- merge(parameter_estimates, species_data[,c('species_code', 'species')])
  
  # add italics latex code
  parameters$species <- paste0('\\textit{', parameters$species, '}')
  
  parameters <- parameters[ ,c('species', 'h0', 'h1', 'h2', 'h3', 
                'p0', 'p1', 'p2', 'p3', 
                's0', 's1', 's2', 's3', 
                'w0', 'w1', 'w2', 'w3')]
  
  # produce xtable
  parameters <- xtable::xtable(parameters)
  
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

#' Produce output trait table
#'
#' @param traits_df dataframe of traits
#' @param output_file file path for output plot
#' @return saved trait table
#' @importFrom xtable xtable
#'
#' @export

traits_table <- function (traits_df, output_file) {
  
  # modify growth form labels
  traits_df$gf <- as.character(traits_df$gf)
  traits_df$gf[traits_df$gf == 'G'] <- 'graminoid'
  traits_df$gf[traits_df$gf == 'F'] <- 'forb'
  traits_df$gf[traits_df$gf == 'NV'] <- 'nonvascular'
  traits_df$gf[traits_df$gf == 'S'] <- 'shrub'
  traits_df$gf[traits_df$gf == 'T'] <- 'tree'
  
  # # round trait values
  # traits_df$SLA <- sprintf("%.2f", round(traits_df$SLA, 2))
  # traits_df$DMC <- sprintf("%.0f", round(traits_df$DMC, 0))
  # traits_df[,c(8,9)] <- lapply(round(traits_df[,c(8,9)], 1), sprintf, fmt = "%.1f")
  # traits_df[,c(10:13)] <- round(traits_df[,c(10:13)], 1)
  # 
  # # for each biomass trait, paste mean and ci of weights together
  # for (i in c('HC_1', 'HC_2', 'CL', 'LG')) {
  #   
  #   for (j in unique(as.character(traits_df$species))) {
  #     
  #     if (!is.na(traits_df[(traits_df$wt_type == 'mean' & traits_df$species == j), i])) {
  #       
  #       traits_df[(traits_df$wt_type == 'mean' & traits_df$species == j), i] <- paste0(
  #         traits_df[(traits_df$wt_type == 'mean' & traits_df$species == j), i], ' [',
  #         traits_df[(traits_df$wt_type == '2.5%' & traits_df$species == j), i], ', ',
  #         traits_df[(traits_df$wt_type == '97.5%' & traits_df$species == j), i], ']')
  #       
  #     }
  #   }
  # }
  # 
  # subset to only include modified mean rows
  trt_table <- traits_df[traits_df$wt_type == 'mean', 3:13]

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