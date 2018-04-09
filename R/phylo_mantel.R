
phylo_mantel <- function(phylo, tips, output_file) {
  
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