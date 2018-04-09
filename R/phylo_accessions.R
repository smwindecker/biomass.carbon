
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
  
  