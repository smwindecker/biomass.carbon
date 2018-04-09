
phylo_readtree <- function (nwk_file) {
  
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
  
  phylo
  
}