FROM rocker/verse:3.5.0
LABEL maintainer="Saras Windecker"
LABEL email="saras.windecker@gmail.com"

## Update and install extra packages
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    clang \
    mesa-common-dev\
    libglu1-mesa-dev \
    libgsl0-dev \
    libomp-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/

## Add in opts
# Global site-wide config for clang
RUN mkdir -p $HOME/.R/ \
    && echo "\nCXX=clang++ -ftemplate-depth-256\n" >> $HOME/.R/Makevars \
    && echo "CC=clang\n" >> $HOME/.R/Makevars

## Add in required R packages
RUN . /etc/environment \
  && install2.r --error --repos $MRAN --deps TRUE \
  dotCall64 spam ape rgl phylobase knitr minpack.lm phytools plyr reshape2 vegan xtable 

## Add in required R packages (without suggestions)
RUN . /etc/environment \
  && install2.r --error --repos $MRAN --deps FALSE \
  segmented sp seqinr igraph boot adegenet adephylo

# Install github packages
RUN installGithub.r \
    --deps "TRUE" \
    smwindecker/deconvolve \
    richfitz/remake

# Remove unnecessary tmp files
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Clone biomass carbon repository
RUN git clone https://github.com/smwindecker/biomass.carbon /home/biomass.carbon

# Set working directory
WORKDIR /home/biomass.carbon