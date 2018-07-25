FROM rocker/verse:3.5.0
LABEL maintainer="Saras Windecker"
LABEL email="saras.windecker@gmail.com"

## Update and install extra packages
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    mesa-common-dev\
    libglu1-mesa-dev \
    libgsl0-dev \
    libomp-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/

## Add in required R packages
RUN . /etc/environment \
  && install2.r --error --repos $MRAN --deps TRUE \
  dotCall64 spam ape rgl phylobase minpack.lm phytools vegan xtable 

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

# Set working directory
WORKDIR /home/biomass.carbon
