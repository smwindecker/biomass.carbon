FROM rocker/tidyverse:3.3.2
MAINTAINER Saras Windecker <saras.windecker@gmail.com>

# Install latex, git and clang then clean up tmp files
RUN    apt-get update \
    && apt-get install -y --no-install-recommends \
         libcurl4-openssl-dev \
         texlive-latex-recommended \
         texlive-latex-extra \
         texlive-humanities \
         texlive-fonts-recommended \
         texlive-science \
         lmodern \
         git \
         clang \
    && apt-get clean \
    && apt-get autoremove \
    && rm -rf var/lib/apt/lists/*

# Global site-wide config
RUN mkdir -p $HOME/.R/ \
    && echo "\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function\n" >> $HOME/.R/Makevars \
    && echo "\nCXX=clang++ -ftemplate-depth-256\n" >> $HOME/.R/Makevars \
    && echo "CC=clang\n" >> $HOME/.R/Makevars

# Install other dependent R packages
RUN install2.r -r "https://mran.revolutionanalytics.com/snapshot/2018-04-017/" --error \
    --deps "TRUE" \
  tools

# Install deconvolve
RUN installGithub.r \
    --deps "TRUE" \
    smwindecker/deconvolve
    
# Install deconvolve
RUN installGithub.r \
    --deps "TRUE" \
    smwindecker/biomass.traits

# Install remake
RUN installGithub.r \
    --deps "TRUE" \
    richfitz/remake

# Remove unnecesarry tmp files
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Clone biomass traits repository
RUN git clone https://github.com/smwindecker/Biomass_Traits /home/Biomass_Traits

# Set working directory
WORKDIR /home/Biomass_Traits

# Open R
CMD ["R"]