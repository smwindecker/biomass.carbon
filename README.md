
# Wetland plant biomass and economic spectrum traits summary 
Understanding variation and covariation in plant morphological and chemical traits is important for predicting plant species' effects on biogeochemical ecosystem processes. The 'fast-slow' economic spectrum identifies coordination of commonly measured traits between fast and slow extremes for a variety of ecosystem processes, including decomposition. Despite the relevance of litter carbon complexity to litter decomposability, coordination of biomass carbon traits along the fast-slow spectrum has not been examined. 

## We measure seven traits for 29 wetland plant species in order to examine:
* the utility of thermogravimetric analysis for estimating biomass carbon traits
* the variation in biomass carbon traits in plant litter 
* the covariation of biomass carbon traits with commonly measured fast-slow plant economic spectrum traits

## Running the code

All analyses were done in `R`, and the paper is written in LaTeX. All code needed to reproduce the submitted products is included in this repository. To reproduce this paper, run the code contained in the `analysis.R` file. Figures will be output to a directory called `output` and the paper and supplementary materials to the directory `ms`.

If you are reproducing these results on your own machine, first download the code and then install the required packages, listed under `Depends` in the `DESCRIPTION` file. This can be achieved by opening the Rstudio project and running:

```{r}
#install.packages("remotes")
remotes::install_deps()
```

You can access an interactive RStudio session with the required software pre-installed by opening a container hosted by [Binder](http://mybinder.org): 

[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/smwindecker/biomass.carbon/master?urlpath=rstudio)

To ensure long-term [computational reproducibility](https://www.britishecologicalsociety.org/wp-content/uploads/2017/12/guide-to-reproducible-code.pdf) of this work, we have created a [Docker](http://dockerhub.com) image to enable others to reproduce these results on their local machines using the same software and versions we used to conduct the original analysis. Instructions for reproducing this work using the docker image are available at the bottom of the page. 

A copy of the data has also been archived in Datadryad at [doi:10.5061/dryad.701q8](https://datadryad.org/resource/doi:10.5061/dryad.701q8). 

## Material included in the repository include:

- `data/`: Raw data
- `R/`: directory containing functions used in analysis
- `ms/`: directory containing manuscript in LaTeX and accompanying style files 
- `img/`: images used in the manuscript, made with other tools
- `figs/`: figures built with the R scripts for the manuscript
- `DESCRIPTION`: A machine-readable [compendium]() file containing key metadata and dependencies 
- `LICENSE`: License for the materials
- `Dockerfile` & `.binder/Dockerfile`: files used to generate docker containers for long-term reproducibility

## Running via Docker

If you have Docker installed, you can recreate the computing environment as follows in the terminal. 

From the directory you'd like this repo saved in, clone the repository:

```
git clone https://github.com/smwindecker/biomass.carbon.git
```

Then fetch the container:

```
docker pull smwindecker/biomass.carbon
```

Navigate to the downloaded repo, then launch the container using the following code (it will map your current working directory inside the docker container): 

```
docker run --user root -v $(pwd):/home/rstudio/ -p 8787:8787 -e DISABLE_AUTH=true smwindecker/biomass.carbon
```

The code above initialises a docker container, which runs an RStudio session accessed by pointing your browser to [localhost:8787](http://localhost:8787). For more instructions on running docker, see the info from [rocker](https://hub.docker.com/r/rocker/rstudio).

### NOTE: Building the docker image

For posterity, the docker image was built off [`rocker/verse:3.6.1` container](https://hub.docker.com/r/rocker/verse) via the following command, in a terminal contained within the downloaded repo:

```
docker build -t smwindecker/biomass.carbon .
```

and was then pushed to [dockerhub](https://cloud.docker.com/u/traitecoevo/repository/docker/smwindecker/biomass.carbon). The image used by binder builds off this container, adding extra features needed by binder, as described in [rocker/binder](https://hub.docker.com/r/rocker/binder/dockerfile).

## Problems?
If you have any problems getting the workflow to run please create an [issue](https://github.com/smwindecker/biomass.carbon/issues) and I will endevour to remedy it ASAP.

Special thank you to [James Camac](https://github.com/jscamac) and [Daniel Falster](https://github.com/dfalster) for assistance in setting up this reproducible workflow. 
