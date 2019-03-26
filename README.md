
# Wetland plant biomass and economic spectrum traits summary 
Understanding variation and covariation in plant morphological and chemical traits is important for predicting plant species' effects on biogeochemical ecosystem processes. The 'fast-slow' economic spectrum identifies coordination of commonly measured traits between fast and slow extremes for a variety of ecosystem processes, including decomposition. Despite the relevance of litter carbon complexity to litter decomposability, coordination of biomass carbon traits along the fast-slow spectrum has not been examined. 

## We measure seven traits for 29 wetland plant species in order to examine:
* the utility of thermogravimetric analysis for estimating biomass carbon traits
* the variation in biomass carbon traits in plant litter 
* the covariation of biomass carbon traits with commonly measured fast-slow plant economic spectrum traits

## Reproducing analysis 
In order to aid in the reproducibility of this work, our code was written using a [remake](https://github.com/richfitz/remake) framework. This allows others to reproduce easily our entire workflow by calling `remake()` in R. We have created a Docker image to enable others to reproduce these results using the same software and versions we used to conduct the original analysis. The steps to do so are outlined below.

### Clone the biomass carbon repository on Github
First off, run the following line of code in the terminal to clone the project repository. 
```
git clone https://github.com/smwindecker/biomass.carbon /home/biomass.carbon
```

### Set up Docker
Docker sets up a virtual machine. This ensures you have the appropriate software and package versions installed to rerun this analysis without error. You'll first need to install [docker](https://www.docker.com/get-docker), and then install the image we have created. You can do this in two ways: 

1. Pull the docker image we have already created by running the following in the terminal:
```
docker pull smwindecker/biomass.carbon
```

2. Rebuild the image from scratch (this option is slower) from the dockerfile included in this repository. To do this open a terminal, navigate to the repository for this project that you downloaded on your home computer, and run:
```
docker build -t biomass.carbon .
```

### Rerunning workflow
To rerun the workflow we will run the docker image and open a new RStudio session. The flag `-v` mounts your local directory `/Users/path/to/biomass.carbon` (you'll have to put your respective path here), into the container at `/home/biomass.carbon`. Any results produced in the container will be automatically saved onto your local directory, so you can play with the results, data, and figures outside the docker container later.

1. Run docker image:

*For Mac & Linux users*
```
docker run -v /Users/path/to/biomass.carbon/:/home/biomass.carbon:/home/rstudio -p 8787:8787 smwindecker/biomass.carbon:latest
```

*For Windows users*
```
docker run -v c:\path\to\biomass.carbon\:/home/biomass.carbon:/home/rstudio -p 8787:8787 smwindecker/biomass.carbon:latest
```

2. Access Rstudio within docker by opening your web browser and going to `localhost:8787/`. Username and password are both `rstudio`.

3. Rerun the analysis:
```
remake::make()
```

If you are interested in a particular component you can simply look at the `remake.yml` file find the appropriate component you want to run and simply run the relevant target name. It will build all the relevant dependencies needed to produce that particular component.

For example if you were interested in just examining the mean values for the seven traits across all 29 species, you could extract that within the docker container by running:

```
trait_data <- remake::make("mean_traits")
```


## Docker Image metadata
| Docker Hub Build Status and URL                                | Image Size
| :-----------------------------------------                     | :--------------
| [good](https://registry.hub.docker.com/u/smwindecker/biomass.carbon/)  | [![Layers and Size](https://images.microbadger.com/badges/image/smwindecker/biomass.carbon.svg)](https://registry.hub.docker.com/u/smwindecker/biomass.carbon/)

Special thank you to [James Camac](https://github.com/jscamac) for assistance in setting up this reproducible workflow. 

## Problems?
If you have any problems getting the workflow to run please create an [issue](https://github.com/smwindecker/biomass.carbon/issues) and I will endevour to remedy it ASAP.
