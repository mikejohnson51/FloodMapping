# Flood Mapping <img src="man/figures/logo.png" width=160 height = 120 align="right" />

[![Build Status](https://travis-ci.org/mikejohnson51/FloodMapping.svg?branch=master)](https://travis-ci.org/mikejohnson51/FloodMapping)

The advent of the National Water Model and the development of the Height Above Nearest Drainage products (HAND) have provided the ability to map flood extents for anywhere in the lower 48 United States. The challenges with this methodology stem from the aquisition, managment and applciation of large scale data sets. 

FloodMapper is design to help users get the data they need, archive it on their local machines, and then process flood extents. In executing the steps in this process the first time the process in run the data is all collects and formated - meaning it is slow. After the intital pass though users can quickly generate flood maps for their region form realtime National Water Model output (getFlows), staged historic data (getRetro), and can adjust forecasted values to test regional sensitivites (adjust). 


#Use

`FloodMapper` uses the AOI package for defining regions of interest. More on that pakcage can be found [here](https://mikejohnson51.github.io/AOI/) and n this example we use the region of Colorado Springs.

```r
AOI = getAOI("Colorado Springs")
check(AOI) %>% addRa
```

![](/Users/mikejohnson/Documents/GitHub/LivingFlood/man/figures/AOI.png)


## Installation:
```
install.packages("devtools")
devtools::install_github("mikejohnson51/FloodMapper")
```

This package calls upon the [AOI](https://github.com/mikejohnson51/AOI) and [nwm](https://github.com/mikejohnson51/nwm) packages to forecast flood extents using the HAND methodology. 

The HAND datasets are a product of:

Liu, Yan Y., David R. Maidment, David G. Tarboton, Xing Zheng, and Shaowen Wang. 2018. A CyberGIS Integration and Computation Framework for High-Resolution Continental-Scale Flood Inundation Mapping. Journal of the American Water Resources Association (JAWRA). Accepted.

Liu, Yan Y., David R. Maidment, David G. Tarboton, Xing Zheng, Ahmet Yildirim, Nazmus S. Sazib and Shaowen Wang. 2016. A CyberGIS Approach to Generating High-resolution Height Above Nearest Drainage (HAND) Raster for National Flood Mapping. The Third International Conference on CyberGIS and Geospatial Data Science. July 26–28, 2016, Urbana, Illinois. http://dx.doi.org/10.13140/RG.2.2.24234.41925/1
