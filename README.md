# cmgo
Deriving principle **c**hannel **m**etrics from bank and long-profile geometry with the R-package cmgo.

![Workflow](https://raw.githubusercontent.com/AntoniusGolly/cmgo/master/man/figures/01-processing.png)
<sup>Figure 1: visualization of the work flow of the package, a) the channel bank points that represent the data input, b) a polygon is generated where bank points are linearly interpolated, c-e) the centerline is calculated via Voronoi polygons, f) transects are calculated, g) the channel width is derived from the transects.</sup>

## What is cmgo?

**cmgo** performs geometrical calculations on your channel bank points  (Fig. 1.a), which represent your basic input. The calculated geometry includes the centerline (Fig. 1.e) of one or multiple given channel shapes, channel length, local and average channel width, local and average slope, local and average bank retreat, or the distances from the centerline to the banks respectively, as well as allows to project additional spatial metrics to the centerline.

## Motivation

Computer-aided products for studying rivers have a long tradition and numerous efforts exist to derive principle channel metrics from remote or in-situ measurements of channel banks. In [Golly et al. 2017b](http://www.earth-surf-dynam-discuss.net/esurf-2017-32/) we compare numerous tools for analyzing river geometry and conclude that *cmgo* adds a unique value to the available approaches by providing a tool for objectively deriving channel metrics, while being easy and free to use and modify and allowing a high degree of parametrization and fine-tuning.

## License / Citation

The program is free to use, modify and redistribute under the terms of the GNU General Public License version 3 as published by the Free Software Foundation. If you make use of the work, please cite the work as follows:

>Golly, A. and Turowski, J. M.: Deriving principal channel metrics from bank and long-profile geometry with the R package cmgo, Earth Surf. Dynam., 5, 557-570, https://doi.org/10.5194/esurf-5-557-2017, 2017.

Find the [paper at ESURF](https://www.earth-surf-dynam.net/5/557/2017/esurf-5-557-2017.html).

## Installation

[Get](https://cran.r-project.org/), install and open R and run the following code in the R console:

```R

# install devtools 
install.packages("devtools")

# load the devtool package
library(devtools)

# installation (required only once)
install_github("AntoniusGolly/cmgo")
```

## Run cmgo
```
# include the package (required for every start of an R session)
library(cmgo)

# set your working directory 
setwd("d:/your_folder") # in that folder an "input" folder must exist which contains one or more files with point data

# load parameter
par = CM.par()
par$bank.interpolate.max.dist = 4 # set roughly to your expected channel width

# load data assuming your file is lying in directory "input"
cmgo.obj = CM.ini(NULL, par)

# Generate a polygon from input data and plot // Fig. 1b
cmgo.obj = CM.generatePolygon(cmgo.obj)

# Generate the voronoi polygons and calculate the centerlin  // Fig. 1c-e
cmgo.obj = CM.calculateCenterline(cmgo.obj)

# Process the centerline (generate width) // Fig. 1f-g
cmgo.obj = CM.processCenterline(cmgo.obj)

```

## FAQ

For support and common technical fails please see the [FAQ](FAQ.md)

## Documentation

```
# package documentation
?cmgo

# function documentation
?cmgo.run # e.g. for the docu of cmgo.run()
```

