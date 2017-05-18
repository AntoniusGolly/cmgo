# cmgo
Deriving principle **c**hannel **m**etrics from bank and long-profile geometry with the R-package cmgo.

![Workflow](https://raw.githubusercontent.com/AntoniusGolly/cmgo/master/man/figures/01_processing.png)
<sup>Figure 1: visualization of the work flow of the package, a) the channel bank points that represent the data input, b) a polygon is generated where bank points are linearly interpolated, c-e) the centerline is calculated via Voronoi polygons, f) transects are calculated, g) the channel width is derived from the transects.</sup>

## What is cmgo?

**cmgo** performs geometrical calculations on your channel bank points, which represent your basic input. The calculated geometry includes the centerline of one or multiple given channel shapes, channel length, local and average channel width, local and average slope, local and average bank retreat, or the distances from the centerline to the banks respectively, as well as allows to project additional spatial metrics to the centerline.

## Motivation

Computer-aided products for studying rivers have a long tradition and numerous efforts exist to derive principle channel metrics from remote or in-situ measurements of channel banks. However, available products are lacking independency, transparency or functionality that is necessary to fit the versatile requirements of academic or applied research and thus the call for software solutions remains present ([Golly et al. 2017, in review](http://www.earth-surf-dynam-discuss.net/esurf-2017-32/)). We conclude that none of the available approaches represents a tool for objectively deriving channel metrics, while being easy and free to use and modify and allowing a high degree of parametrization and fine-tuning.

## License / Citation

The program is free to use, modify and redistribute under the terms of the GNU General Public License version 3 as published by the Free Software Foundation. If you make use of the work, please cite the work as follows:

>**Citation**: Golly, A. and Turowski, J. M.: Deriving principle channel metrics from bank and long-profile geometry with the R-package cmgo, Earth Surf. Dynam. Discuss., doi:10.5194/esurf-2017-32, in review, 2017.

## Installation

There are two ways to use cmgo:
1. Include the cmgo package in R (for user with minor R experience, for users who do not intend to extend/modify the code)
2. Source the R scripts manually (for users with major R experience, for users who intend to extend/modify the code)

### 1. Include the cmgo package

[Get](https://cran.r-project.org/), install and open R (2 min.) and run the following code in the R console:

```R
# installation of dependencies (required only once)
install.packages(c("spatstat", "zoo", "sp", "stringr"))

# installation (required only once)
install.packages("cmgo", repos="http://code.backtosquareone.de", type="source")

# include the package (required for every start of an R session)
library(cmgo)

# open the help and get started
?cmgo
```

### 2. Source the R sciprts manually
Alternatively, you can source the scipts manually. This has the benefit, that you can directly modify the scripts. To do this, download the directories **./R** and **./data** from this repository and copy it to a location on your disc (e.g. "C:/cmgo"). Then run the following code:

```R

path_to_cmgo = "C:/cmgo" # path to cmgo directories "./R" and "./data"

# installation of dependencies (required only once)
install.packages(c("spatstat", "zoo", "sp", "stringr"))

# load libraries
library(zoo)
library(spatstat)
library(stringr)
library(sp)

# source functions and load data
for(function.file in list.files(paste(path_to_cmgo, "/R",    sep="")) source(paste(path_to_cmgo, "/R/",    function.file, sep=""))
for(data.file     in list.files(paste(path_to_cmgo, "/data", sep="")) load(paste(path_to_cmgo,   "/data/", data.file,     sep=""))

```