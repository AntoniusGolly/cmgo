# FAQ

## Support

If you experience problems with the package, please send an email to antonius.golly@gmail.com with following this checklist:
* read the [publication](https://www.earth-surf-dynam.net/5/557/2017/esurf-5-557-2017.html) first, it will give you a lot of technical details 
* attach a list of input files (ASCII files with river bank points)
* attach an R script with the relevant run commands you used
* attach images showing the problem (optional)

## Common technical fails

### 1. Maximum iterations
```
### exit due to maximum iterations (max. iterations = 20) ###
Note: this may be caused by gaps that opened in the centerline due to
jagged centerline paths. First, check for gaps visually with CM.plotPlanView(cmgo.obj, set="set1", error=1). 
You can than either repair these gaps by editing the centerline paths manually or 
simply increase the bank resolution via parameter par$bank.interpolation.max.dist! 
```

There is two common issues with that. To find out, which applies use the suggested command from the error message and enter a _zoom.length_ to show the problem:

`CM.plotPlanView(cmgo.obj, set="set1", error=1, zoom.length=20)`

#### 1.1 Gaps occurred in the centerline
<img src='https://raw.githubusercontent.com/AntoniusGolly/cmgo/master/man/figures/03-sep-spacing.png' alt='drawing' width='600'/>

If your plot shows something similar to the left image decrease the value of the parameter `par$bank.interpolate.max.dist` and run the program again.

#### 1.2 Segment cut-off not yet finished
<img src='https://raw.githubusercontent.com/AntoniusGolly/cmgo/master/man/figures/basins.png' alt='drawing' width='600'/>

In case you image shows open side arms of the centerline cmgo will handle that case if you increase the parameter `par$bank.filter2.max.it`. By default there is 20 iterations to remove those sidearms. Increase to a higher value (e.g. 50) and rerun the program.   