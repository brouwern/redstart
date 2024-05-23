# redstart

## Introduction 

The goal of `redstart` is to simulate the dynamics of migratory songbirds throughout their entire annual migratory cycle using a model developed by Runge and Marra (2004) "[Modeling seasonal interactions in the population dynamics of migratory birds](https://avibirds.com/wp-content/uploads/pdf/amerikaanse-roodstaart2.pdf)," from the monograph *[Birds of Two Worlds: The Ecology and Evolution of Migration](https://books.google.com/books/about/Birds_of_Two_Worlds.html?id=72Kp8vTzlhIC)*, edited by  Russell Greenberg and Peter Marra.

Key features of `redstart` are explained in the [package website](https://brouwern.github.io/redstart/index.html) under "[Tutorials](https://brouwern.github.io/redstart/articles/d-the_runFAC_command.html)"

The primary goal of the package is to model bird demographic rates through the breeding season, migration, and the wintering season to determine when during the annual cycle bird populations might be most vulnerable to changing conditions. 

There is large uncertainty in the parameters of this model, and so it must be run across ranges of values to determine where model stability and change points occur. A central figure produced by the model is shown below, where the x and y axes are two model parameters, and the z-axis is the equilibrium population size.


<img src="https://raw.githubusercontent.com/brouwern/redstart/master/Runge_Mara_Fig28_3.png" alt="Figure 28.3 of Runge and Marra 2004" width="375">



## Package name

The package takes its name from the [American Redstart](https://en.wikipedia.org/wiki/American_redstart), a common warbler in the forests of eastern North America and central Canada.

<img src="https://upload.wikimedia.org/wikipedia/commons/c/cd/Setophaga_ruticilla_-Chiquimula%2C_Guatemala_-male-8-4c.jpg" alt="American Redstart" width="250">

<br> 

The Redstart breeds in North American (yellow in the map below) and migrates through the American South East and South West to spend the winter in Mexico, The Caribbean, and South America (blue).

<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/6/6a/Setophaga_ruticilla_map.svg/1920px-Setophaga_ruticilla_map.svg.png" alt="American Redstart" width="250">


## Package installation

You can install the development version of `redstart` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("brouwern/redstart")
```

