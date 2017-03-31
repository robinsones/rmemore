# rmemore

This package introduces the `mediate_ws` function, which replicates part of the [MEMORE](http://afhayes.com/spss-sas-and-mplus-macros-and-code.html) macro for SPSS and SAS written by Montoya and Hayes. As they describe:

> It estimates the total, direct, and indirect effects of X on Y through one or more mediators M in the two-condition or two-occasion within-subjects/repeated measures design. In a path-analytic form using OLS regression as illustrated in Montoya and Hayes (2015), it implements the method described by Judd, Kenny, and McClelland (2001, Psychological Methods) and extended by Montoya and Hayes (2015) to multiple mediators. Along with an estimate of the indirect effect(s), MEMORE generates confidence intervals for inference about the indirect effect(s) using bootstrapping approach. 


# Installation 

To install the package, you will need to use [devtools](https://github.com/hadley/devtools) as the package is not yet on CRAN: 

```
library(devtools)
install_github("erobinson/rmemore")
```

# Usage

`mediate_ws` calculates the indirect effects of an independent variable on a dependent variable through a mediator in two-condition within-subjects design. It returns a dataframe with the type of bootstrap, the confidence interval level, the lower and upper bounds of the confidence interval, and the estimate. The inputs are the dataset, the mediating variable at time 1, mediating variable at time 2, dependent variable at time 1, and dependent variable at time 2. 

`mediate_ws` currently includes options for setting the number of bootstrap samples, the confidence interval width, and the type of bootstrap method. 

Included in the package is `sample_data`, the data used in Montoya and Hayes' paper, to illustrate how the function works. To try it out, run: 

`mediate_ws(sample_data, M1, M2, Y1, Y2, Reps = 3000, CONF = c(.9, .95, .99), BSType = "bca")`
