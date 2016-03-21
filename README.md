# multifractal_model

This is a financial statistic project. The aim is to implement some volatilities modelisation, more especially multi fractal volatilities - called also markov switching model.

#### 1. What it does ?
 It uses a brew template (template.brew) - latex which embeds R commands- to generates a pdf report which contains :
 * ARMA examples

#### 2. Run it !
The command has to be launch from the repository : `R -f script.r`.
Or from R : `source("script.r");main_report("./")`

Why *from the repository* ?
 - it creates various directories and filled it with PDF (lots of).
 
You might need :
 - to install R, and some R packages
    * to install R packages, launch R, then type `install.packages("packagename")`
 - to install latex (and eventualy texinfo)


The report is saved in : generated_report.pdf

Best,
Marouane