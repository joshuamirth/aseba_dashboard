# aseba_dashboard

A simple R shiny dashboard for visualizing Achenbach System of Empirically
Based Assessment data.



## Installation Instructions

This dashboard is powered by R Shiny. To run a local copy you will need to
install R and several associated packages.


### Installing R

The easiest way to install R is via the R Studio IDE which can be downloaded
here: https://www.rstudio.com/products/rstudio/download/ (the free RStudio
desktop version is sufficient). Select "Download" and follow the installation
instructions for your platform.


### Installing Dependencies

The dashboard depends on three packages which are not included in the default R
distribution. To install these, open the RStudio IDE and select the "packages"
tab in the lower right. Click the install button and enter the name of the
package. (Make sure that "install dependencies" is selected.) The three
packages to install are `shiny`, `fmsb`, and `formattable`.

![Package installation](/images/install_package.png)

You can also install these from the R Console (be default in the lower left of
the RStudio IDE). The appropriate command is `install.packages($PACKAGE_NAME)`,
replacing `$PACKAGE_NAME` with the name of the desired package.


### Installing the Dashboard

To install the ASEBA Dashboard, click the green "Code" button in the upper
right of this Github page and either clone the package or download the zip
file. If downloading the zip file, unzip it to a folder on your computer. The
dashboard is now installed.

To test your installation, navigate to the downloaded folder and open `app.R`
with RStudio. Click the "Run App" button at the top center of the IDE, or enter
`shiny::runApp()` at the console. The dashboard should open displaying some
dummy example data.


### Configuring the Dashboard

To view data, navigate to the folder containing the R code. There ought to be a
subfolder titled `data` containing an R database `example.Rda`. Add your R
database file to this folder.

Back in RStudio with `app.R` open in the editor, the 9th line of `app.R` gives
the location of your data file. By default it is `df <- readRDS("data/example.Rda")`.
Replace `example.Rda` with the name of your data file.

![Data configuration](/images/data_config.png)

