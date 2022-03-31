
> Here we cover how to setup `fasstrshiny` on your local computer as well 
> as how to start using the code output from `fasstrshiny`

## Using `fasstrshiny`

There are several ways to use `fasstrshiny`

#### **1. Online <https://bcgov-env.shinyapps.io/fasstrshiny/>**

**Pros**

-   No need to install R or `fasstrshiny`!

**Cons**

-   Slower
-   Bookmarking uses urls which can be very long
-   Won’t be learning any new R :(

#### **2. Locally**

To install, in the R console run the following (this needs to be done
**once**)

    install.packages("remotes")
    remotes::install_github("bcgov/fasstrshiny")

To use HYDAT data in the app, the Environment and Climate Change Canada’s HYDAT database
must be downloaded (this needs to be done **once** or as needed to
update):

    tidyhydat::download_hydat()

To the Shiny App run, in the R console, run the following (this needs to
be done **everytime**):

    library(fasstrshiny) # Loads the package
    fasstr_shiny()       # Launches the Shiny App

**Pros**

-   Faster
-   Bookmarking uses local files so urls are simpler
-   As you’re working in R already, it’s easier to start using the
    `fasstr` code output by the Shiny App to learn more!

**Cons**

-   You need to install R, RStudio (optional but recommended), and the
    `fasstrshiny` package
-   Working locally means that your system setup may occasionally create
    unique problems that are tricky to trouble shoot (when in doubt,
    update all your packages: `remotes::update_packages()`)
    
    
## Using code output from the fasstrshiny Shiny App

Every page in this app has an "R Code" tab. This tab shows the R code used
to create all the plots and figures on that particular page. 

Playing around with this code on your own is a great way to become more familiar
with R and fasstr. 

First, if you're brand new to R, consider running through 
[a primer on getting started in R and RStudio](https://education.rstudio.com/learn/beginner/).

Once you're comfortable working with basic R scripts, you're ready to take a 
stab playing with fasstr code!

### Loading packages

First, you'll need to make sure you have the right packages loaded. If you've
already installed the fasstrshiny package (as above) and have been using fasstrshiny
locally, you're good to go. Otherwise, install fasstr and the HYDAT database
using the instructions above.

### Running fasstr code

Each series of code starts with a block loading the data (`data_flow = ...`), 
either from the HYDAT data base or a local file (see Things to remember, below
if loading local data).

To run this code locally, copy all the code, and paste it into a script in 
RStudio (or similar R IDE).

There is one very important step to do now: **Add `library(fasstr)` to the top
of the script**. This only needs to be done once per script, and it's best to 
put this code at the top of a script since it needs to be loaded before any thing
else.

Now you can run the script! It should produce the figures and/or output you 
see in the fasstrshiny app. 

> Note that some complex figures (e.g. hydrographs with
> added dates and added MAD values) have been modified *after* the fasstr figure
> was produced so the code won't reproduce those figures exactly.

Saving this script is a great way to keep track of your analyses for future use.
You can also share them with colleagues. 

### Things to remember

#### **Updating code**

The R code panel generally *only* shows the last figure you produced.
So if you're on that tab and change a setting, it won't always be updated in the
code. To be safe, **always** look at the figure/table you want to reproduce
first, and *then* grab the code. 

This also means that if you don't look at a tab, the code won't appear in the 
R Code tab.

#### **Local data**

If you load a local data set, the R code data block will only show the name of 
the file. This means that **unless your file is in your working directory** the
loading code will error. For a better understanding of working directories, 
see [An Introduction to R - 1.7 Working directories](https://intro2r.com/work-d.html).

In general, try to work with RStudio projects, and keep your scripts and data 
together. If your data is in your working directory, you can run the code as is,
otherwise you'll need to modify the file name to include a location.

For example, "test.csv" becomes "data/test.csv" if I have test.csv in a `data` folder in my
working directory. 


  
  
  
  




