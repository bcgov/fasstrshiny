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

To use the app, a Environment and Climate Change Canada’s HYDAT database
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
