
# fasstr Shiny App

<!-- badges: start -->

[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Codecov test
coverage](https://codecov.io/gh/bcgov/fasstrshiny/branch/steffi-dev/graph/badge.svg)](https://app.codecov.io/gh/bcgov/fasstrshiny?branch=steffi-dev)
[![R-CMD-check](https://github.com/bcgov/fasstrshiny/workflows/R-CMD-check/badge.svg)](https://github.com/bcgov/fasstrshiny/actions)
<!-- badges: end -->

A Shiny app to analyze, summarize, and visualize daily streamflow data
💧.

This app takes advantage of bcgov’s
[{fasstr}](https://cran.r-project.org/package=fasstr) and
[{tidyhydat}](https://cran.r-project.org/package=tidyhydat) packages to
allow for an interactive way to view and customize the statistics,
tables, and plots created from {fasstr}’s streamflow analysis functions.
More information on {fasstr} and its functions can be found on its
[GitHub Page](https://bcgov.github.io/fasstr/) and [GitHub
repository](https://github.com/bcgov/fasstr).

Also see the Excel version, [FASSTX](https://github.com/bcgov/FASSTX/),
on [GitHub](https://github.com/bcgov/FASSTX/).

### Working with {fasstrshiny}

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

### Project Status

This project is under development. While many features are in place,
testing is still proceeding.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/fasstr_shiny/issues/).

### How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2021 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

------------------------------------------------------------------------

This repository is maintained by the [Water Protection and
Sustainability
Branch](https://www2.gov.bc.ca/gov/content/environment/air-land-water/water)
of the British Columbia Ministry of Environment and Climate Change
Strategy.
