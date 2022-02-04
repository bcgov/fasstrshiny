
library(rsconnect)

# Deplying on shinyapps.io ----------------------------------------------------

## Configuring -------------------
# Make sure you have configured rsconnect
# (https://docs.rstudio.com/shinyapps.io/getting-started.html#configure-rsconnect)

# rsconnect::setAccountInfo(name='bcgov-env', token=XXXXX, secret=XXXX)

# Make sure you first install package from a remote host (i.e. CRAN, GitHub, etc.)
# In particular, fasstrshiny should be installed from GitHub

# remotes::install_github("bcgov/fasstrshiny", ref = "steffi-dev")


## Set up HYDAT data base ------

# We need to include HYDAT data in the shinyapps.io, but NOT in the fasstrshiny
# package. The shiny app will check for HYDAT data in either the inst/shiny_app
# folder OR the normal user cache. If it can't find it in either it'll proceed
# with downloading as normal

# Compare versions
hy <- find_hydat() # "inst/shiny_app/Hydat.sqlite3"

v1 <- lubridate::as_date(tidyhydat::hy_version(hy)$Date) #LOCAL
v2 <- "https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/" %>%
  httr::GET() %>%
  httr::content("text") %>%
  stringr::str_extract("(?<=Hydat_sqlite3_)[0-9]{4,8}") %>%
  lubridate::ymd() #ONLINE

v1
v2

# Get if doesn't exist or need to update
if(v1 != v2) tidyhydat::download_hydat("inst/shiny_app/")

# Make sure ignored
usethis::use_git_ignore("inst/shiny_app/Hydat.sqlite3")


## Deploy shiny app --------------------------------

deployApp()
