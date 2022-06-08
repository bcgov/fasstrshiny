---
editor_options: 
  markdown: 
    wrap: 72
---

> This is broad overview of how to use this Shiny app

## Getting Started

### Loading Station Data

The first thing you'll need to do is load some daily streamflow data. Go
to **Data \>\> Loading & Options**. Here you can choose whether to load
HYDAT data or your own local data from a .csv file format.

#### **HYDAT Data**

The map shows all the HYDAT stations in BC. To look at all of Canada,
unselect "BC Stations Only". Hover over stations for more information,
click on a station to select that station. Below the map is a table of
HYDAT stations. You can filter stations by typing into the box above
each column. As you filter the stations list, the map will update to
show only the stations you've filtered. Once you've decided on a
station, make sure it is selected in the panel on the left and click the
"Load Data" button. You can now preview the data on the "Daily Flow
Plot" and "Daily Flow Table" tabs.

If there is missing data between the start and end of the period of
record, a note in red text will pop up below this section. Missing data
may cause plots or tables to have gaps throughout the app. To manage how
to deal with missing dates, see the 'handling Missing Dates' options at
the bottom of the options.

#### **Local Data**

Click on the CSV button to change the data source to local csv. Your
data must be in csv format with at least two columns: one for the date,
and one for the flow value (in cms). Click on the "Browse" button to
locate your file. You can preview the file on the right. Make sure the
Date and Flow columns are correctly identified. If you have a Symbol
column (with daily flow qualifers, Estimate, Partial day etc.) you can
add that as well, otherwise leave that entry blank. If you know the
basin area for your data, add it to under "Station Information". Click
"Load Data".

If your dates are not standardized (YYYY-MM-DD) or if you have more than
one flow value per date, you will be warned and will have to fix your
data before it can be loaded.

You can now preview the data on the "Daily Flow Plot" and "Daily Flow
Table" tabs.

If there is missing data between the start and end of the period of
record, a note in red text will pop up below this section. Missing data
may cause plots or tables to have gaps throughout the app. To manage how
to deal with missing dates, see the 'handling Missing Dates' options at
the bottom of the options.

#### **Station Information**

Click this toggle to view and/or change the station name (used for
titles throughout the app) and the upstream basin area, in square
kilometres (used for area-based runoff calculations). When using HYDAT
data, these will default to the provided HYDAT information, but can be
modified as necessary.

### Setting Data Options

Below the Load Station Data and Station Information (can click the
toggles to close), explore data and date settings to customize the
station information, filter your dates, adjust the types of data you're
using or handle missing dates differently (also see the section below,
**Data** **Options**). These options will also be shown in the sidebar
on the left below all the tab selections for viewing throughout the app.
Options sections:

-   **Flow Averaging and Units**
    -   Change the duration of the daily data by averaging sequential
        daily data (ie. 7-day rolling average); alignment will determine
        where the date of rolling average will align ('right' = last of
        n-days, 'left' = first of n-days, and 'centre' = middle of
        n-days).

    -   Convert the daily cubic metres per second data into a daily
        volumetric (cubic metres) or a daily runoff yield (millimetres
        depth; requires an upstream basin area) before calculating
        statistics and plots.
-   **Years and Months**
    -   Select the starting month of the your water year. Typically
        January for following calendar years or October for typical
        water years. Other months may be used to designate start of
        water years for other purposes
    -   Select the months to include in the analyses. If looking just
        for summer months from July through September, for example, move
        the slider ends to that range of months.
    -   Select the start and end years of the analysis. Certain time
        periods of interest or data availability or quality may
        influence start and end years.
    -   Select any years to remove from the analysis. There may be years
        or poor data quality, data availability, outliers, or other
        factors that may exist to remove specific years.
-   **Handling Missing Dates**
    -   Filter the flow data set for only those years which have
        complete data (no missing values). By toggling this, it will
        change the Start/End years slider to the first and last years of
        complete data and will add any years with complete data between
        those years into Years to Exclude box.

    -   If complete years are not require, choose to ignore any missing
        values and calculate statistics for a given time period
        regardless of any missing dates. This will move the 'Allowed
        Missing' slider to jump to 100% below, select the percent
        allowed missing values per period below.

    -   Adjust the percent of allowed missing values per time period
        (months, years). If 100%, statistics will be calculated
        regardless of the number of missing values (i.e. even if 1 good
        value per year or month). If 25%, statistics will be calculated
        if there is if 25% or less missing data per period (at least 75%
        non-missing values). If set to 0%, then no statistics will be
        calculated per period if there is missing data.

### Data Availability & Screening

Now that you've loaded your data, you'll want review your data and
screen the data for data availability (missing dates) and data quality
(data qualifiers/symbols or outliers). Click on **Data \>\> Availability
& Screening** to view these plots and tables.

This section contains several different tools for exploring your data:

-   **Daily Symbols Flow Plot** - Interactive plotly plot for exploring
    Symbols in your data.
-   **Annual Symbols Plot** - Summary of the daily flow
    symbols/qualifiers. Can view data by days of years or show annual
    totals (as percent or counts of days).
-   **Data Availability Plo**t - Summary of the amount of missing data
    per months over all years. Can change the plot type and months to
    include on the plot.
-   **Data Summary Plot** - View high-level annual basic summary
    statistics (annual mean, maximum, minimum, median, and standard
    deviations) of the data for screening. If there is missing data for
    a given year, the plot symbol will be hollow (not red-filled). Can
    choose which statistics to include on the plot.
-   **Data Summary Table** - Tabular summary of all the screening
    statistics; including number of days and missing days per year, the
    number of symbols per year (if provided), the basic summary
    statistics, and the number of missing values for each month of year.

**Filtering** and **Missing Dates**. If after reviewing your data you
find any periods that are problematic and you would like to omit from
your analysis, go to **Data \>\> Loading & Options**, and adjust the
dates under "Years and Months". Similarly, if you have a lot of missing
data, you can specify how these values should be handled under "Handling
Missing Dates". See previous section for more descriptions of the
options.

### Overview

For an overview of the summary statistics and flow patterns for your
selected station, explore the **Overview** tab. This tab will provide
the following information:

-   **Long-term Data Summary** - various statistics summarizing all the
    provided daily flow data, including the long-term mean annual
    discharge (LTMAD; mean of all flow values) and selected percentages
    of LTMAD; a low-flow frequency quantile of a certain duration
    (rolling average) and return period, default to 30Q20 (30-day low
    flow with a 20-year return period); and median, maximum, minimum and
    percentile values of all flow values. Can change some of the
    statistic options below the table, which may also change the plots
    on the right. Note the frequency quantile is fitting using a
    log-Pearson Type III distribution and may not be the best
    distribtuion for your data; see the Low-Flows Probabilities plot on
    the bottom right to see how the points fit against the computed
    curve or the **Analyses \>\> Volume Frequency** tab for further
    analysis
-   **Months Means Plot** - plots monthly means of all flow values with
    the LTMAD (and LTMAD percentages) to show annual flow patterns.
-   **Annual Means Plot** - plots annual means of all flow values to
    show long-term changes in annual flows. Plots with the 10th and 90th
    mean annual flows (can modify percentile in options on bottom left)
    to show which years are more extreme than others.
-   **Annual Means Plot** - plot the

## Settings

There are three main areas in which you can adjust the settings in this
app.

First, as mentioned above, most settings are in the **Data \>\>
Loading** page. These settings apply to every other page in the app. If
you wish to change them, go back to the Data \> Loading page and change
them, then return to the page you were using.

Second, most pages have specific settings to that set of calculations.
You can set these settings on the page in question, they have no bearing
on any other calculations (but do apply to all tabs on a page, e.g.,
Plot and Table).

Finally, plots and tables sometimes have specific settings available by
clicking on the "gear" button on the upper right side. These are like
the page settings, but even more specific, they only apply the plot or
table on that page, on that tab.

## Plots and Tables

All plots and table in this app are interactive.

#### **Plots**

You can hover the mouse over any plot to get more information about the
values presented.

There are also two plotly plots (Data \> Loading \> Daily Flows Plot and
Data \> Available \> Symbols Flow) which can be zoomed in on by clicking
and dragging over a time period.

Finally, in Analyses \> Annual Trends and Analyses \> Volume Frequency,
you can click on specific points on the plot in order to identify years
to omit from the analysis. You can also use the lasso to drag around the
points to select a group.

Note that these last two special types of plots each have reminder
instructions on the page itself to help you out.

**Downloading**. All plots can be downloaded by hovering over the plot
and clicking on the small download button that appears in the upper
right corner. This button looks like a small camera for plotly plots and
a download icon for others. Plots that are relevant for reports also
have an additional Plot download button (small blue button with download
icon saying "Plot", usually in the left hand settings tab). This results
in a higher quality PNG than the other buttons.

#### **Tables**

Tables can be sorted by clicking on the desired column and exported to
"Excel" or "CSV". You can also copy the table and paste it into a
spreadsheet by clicking the "Copy" button.

One table, the HYDAT stations table in Data \> Loading, can also be
filtered by typing filter text into the boxes above each column. This
results in the HYDAT map showing only filtering stations.

Finally there is another special table in Analyses \> Annual Trends.
Once a trend analysis has been computed, the plot of a relevant
statistic can be viewed by clicking on that statistic in the table. As
with other tables you can also sort this table.

## Bookmarking

If you wish to save the status of your analysis so you can return to it
later, you can click on the "Bookmark" button in the lower right corner
of the window.

**Note:** While we have made every effort to ensure bookmarking works
well, it can be slightly unstable and may not work between versions of
this App. We therefore recommend using it as a helper or shortcut tool,
rather than relying on it to save all your work as a complete record.

#### **Local bookmarks**

If you are using `fasstrshiny` from your local computer, this will save
the details into a folder called `shiny_bookmarks` and you'll get a link
like:

`http://127.0.0.1:7493/?_state_id_=a5fa7412c08683dd`

The important part that identifies your bookmark is the id
`a5fa7412c08683dd`.

Next time you run `fasstr_shiny()` you might have a different port
(here, `7493`). But if you paste the end of this link
`?_state_id_=a5fa7412c08683dd` to your new url and hit enter, you will
recover the bookmark. Give the app a couple of seconds to reset all the
inputs.

#### **shinyapps.io bookmarks**

If you are using `fasstrshiny` from the web (shinyapps.io), then when
you click the bookmark button you will get a VERY long URL. We recommend
that you follow the instructions presented to shorten that URL with a
service such as [tinyURL](https://tinyurl.com). That url should then
work to recover the app in future.
