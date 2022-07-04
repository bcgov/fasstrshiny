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

#### **Station Information**

Click this toggle to view and/or change the station name (used for
titles throughout the app) and the upstream basin area, in square
kilometres (used for area-based runoff calculations). When using HYDAT
data, these will default to the provided HYDAT information, but can be
modified as necessary.

#### **Data Options**

Below the Load Station Data and Station Information (can click the
toggles to close), explore data and date settings to customize the
station information, filter your dates, adjust the types of data you're
using or handle missing dates differently (also see the section below,
**Settings**).

### Availability and Screening

Now that you've loaded your data, you'll want to take a look and make
sure there are no surprises. Click on **Data \>\> Availability &
Screening**.

This section contains several different tools for exploring your data:

1.  Symbols Flow Plot - Interactive plotly plot for exploring Symbols in
    your data.
2.  Symbols Summary Plot - Summary of the symbols in your data
3.  Data Availability Plot - Summary of missing data
4.  Data Summary Plot/Table - Summary of flow data statistics

**Filtering** and **Missing Dates**. If you find any date periods that
are problematic and which you would like to omit from your analysis, go
to **Data \>\> Loading**, and adjust the dates under "Date Filtering".
Similarly, if you have a lot of missing data, you can specify how these
values should be handled by fasstr functions under "Handling Missing
Dates".

### Going Further

Now that you have your data and have filtered and adjusted the settings
as required, you're ready to start analysis.

Explore the other pages on the side bar. Each page holds a different set
of calculations, generally in both plot and data form.

The **Analyses** are more complex and computationally intensive
calculations.

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
