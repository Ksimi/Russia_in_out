# Border crossings with the Russian Federation 
**["Russia_in_out"](
https://ksimi.shinyapps.io/Russia_In_Out/)** is the Shiny app based on the dataset of the border crossings with Russia for the period of **2010 through 2019**.

The official source of these quarterly updated data is the Border Service of the Russian Federation (see the data for [incoming](https://fedstat.ru/indicator/38479) and [outgoing](https://fedstat.ru/indicator/38479) traffic).

The dataset contains 40 quarterly observations of the border crossings with Russian Federation.

The app code is written in **[R](https://www.r-project.org/about.html)** using the **[Shiny](https://shiny.rstudio.com/)** package with some CSS fine-tuning.

The app utilizes the power of the **[Shinydashboard](http://rstudio.github.io/shinydashboard/index.html)**.

![alt text](https://github.com/Ksimi/Russia_in_out/blob/master/Russia_in_out_screenshot.png)

The lines on the dashboard map are built as the [great-circle distance sections](https://en.wikipedia.org/wiki/Great-circle_distance) and are showing the border crossings with Russia as the great-circle sections* on the World map utilizing the [Leaflet](https://leafletjs.com/) library.

###### _*The starting (for outgoing) or ending (for incoming) points for the great-circle sections are placed in the Russian capital, Moscow. This geographical simplification was made due to the facts that: a) No regional split of the Russian border crossings is openly available yet (as of Sep 2020); b) On average more than 50% of all incoming/outgoing air traffic finishes in/originates from Moscow._

The dashboard menu allows a user to select the following parameters:

* Type of border crossing ('into Russia', 'into Russia by population', 'out of Russia', 'crossing balance')
* Number of top countries for showing on the map/populating the top countries list
* Date in quarterly increments
* Map view with some pre-selected layers from the [map provider list for Leaflet](http://leaflet-extras.github.io/leaflet-providers/preview/index.html)

The map and the great circles are **interactive on hover** and show the detailed crossing stats per country.

The dashboard also shows the following data:

* Total border crossings + line chart
* YOY change in border crossings + bar chart
* RUB/EUR currency exchange rate + line chart
* YOY change in FX rate + bar chart
* Top countries list

A user can also start an animated sequence of quarterly views when a “play” button on the date selection is clicked.

**Here is the direct link to the Russian border crossings app on the shinyapp.io:**

https://ksimi.shinyapps.io/Russia_In_Out/
