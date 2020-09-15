# Border crossings with Russian Federation 
"Russia_in_out" is the Shiny app based on the dataset of the border crossings with Russia for the period of **2010 through 2019**.

The official source of the quarterly updated data is the Border Service of the Russian Federation (see the data for [incoming](https://fedstat.ru/indicator/38479) and [outgoing](https://fedstat.ru/indicator/38479) traffic).

The dataset contains 40 quarterly observations of the border crossings with Russia with the traffic type additionally split into air, car, rail, ship and walk subsets.

The app code is written in **[R](https://www.r-project.org/about.html)** using the **[Shiny](https://shiny.rstudio.com/)** package with some CSS fine-tuning.

The app utilizes the power of the **[Shinydashboard](http://rstudio.github.io/shinydashboard/index.html)** and is organized into **two different dashboard types**:

## **Great Circles dashboard**

_The Great Circles dashboard map based on the idea of the [great-circle distances](https://en.wikipedia.org/wiki/Great-circle_distance) and is showing the border crossings with Russia as the great-circle sections* on the World map utilizing the [Leaflet](https://leafletjs.com/) library._

###### _The starting (for outgoing) or ending (for incoming) points for the great-circle sections are placed in the Russian capital, Moscow. This geographical simplification was made due to the facts that: a) No regional split of the Russian border crossings is openly available yet (as of Sep 2020). b) On average more than 50% of all incoming/outgoing air traffic finishes in/originates from Moscow._

The Great Circles dashboard menu allows a user to select the following parameters:

* Type of border crossing (into Russia, into Russia – by population, out of Russia, crossing balance)
* Number of top countries for showing on the map/populating the top countries list
* Date in quarterly increments
* Map view with some pre-selected layers from the map provider list for Leaflet

The map itself is **interactive on hover** and shows the detailed crossing stats per country.

The Great Circles dashboard also shows the following data:

* Total border crossings + line chart
* YOY change in border crossings + bar chart
* RUB/EUR currency exchange rate + line chart
* YOY change in FX rate + bar chart
* Top countries list

A user can also start an animated sequence of quarterly views when a “play” button on the date selection is clicked.

## Mini Pie Charts dashboard

The Mini Pie Charts dashboard is using the leaflet.minicharts package for showing the pie charts with the traffic type split by country on the World map.

The Mini Pie Chars dashboard menu allows a user to select the following parameters:

* Type of border crossing (into Russia, out of Russia)
* Pie chart mode (total and by transport type with multi-selection possible)
* Transport type (multi-selection possible)
* Pie chart sizes
* Map view as per map provider list for Leaflet
* Date in quarterly increments (with the date controls on the map itself)

A user can also start an animated sequence of quarterly pie chart views when a “play” button on the map is clicked.
