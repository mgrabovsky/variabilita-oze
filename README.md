# Renewables production variability dashboard

This is a simple Shiny app for exploration of the variability of renewable energy production throughout the year in Czechia.

It can be accessed [via shinyapps.io](https://mgrabovsky.shinyapps.io/variabilita-oze/).

## Container

The app can also be run locally, either directly from R using the `shiny` package, or as a container. A [Dockerfile](./Dockerfile) is included to make this as easy as possible:

    $ podman build -t shiny-variabilita-oze .
    $ podman run --rm -itp 8080:8080 shiny-variabilita-oze .

Once the container is running, the app can be access at <http://localhost:8080>.

## Licence

The code is licensed under the terms of [Apache 2.0 licence](http://www.apache.org/licenses/).

