FROM rocker/shiny:4.4.1

ARG R_PACKAGES="arrow hrbrthemes patchwork rsconnect tidyverse tsibble"

EXPOSE 8080

RUN install2.r "$R_PACKAGES" && \
    rm -fr /tmp/downloaded_packages

WORKDIR /home/shinyusr
COPY app.r deploy.r entsoe-cz.parquet .

CMD Rscript -e "shiny::runApp('.', port = 8080)"

