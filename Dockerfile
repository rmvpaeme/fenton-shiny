# Example shiny app docker file
# https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/

# get shiny server and R from the rocker project
FROM rocker/shiny:4.3.0

# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev libmagic-dev
  

# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "tidyverse", \
              "shinythemes", \
              "DT", \
              "shinyTime" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2023-12-04"\
          )'


# copy the app directory into the image
COPY ./* /srv/shiny-server/

# run app
CMD ["/usr/bin/shiny-server"]
