# Fenton NICU growth-curve Shiny app
# https://github.com/rmvpaeme/fenton-shiny

FROM rocker/shiny:4.4.2

# system libraries required by tidyverse / readxl
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libmagic-dev \
  && rm -rf /var/lib/apt/lists/*

# R packages (pinned to a Posit Package Manager snapshot for reproducibility)
RUN R -e 'install.packages(c(\
              "shiny", \
              "tidyverse", \
              "readxl", \
              "bslib", \
              "shiny.i18n", \
              "shinycssloaders", \
              "scales", \
              "DT" \
            ), \
            repos="https://packagemanager.posit.co/cran/__linux__/jammy/2024-12-01"\
          )'

# copy app (www/, data/, app.R, …); see .dockerignore for exclusions
COPY . /srv/shiny-server/

CMD ["/usr/bin/shiny-server"]
