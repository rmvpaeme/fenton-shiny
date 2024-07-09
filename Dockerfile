# Example shiny app docker file
# https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/
# docker build  --platform linux/amd64 --no-cache -t fenton .
# docker run  --platform linux/amd64 -p 3838:3838 -v ./data:data fenton
# docker tag fenton rmvpaeme/fenton:0.3
# docker push rmvpaeme/fenton:0.3

# get shiny server and R from the rocker project
FROM rocker/shiny:4.3.0

# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev libmagic-dev \
    texlive-latex-extra \
    lmodern && \
    /rocker_scripts/install_pandoc.sh && \
    install2.r rmarkdown

ENV RSTUDIO_PANDOC=/usr/lib/rstudio/bin/pandoc
    
#RUN apt-get update && apt-get install -y pandoc

# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "tidyverse", \
              "shinythemes", \
              "DT", \
              "rmarkdown", \
              "shinyTime" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2023-12-04"\
          )'


# copy the app directory into the image
COPY ./* /srv/shiny-server/

# run app
CMD ["/usr/bin/shiny-server"]
