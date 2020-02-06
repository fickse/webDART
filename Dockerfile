FROM openanalytics/r-base

LABEL maintainer "Tobias Verbeke <tobias.verbeke@openanalytics.eu>"

# system libraries of general use
RUN apt-get update && apt-get install -y     sudo     pandoc     pandoc-citeproc     libcurl4-gnutls-dev     libcairo2-dev     libxt-dev     libssl-dev     libssh2-1-dev     libssl1.0.0     libgdal-dev \ 
    libproj-dev

# system library dependency for the euler app
#RUN apt-get update && apt-get install -y #    libmpfr-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the app
RUN R -e "install.packages(c('raster', 'leaflet', 'rgdal'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('rgeos'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('gower'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/webDART
COPY app /root/webDART

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/webDART')"]
