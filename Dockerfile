# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:4.2.0

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libsodium-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# install renv & restore packages
## renv.lock file
COPY ./renv.lock .
RUN Rscript -e 'renv::restore()'

# copy necessary files
## app folder
COPY /shiny-app ./app

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
