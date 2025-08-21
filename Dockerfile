# Use an official R image as a base
FROM rocker/shiny:latest

# Install Linux dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libssh2-1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libgit2-dev \
    libsodium-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinythemes', 'ggplot2', \
 'oro.nifti', 'neurobase', 'shinycssloaders', 'shinyjs', 'fields', \
 'sass', 'bslib', 'reshape', 'gridExtra', 'shinyBS', 'shinyscreenshot'))"

# Install devtools for GitHub installations if needed
RUN R -e "install.packages('devtools')"

# Install EffeX directly from GitHub
RUN R -e "devtools::install_github('halleeshearer/EffeX')"

# Set the working directory in the container
WORKDIR /srv/shiny-server

# Copy the Shiny app's files into the container
COPY . /srv/shiny-server

# Expose port to access the app
EXPOSE 3838

# Run the app using Rscript
CMD ["Rscript", "app.R"]

