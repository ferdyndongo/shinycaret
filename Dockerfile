FROM rocker/r-ubuntu:20.04
LABEL maintainer="USER <ferdyndongo@gmail.com>"
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl14-openssl-dev \
    libv8-3.14-dev \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*
COPY Rprofile.site /etc/R
RUN install.r remotes
COPY DESCRIPTION .
RUN Rscript -e "remotes::install_deps()"
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home/app
COPY /R/ .
RUN chown app:app -R /home/app
USER app
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app')"]

