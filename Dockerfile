FROM rocker/shiny:4.0.0
RUN apt-get update -y &&  apt-get -y install git libeigen3-dev libpq-dev libgdal-dev libudunits2-dev
## Download and install library
RUN R -e "install.packages(c('packrat'),  dependencies = TRUE, repos='http://cran.rstudio.com/')" && \
    R -e "install.packages(c('lubridate'),  dependencies = TRUE)" && \
    R -e "install.packages(c('tidyr'),  dependencies = TRUE)" && \
    R -e "install.packages(c('stringr'),  dependencies = TRUE)" && \
    R -e "install.packages(c('reshape2'),  dependencies = TRUE)" && \
    R -e "install.packages(c('zoo'),  dependencies = TRUE)" && \
    R -e "install.packages(c('jsonlite'),  dependencies = TRUE)" && \
    R -e "install.packages(c('dplyr'),  dependencies = TRUE)" && \
    R -e "install.packages(c('ggplot2'),  dependencies = TRUE)" && \
    R -e "install.packages(c('sf'),  dependencies = TRUE)" && \
    R -e "install.packages(c('ggrepel'),  dependencies = TRUE)" && \
    R -e "install.packages(c('memoise'),  dependencies = TRUE)" && \
    rm -fr /tmpR*
#RUN git clone https://github.com/ruxandra-valcu/medie-covid-judete.git /srv/shiny-server/medie-covid-judete || exit 1
RUN mkdir /srv/shiny-server/medie-covid-judete
COPY . /srv/shiny-server/medie-covid-judete
