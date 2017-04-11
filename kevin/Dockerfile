FROM rocker/r-base
MAINTAINER Kevin Son <eunwoo.son@sdstate.edu>

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev

RUN R -e 'install.packages(c("devtools"))'
RUN R -e 'devtools::install_github("trestletech/plumber")'
RUN R -e 'install.packages("ggplot2", dep = TRUE)'
RUN R -e 'install.packages(c("data.table","RCurl", "earth", "mda", "faraway", "lattice", "caret"))'

COPY ./ /usr/local/src/plumber/
WORKDIR /usr/local/src/plumber/
VOLUME /usr/local/src/plumber/

EXPOSE 8000
CMD ["Rscript", "/lib/baseLib.R"]
CMD ["Rscript", "runServer.R"]
