FROM rocker/rstudio:3.6.0
#FROM rocker/r-ubuntu:18.04 

# required
MAINTAINER Nathan Skene <nskene@imperial.ac.uk>

#RUN apt-get update \
 #   && apt-get install -y --no-install-recommends \
  #  libxml2-dev # add any additional libraries you need

#RUN apt-get update -qq \
#	&& apt-get install -t -y --no-install-recommends \
#    liblzma-dev \
#    libcurl4-openssl-dev \
#    libxml2-dev


RUN apt-get update -qq && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
    apt-transport-https \
    build-essential \
    curl \
    gfortran \
    libatlas-base-dev \
    libbz2-dev \
    libcairo2 \
    libcurl4-openssl-dev \
    libicu-dev \
    liblzma-dev \
    libpango-1.0-0 \
    libpangocairo-1.0-0 \
    libpcre3-dev \
    libtcl8.6 \
    libtiff5 \
    libtk8.6 \
    libx11-6 \
    libxml2-dev \
    libxt6 \
    locales \
    tzdata \
    zlib1g-dev

# Install MAGMA
RUN apt-get update \
    && apt-get install procps -y \
    && apt-get install wget zip unzip libxt-dev -y \
    #&& wget https://ctg.cncr.nl/software/MAGMA/prog/magma_v1.0.zip \
    && wget https://ctg.cncr.nl/software/MAGMA/prog/archive/magma_v1.07.zip \
    #&& unzip magma_v1.0.zip \
    && unzip magma_v1.07.zip \
    #add permissions for docker image to run magma
    && chmod +x magma \
    && cp magma /usr/local/bin/ 

# Install required libraries -- using prebuild binaries where available
#RUN apt-get update && apt-get install -y \
#    git \
    #r-cran-litter 
   # r-cran-gert \
   # r-cran-usethis \
   # r-cran-devtools \
   # r-cran-gh \
   # r-cran-git2r

# Install additional R packages from CRAN (on top of the ones 
# pre-built as r-cran-*)
#RUN install.r devtools
RUN Rscript -e "install.packages('devtools',repos = 'http://cran.us.r-project.org')"
    
# Install from GH the following
RUN installGithub.r NathanSkene/One2One neurogenomics/EWCE NathanSkene/MAGMA_Celltyping
