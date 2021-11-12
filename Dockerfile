## Use rstudio installs binaries from RStudio's RSPM service by default, 
## Uses the latest stable ubuntu, R and Bioconductor versions. Created on unbuntu 20.04, R 4.0 and BiocManager 3.12
FROM rocker/rstudio

# required
MAINTAINER Nathan Skene <nskene@imperial.ac.uk>

## Add packages dependencies
RUN apt-get update \
	&& apt-get install -y --no-install-recommends apt-utils \
	&& apt-get install -y --no-install-recommends \
	## Basic deps
	gdb \
	libxml2-dev \
	python3-pip \
	libz-dev \
	liblzma-dev \
	libbz2-dev \
	libpng-dev \
	libgit2-dev \
	## sys deps from bioc_full
	pkg-config \
	fortran77-compiler \
	byacc \
	automake \
	curl \
	## This section installs libraries
	libpcre2-dev \
	libnetcdf-dev \
	libhdf5-serial-dev \
	libfftw3-dev \
	libopenbabel-dev \
	libopenmpi-dev \
	libxt-dev \
	libudunits2-dev \
	libgeos-dev \
	libproj-dev \
	libcairo2-dev \
	libtiff5-dev \
	libreadline-dev \
	libgsl0-dev \
	libgslcblas0 \
	libgtk2.0-dev \
	libgl1-mesa-dev \
	libglu1-mesa-dev \
	libgmp3-dev \
	libhdf5-dev \
	libncurses-dev \
	libbz2-dev \
	libxpm-dev \
	liblapack-dev \
	libv8-dev \
	libgtkmm-2.4-dev \
	libmpfr-dev \
	libmodule-build-perl \
	libapparmor-dev \
	libprotoc-dev \
	librdf0-dev \
	libmagick++-dev \
	libsasl2-dev \
	libpoppler-cpp-dev \
	libprotobuf-dev \
	libpq-dev \
	libperl-dev \
	## software - perl extentions and modules
	libarchive-extract-perl \
	libfile-copy-recursive-perl \
	libcgi-pm-perl \
	libdbi-perl \
	libdbd-mysql-perl \
	libxml-simple-perl \
	libmysqlclient-dev \
	default-libmysqlclient-dev \
	libgdal-dev \
	## new libs
	libglpk-dev \
	## Databases and other software
	sqlite \
	openmpi-bin \
	mpi-default-bin \
	openmpi-common \
	openmpi-doc \
	tcl8.6-dev \
	tk-dev \
	default-jdk \
	imagemagick \
	tabix \
	ggobi \
	graphviz \
	protobuf-compiler \
	jags \
	## Additional resources
	xfonts-100dpi \
	xfonts-75dpi \
	biber \
	libsbml5-dev \
	## qpdf needed to stop R CMD Check warning
	qpdf \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/*

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

RUN install2.r -e \
purrr \
dplyr \
knitr \
data.table \
R.utils \
rmarkdown \
rlang \
ggplot2 \
ggdendro \
cowplot \
stringr \
tibble \
tidyr \
parallel \
magrittr \
grDevices \
usethis \
gridExtra \
devtools

## Install remaining packages from source
RUN Rscript -e 'install.packages(c("BiocManager"))'

## Install Bioconductor packages
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
   libfftw3-dev \
   gcc && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN Rscript -e 'requireNamespace("BiocManager"); BiocManager::install(ask=F);' \
&& Rscript -e 'requireNamespace("BiocManager"); BiocManager::install(c("SNPlocs.Hsapiens.dbSNP144.GRCh37","SNPlocs.Hsapiens.dbSNP144.GRCh38","BiocStyle","EWCE","MungeSumstats","ewceData","limma","GenomeInfoDb","BiocGenerics","BSgenome","S4Vectors"),ask=F)'

    
# Install from GH the following
RUN installGithub.r neurogenomics/orthogene NathanSkene/One2One NathanSkene/MAGMA_Celltyping
