FROM ubuntu:24.04

## for apt to be noninteractive
ENV DEBIAN_FRONTEND=noninteractive
ENV DEBCONF_NONINTERACTIVE_SEEN=true

## Install wget
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    wget

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    git

## Install git
RUN  apt-get update \
  && apt-get install -y --no-install-recommends \
    git

## Install gpg
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    gnupg

## R repo
RUN apt-get update -qq \
  && apt-get install -y  --no-install-recommends \
    software-properties-common \
    dirmngr \
    curl

RUN curl -O https://cran.rstudio.com/src/base-prerelease/R-devel.tar.gz \
  && tar -xzvf R-devel.tar.gz

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
  && add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

RUN sed -i 's/^Types: deb$/Types: deb deb-src/' /etc/apt/sources.list.d/ubuntu.sources

RUN cat /etc/apt/sources.list.d/ubuntu.sources

RUN apt update \
  && apt build-dep -y --no-install-recommends r-base

RUN cd R-devel \
  && ./configure \
    --prefix=/opt/R/R-devel \
    --enable-R-shlib \
    --enable-memory-profiling \
    --with-blas \
    --with-lapack \
  && make \
  && make install

RUN ln -s /opt/R/R-devel/bin/R /usr/local/bin/R \
  && ln -s /opt/R/R-devel/bin/Rscript /usr/local/bin/Rscript

RUN mkdir /etc/R \
  && curl https://tutorials.inbo.be/installation/administrator/admin_install_r/Rprofile.site -o .Rprofile

COPY Rprofile.site /.Rprofile

RUN Rscript -e 'install.packages("pak")'

RUN Rscript -e 'pak::pkg_install("devtools", dependencies = TRUE)'

RUN Rscript -e 'pak::pkg_install("ropensci/git2rdata@append", dependencies = TRUE)'

ENTRYPOINT ["/bin/bash"]
