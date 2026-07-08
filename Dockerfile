FROM rocker/r-ver:4.4.1

WORKDIR /

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        git \
        r-cran-ggplot2 \
        r-cran-jsonlite \
        r-cran-remotes \
        libxml2-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libcairo2-dev \
        libxt-dev \
        libfontconfig1-dev \
        libfreetype6-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev \
        libuv1-dev \
        pandoc \
    && rm -rf /var/lib/apt/lists/*

COPY . /tmp/RamEx

RUN rm -f /tmp/RamEx/src/*.o /tmp/RamEx/src/*.so /tmp/RamEx/src/*.dll
RUN R -e "remotes::install_deps('/tmp/RamEx', dependencies = TRUE, upgrade = 'never')"

# RamEx ships only a Windows Makevars; force the plain TinyThread backend
# for RcppParallel instead of relying on this build's TBB support.
RUN echo 'PKG_CPPFLAGS += -DRCPP_PARALLEL_USE_TBB=0' > /tmp/RamEx/src/Makevars

RUN R CMD INSTALL /tmp/RamEx \
    && R -e "library(RamEx)" \
    && rm -rf /tmp/RamEx
