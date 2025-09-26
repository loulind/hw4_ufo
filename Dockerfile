FROM rocker/verse:latest

USER root

# Install the stopwords package from CRAN
RUN R -e "install.packages('stopwords', repos='http://cran.rstudio.com/')"