FROM rocker/verse:latest

USER root

RUN R -e "install.packages(c('RSQlite','DBI'))"