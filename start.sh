echo "Starting Docker container..."
cd ~/bios611/hw4_ufo/

docker build . --platform=linux/amd64 -t r-ufo-analysis
docker run\
  --platform=linux/amd64\
  -v $(pwd):/home/rstudio/work\
  -v $HOME/.ssh:/home/rstudio/.ssh\
  -v $HOME/.gitconfig:/home/rstudio/.gitconfig\
  -e PASSWORD=123\
  -p 8787:8787 -it r-ufo-analysis

