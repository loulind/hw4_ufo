Hi! This is the README for Lou's BIOS 611 HW 4 submission
Steps to run this project:
1. Clone rep into desired directory. 
2. cd into directory and create a file named "data" (copy in nuforc_sightings.csv)
  and create a file named "output" (empty for now)
3. run 
    ```{zsh}
    ./start.sh 
    ```
    Make sure Docker is open and that start.sh has execution permissions. 
    If it doesn't run...
    ```{zsh}
    chmod -x start.sh
    ```
4. Go to http://localhost:8787 (user:rstudio, password:123)
5. Run the whole "source.r" r script. The final outputted PDF should be in the "output" folder