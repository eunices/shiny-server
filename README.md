# shiny-server

## Deploying apps
1. Connect using gcloud. Copy and paste from VM's console.
```bash
gcloud beta compute ssh --zone "asia-southeast1-b" "instance-2" --project "astute-rig-271004"
```
2. `sudo git pull` in `/srv/shiny-server/` folder

## Set up shiny server with GCP
https://github.com/paeselhz/RStudio-Shiny-Server-on-GCP

More info:
https://cloudyr.github.io/googleComputeEngineR/articles/shiny-app.html 

## rmarkdown
Use `rmarkdown::render`.

## Shiny and rmarkdown
https://shiny.rstudio.com/articles/interactive-docs.html 
https://rmarkdown.rstudio.com/lesson-3.html

Use `rmarkdown::run("public-apps/covid19/covid19-sg.rmd")`.

## Information
http://34.87.161.142:3838/