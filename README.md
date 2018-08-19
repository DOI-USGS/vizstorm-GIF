# vizstorm-GIF

## Setting up to use Docker

There is a Docker image built specially for this project using the USGS-VIZLAB/vizstorm-GIF-docker code repo on GitHub and the aapplingusgs/vizstorm-gif image repo on Docker Hub. To use it, you should create a file called `docker-compose.yml` in the main directory (it'll be git ignored) with these contents:

```
version: "3"
services:
  vizzy-dev-mode:
    image: aapplingusgs/vizstorm-gif:latest
    ports:
      - "8787:8787"
    volumes:
      - "YOUR_PATH/vizstorm-GIF:/home/rstudio/vizstorm-GIF"
```

where `YOUR_PATH` for Alison on Windows 7 is `/D_DRIVE/APAData/Github/DS Vizzies`, and for Lindsay on Windows 10 would probably be `d:/LRCData/R/vizstorm-gif` - the important difference is that Alison had to use `D_DRIVE`, which is the name VirtualBox gave to the D drive when Alison designated it as a shared folder. You will need to designate vizstorm-GIF or some parent folder as a shared folder within your Docker system for this to work.

## Launching the Docker container

Open a bash shell. <win7>In Windows 7, run Docker Quickstart Terminal <em>as administrator, with VPN off.</em></win7>

Launch a container from the docker image:
```
docker-compose up
```

Next, in your browser, navigate to `localhost:8787` <win7>In Windows 7, replace `localhost` with the specific IP given by `docker-machine ip default`. Still affix the colon and port number 8787, e.g., `http://192.168.99.100:8787`.</win7> Login with username=`rstudio` and password=`rstudio`.

In the RStudio Files pane, click on your project directory and then on the project .proj file to open your project as an RStudio project.
