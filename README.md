# vizstorm-GIF

## How to work on this project

This repository has a master branch with all the important code, and it has a flood-specific branch containing the .ind and build/status files for each flood. The naming convention for flood branches is `Stormname-noaacode`, e.g., `Matthew-al142016`.

The master branch on GitHub ahould't contain a viz_config.yml or any shared-cache files (`*.ind`, `build/status/*.yml`), so the .gitignore on the master branch should ignore those files. The flood branches *should* contain these files, so remember to edit the .gitignore to permit them when you first start a storm.

None of these branches have a lib/cfg/gd_config.yml file, but you always need one. To create one, first identify the Drive folder where data are stored for a given branch, then run `scipiper::gd_config(config_file=target_name, folder=I("drive-folder-id"))` (or just copy and modify a similar such file from another project or branch).

### Working on a new storm branch

To set up the branch:
1. Create a new RStudio project just for this storm. Clone the GitHub repository into this project. Because it's a separate folder, you'll be able to add storm-specific data and image files to this folder/project without overwriting files from other storms.
1. Create and switch to a new storm-specific branch. Use the branch naming convention `Stormname-noaacode`, e.g., `Matthew-al142016`.
1. Edit .gitconfig so that the following files are *not* ignored: `*.ind`, `build/status/*.yml`, `viz_config.yml`, `lib/cfg/gd_config.yml`, `2_process/src/filter_sites_custom.R`.
1. Add a viz_config.yml. See other storm branches for examples.
1. Add a 2_process/src/filter_sites_custom.R file and function. See other storm branches for examples.
1. Create a shared Google Drive folder for the cache in https://drive.google.com/drive/u/1/folders/169KSGMULk6eJxBTsIDQ0l9z65CueZEbF. Run `scipiper::gd_config(config_file=target_name, folder=I("drive-folder-id"))`.
1. The first time you build a target or run a function that uses `gd_put()` or `gd_get()`, you'll be prompted in a browser to log in to your Google Account. This will create a an .httr-oauth file locally, which should continue to be .gitignored.

Most changes you'll make will be storm specific. If you make changes that should apply to all other (or future) storms, pull or copy those changes over to the master branch, taking care not to copy storm-specific files. See the next section on working on the master branch.

### Working on the vizstorm-GIF master branch

Because this branch has no shared cache, it's recommended that if you want to develop code for the master branch, you either pull/copy code changes from other branches, or you create your own Drive cache (folder) for test

Setup to work on the master branch:
1. Create a new RStudio project just for this master branch. Clone the GitHub repository into this project. Because it's a separate folder, you'll be able to add storm-specific data and image files to this folder/project without overwriting files from other storms.
1. Make sure you're on the `master` branch.
1. Edit .gitconfig so that the following files *are* ignored: `*.ind`, `build/status/*.yml`, `viz_config.yml`, `lib/cfg/gd_config.yml`, `2_process/src/filter_sites_custom.R`. (This step will likely be done once by one person for all of us.)
1. Add a viz_config.yml. See other storm branches for examples.
1. Add a 2_process/src/filter_sites_custom.R file and function. See other storm branches for examples.
1. Create *your own* Google Drive folder for the cache and run `scipiper::gd_config(config_file=target_name, folder=I("drive-folder-id"))`. With your own Drive folder, you can experiment with whichever storms, sites, etc. are most appropriate to your code development.
1. The first time you build a target or run a function that uses `gd_put()` or `gd_get()`, you'll be prompted in a browser to log in to your Google Account. This will create a an .httr-oauth file locally, which should continue to be .gitignored.


## Docker

There is a Docker image built specially for this project using the USGS-VIZLAB/vizstorm-GIF-docker code repo on GitHub and the aapplingusgs/vizstorm-gif image repo on Docker Hub.

### Setting up to use Docker

To use the docker image, you should create a file called `docker-compose.yml` in the main directory (it'll be git ignored) with these contents:

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

### Launching the Docker container

Open a bash shell. <win7>In Windows 7, run Docker Quickstart Terminal <em>as administrator, with VPN off.</em></win7>

Launch a container from the docker image:
```
docker-compose up
```

Next, in your browser, navigate to `localhost:8787` <win7>In Windows 7, replace `localhost` with the specific IP given by `docker-machine ip default`. Still affix the colon and port number 8787, e.g., `http://192.168.99.100:8787`.</win7> Login with username=`rstudio` and password=`rstudio`.

In the RStudio Files pane, click on your project directory and then on the project .proj file to open your project as an RStudio project.
