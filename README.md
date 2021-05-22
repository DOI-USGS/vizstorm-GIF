# vizstorm-GIF

## How to work on this project

This repository has a master branch with all the important code, and it has a flood-specific branch containing the .ind and build/status files for each flood. The naming convention for flood branches is `Stormname-year`, e.g., `Matthew-2016`.

The master branch on GitHub ahould't contain a viz_config.yml or any shared-cache files (`*.ind`, `build/status/*.yml`), so the .gitignore on the master branch should ignore those files. The flood branches *should* contain these files, so remember to edit the .gitignore to permit them when you first start a storm.

None of these branches have a lib/cfg/gd_config.yml file, but you always need one. To create one, first identify the Drive folder where data are stored for a given branch, then run `scipiper::gd_config(config_file=target_name, folder=I("drive-folder-id"))` (or just copy and modify a similar such file from another project or branch).

### Working on a new storm branch

To set up the branch:
1. Create a new folder and RStudio project just for this storm. Clone the GitHub repository into this project. Because it's a separate folder, you'll be able to add storm-specific data and image files to this folder/project without overwriting files from other storms. We recommend the folder and project name `vizstorm-GIF-[stormname-year]`.
1. Create and switch to a new storm-specific branch. Use the branch naming convention `Stormname-year`, e.g., `Matthew-2016`.
1. Edit .gitignore so that the following files are *not* ignored: `*.ind`, `build/status/*.yml`, `viz_config.yml`, `lib/cfg/gd_config.yml`, `2_process/src/filter_sites_custom.R`.
1. Add a lib/cfg/gd_config.yml. See other storm branches and lib/cfg/gd_config-example.yml for examples.
1. Add a viz_config.yml. See other storm branches and viz_config-example.yml for examples.
1. Add a 2_process/src/filter_sites_custom.R file and function. See other storm branches and 2_process/src/filter_sites_custom-example.R for examples.
1. Create a shared Google Drive folder for the cache in https://drive.google.com/drive/u/3/folders/1dOxz5NWbRizK9KgKDGjVlN3XZtutSOz4.
1. Create or update your lib/cfg/gd_config.yml file by calling `scipiper::gd_config(config_file="lib/cfg/gd_config.yml", folder="drivefolderid")` with the new drivefolderid value from the Drive folder you just created. Note that there is an existing `gd_config-example.yml` at that location. You could choose to copy that and use it to create the `lib/cfg/gd_config.yml` instead of the function above.
1. The first time you build a target or run a function that uses `gd_put()` or `gd_get()`, you'll be prompted in a browser to log in to your Google Account. This will create a an .httr-oauth file locally, which should continue to be .gitignored.

To build the storm GIF:
1. Make your best guess about an appropriate `bbox` in viz_config.yml.
1. Make your best guess about a good site list in 2_process/src/filter_sites_custom.R (this can be broad, at the cost of needing to pull and plot more NWIS data in early iterations). It can be helpful to see the site numbers on the figures with the spark lines as you iterate through site selection. One way to do this is to add this `text(full_poly$x[1]-strwidth("5555"), full_poly$y[1], labels=site)` to the `prep_spark_line_fun` where the stage shapes are being added to the plot. Just delete when you're done iterating. 
1. Build 6_storm_gif_tasks.yml with `scmake('6_storm_gif_tasks.yml')`.
1. Choose a GIF frame to build to preview the vizzy setup. The dates and therefore exact filenames will differ, but you may want to build one gif_frame and one gif_TEST_frame (which shows the bbox) to start. For example, you might run `scmake('6_visualize/tmp/gif_frame_a_20170825_00.png', '6_storm_gif_tasks.yml')` and/or `scmake('6_visualize/tmp/gif_TEST_frame_a_20170825_00.png', '6_storm_gif_tasks.yml')`
1. If jumping directly to building a gif_frame, you may find that some steps (especially fetches from the internet) require retries. It may boost your overall build efficiency to `scmake` some of these steps individually rather than relying on the full `scipiper` chain, mainly because of known issues https://github.com/USGS-R/scipiper/issues/70 and https://github.com/USGS-R/scipiper/issues/47.
1. When git committing your changes, make sure that any `*.ind` file you commit (new or modified) has a matching `build/status/*.yml` file in your commit. If it doesn't, run `scipiper:::YAMLify_build_status('yourindfile.ind')`. The need to do this reflects the scipiper issues noted just above.
1. Iterate through choices of bbox, site list, ocean and city names, etc. until you have the gif_frames (pngs) looking the way you want.
1. Build the animated GIF with `scmake('6_visualize/out/animation_a.gif')`. It often takes a long time to build all the pngs, so if you want to preview an animated GIF with only some frames, consider specifying a subset of frames with the `task_names` argument to `combine_animation_frames()`.
1. Once you're happy with the way the animated GIF looks, you will need to build the intro frames. Do this by making sure `task_names=NULL` for the target `6_visualize/log/6_intro_gif_tasks.ind` in `6_visualize.yml`. Run `scmake('6_visualize/log/6_intro_gif_tasks.ind')` to build the intro frames. Then, run `scmake('6_visualize/out/animation_a.gif')` to incorporate the intro frames into the full animated GIF (note that you might need to add `force = TRUE` if the file already exists, or if `6_visualize/log/6_intro_gif_tasks.ind` or `6_visualize/log/6_storm_gif_tasks.ind` already exist and the PNGs need to be updated).
1. Build and push the final animated GIF with `scmake('6_visualize/out/animation_a.gif.ind')`. This is the one case in the vizstorm project where the .ind file does not get built before the data (.gif) file. The unusal ordering allows you to iterate on the .gif many times before needing to push to Drive. But don't forget to push when you're done so others can see!

Use `scmake()` to build the intermediate or final products you want to look at right away. 

Most changes you'll make will be storm specific. If you make changes that should apply to all other (or future) storms, pull or copy those changes over to the master branch, taking care not to copy storm-specific files. See the next section on working on the master branch.

### Working on the vizstorm-GIF master branch

Because this branch has no shared cache, it's recommended that if you want to develop code for the master branch, you either pull/copy code changes from other branches, or you create your own Drive cache (folder) for test

Setup to work on the master branch:
1. Create a new folder and RStudio project just for this master branch. Clone the GitHub repository into this project. Because it's a separate folder, you'll be able to add storm-specific data and image files to this folder/project without overwriting files from other storms.
1. Make sure you're on the `master` branch.
1. Edit .gitignore so that the following files *are* ignored: `*.ind`, `build/status/*.yml`, `viz_config.yml`, `lib/cfg/gd_config.yml`, `2_process/src/filter_sites_custom.R`. (This step will likely be done once by one person for all of us.)
1. Add a viz_config.yml. See other storm branches and viz_config-example.yml for examples.
1. Add a 2_process/src/filter_sites_custom.R file and function. See other storm branches and 2_process/src/filter_sites_custom-example.R for examples.
1. Create *your own* Google Drive folder for the cache. With your own Drive folder, you can experiment with whichever storms, sites, etc. are most appropriate to your code development.
1. Create or update your lib/cfg/gd_config.yml file by calling `scipiper::gd_config(config_file="lib/cfg/gd_config.yml", folder="drivefolderid")` with the new drivefolderid value from the Drive folder you just created.
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
