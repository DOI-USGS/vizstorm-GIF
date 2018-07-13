# create task tables for gif generation - first table is the animation showing
# how gages map to sparklines, second table shows the storm+precip+stage changes
# over time

create_intro_gif_tasks <- function(n_timesteps, folders){

  # set up a series of animation frame timesteps (not related to actual time,
  # just describe time within the explanatory animation)
  timestep <- seq_len(n_timesteps)

  # aspect/resolution configuration placeholder. see same code in
  # create_storm_gif_tasks - we should extract this into shared location once we
  # get multiple configurations really going
  cfgs <- c('a')

  tasks <- tidyr::crossing(timestep, cfgs) %>%
    unite(task_name, cfgs, timestep, sep = '_', remove = F) %>%
    mutate(date_hour = sprintf('00_%03d', timestep),
           task_name = sprintf("%s_%s", cfgs, date_hour))

  gage2spark <- scipiper::create_task_step(
    step_name = 'gage2spark',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('gage2spark_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "prep_gage2spark_fun(",
        "timestep=I(%d))" = cur_task$timestep
      )
    }
  )

  gif_frame <- scipiper::create_task_step(
    step_name = 'gif_frame',
    target_name = function(task_name, step_name, ...){
      file.path(folders$tmp, sprintf('gif_frame_%s.png', task_name))
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "create_animation_frame(",
        "png_file=target_name,",
        "config=intro_frame_config,",
        "view_fun,",
        "basemap_fun,",
        "rivers_fun,",
        "storm_sites_initial,",
        "storm_line_fun,",
        "gage2spark_%s," = cur_task$tn,
        "legend_fun,",
        "watermark_fun)"
      )
    }
  )

  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(gage2spark, gif_frame),
    add_complete=FALSE,
    final_steps='gif_frame',
    ind_dir=folders$log)
}

create_storm_gif_tasks <- function(timestep_ind, folders){

  timestep <- readRDS(sc_retrieve(timestep_ind))

  cfgs <- c('a') # for now, just use one config, since > 1 results in duplication of input files
  #,'b') # dummy placement for different configurations; will eventually be configurations that hold information about size, aspect, ect...

  tasks <- tidyr::crossing(timestep, cfgs) %>%
    unite(task_name, cfgs, timestep, sep = '_', remove = F) %>%
    mutate(date_hour = strftime(timestep, format = '%Y%m%d_%H', tz = 'UTC'),
           task_name = sprintf("%s_%s", cfgs, date_hour))

  sites_frame <- scipiper::create_task_step(
    step_name = 'sites_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('storm_sites_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_storm_sites_fun(storm_data, 'viz_config.yml', I('%s'))", format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"))
    }
  )

  point_frame <- scipiper::create_task_step(
    step_name = 'point_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('storm_point_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_storm_point_fun(storm_points_sf, I('%s'), hurricane_cols)", format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"))
    }
  )

  precip_frame <- scipiper::create_task_step( # not sure why this is called "frame".
    step_name = 'precip_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('precip_raster_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_precip_fun(precip_rasters, precip_bins, I('%s'))", format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"))
    }
  )

  spark_frame <- scipiper::create_task_step(
    step_name = 'spark_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('spark_line_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_spark_line_fun(storm_data, 'viz_config.yml', I('%s'))", cur_task$timestep)
    }
  )

  datetime_frame <- scipiper::create_task_step(
    step_name = 'datetime_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('datetime_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_datetime_fun(I('%s'), datetime_placement)", format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"))
    }
  )

  gif_frame <- scipiper::create_task_step(
    step_name = 'gif_frame',
    target_name = function(task_name, step_name, ...){
      file.path(folders$tmp, sprintf('gif_frame_%s.png', task_name))
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "create_animation_frame(",
        "png_file=target_name,",
        "config=storm_frame_config,",
        "view_fun,",
        "basemap_fun,",
        "ocean_name_fun,",
        "rivers_fun,",
        "precip_raster_fun_%s,"=cur_task$tn,
        "storm_sites_fun_%s,"=cur_task$tn,
        "storm_line_fun,",
        "storm_point_fun_%s,"= cur_task$tn,
        "spark_line_%s,"= cur_task$tn,
        "legend_fun,",
        "datetime_fun_%s,"=cur_task$tn,
        "watermark_fun)",
        #"streamdata_%s,"= cur_task$tn,
        sep="\n      ")
    }
  )

  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(sites_frame, point_frame, precip_frame,
                    spark_frame, datetime_frame, gif_frame),
    add_complete=FALSE,
    final_steps='gif_frame',
    ind_dir=folders$log)
}

# helper function to sprintf a bunch of key-value (string-variableVector) pairs,
# then paste them together with a good separator for constructing remake recipes
psprintf <- function(..., sep='\n      ') {
  args <- list(...)
  strs <- mapply(function(string, variables) {
    spargs <- if(string == '') list(variables) else c(list(string), as.list(variables))
    do.call(sprintf, spargs)
  }, string=names(args), variables=args)
  paste(strs, collapse=sep)
}
