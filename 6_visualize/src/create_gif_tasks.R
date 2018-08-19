# create task tables for gif generation - first table is the animation showing
# how gages map to sparklines, second table shows the storm+precip+stage changes
# over time

create_intro_gif_tasks <- function(intro_config, folders, storm_start_date){

  # set up a series of animation frame timesteps (not related to actual time,
  # just describe time within the explanatory animation)
  timestep <- seq_len(intro_config$n_frames)

  # aspect/resolution configuration placeholder. see same code in
  # create_storm_gif_tasks - we should extract this into shared location once we
  # get multiple configurations really going
  cfgs <- c('a')

  tasks <- tidyr::crossing(timestep, cfgs) %>%
    unite(task_name, cfgs, timestep, sep = '_', remove = F) %>%
    mutate(date_hour = sprintf('01_%03d', timestep),
           task_name = sprintf("%s_%s", cfgs, date_hour))

  gage2spark_frame <- scipiper::create_task_step(
    step_name = 'gage2spark_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('gage2spark_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "prep_gage2spark_fun(",
        "intro_config = intro_config,",
        "timestep=I(%d)," = cur_task$timestep,
        "storm_data = storm_data,",
        "site_data = site_data,",
        "gage_color_config = gage_color_config,",
        "timestep_ind = '2_process/out/timesteps.rds.ind',",
        "spark_config = sparkline_placement,",
        "DateTime = I('%s'))" = format(storm_start_date, "%Y-%m-%d %H:%M:%S")
      )
    }
  )

  legend_frame <- scipiper::create_task_step(
    step_name = 'legend_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('legend_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "prep_legend_fun(",
        "precip_bins = precip_bins,",
        "legend_styles = legend_styles,",
        "storm_points_sf = storm_points_sf,",
        "DateTime = I(NA),",
        "x_pos = legend_x_pos,",
        "y_pos = legend_y_pos)"
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
        "ocean_name_fun,",
        "rivers_fun,",
        "storm_sites_initial,",
        "storm_line_fun,",
        "gage2spark_fun_%s,"=cur_task$tn,
        "legend_fun_%s,"=cur_task$tn,
        "watermark_fun)"
      )
    }
  )

  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(gage2spark_frame, legend_frame, gif_frame),
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
      psprintf(
        "prep_storm_sites_fun(",
        "storm_data = storm_data,",
        "gage_col_config = gage_color_config,",
        "DateTime = I('%s'))"=format(cur_task$timestep, "%Y-%m-%d %H:%M:%S")
      )
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
      psprintf(
        "prep_storm_point_fun(",
        "storm_points_sf = storm_points_sf,",
        "DateTime = I('%s'),"=format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"),
        "hurricane_cols = hurricane_cols)"
      )
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
      psprintf(
        "prep_spark_line_fun(",
        "storm_data,",
        "site_data,",
        "timestep_ind = '2_process/out/timesteps.rds.ind',",
        "sparkline_placement,",
        "gage_color_config,",
        "I('%s'))"=cur_task$timestep)
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

  legend_frame <- scipiper::create_task_step(
    step_name = 'legend_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('legend_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "prep_legend_fun(",
        "precip_bins = precip_bins,",
        "legend_styles = legend_styles,",
        "storm_points_sf = storm_points_sf,",
        "DateTime = I('%s')," = format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"),
        "x_pos = legend_x_pos,",
        "y_pos = legend_y_pos)"
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
        "legend_fun_%s,"=cur_task$tn,
        "datetime_fun_%s,"=cur_task$tn,
        "watermark_fun)",
        #"streamdata_%s,"= cur_task$tn,
        sep="\n      ")
    }
  )
  gif_test_frame <- scipiper::create_task_step(
    step_name = 'gif_test_frame',
    target_name = function(task_name, step_name, ...){
      file.path(folders$tmp, sprintf('gif_TEST_frame_%s.png', task_name))
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
        "storm_line_fun,",
        "storm_point_fun_%s,"= cur_task$tn,
        "legend_fun_%s,"=cur_task$tn,
        "bbox_fun,",
        "datetime_fun_%s,"=cur_task$tn,
        "watermark_fun)",
        sep="\n      ")
    }
  )
  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(sites_frame, point_frame, precip_frame,
                    spark_frame, datetime_frame, legend_frame, gif_frame, gif_test_frame),
    add_complete=FALSE,
    final_steps='gif_frame',
    ind_dir=folders$log)
}

# helper function to sprintf a bunch of key-value (string-variableVector) pairs,
# then paste them together with a good separator for constructing remake recipes
psprintf <- function(..., sep='\n      ') {
  args <- list(...)
  templates <- if(is.null(names(args))) {
    rep('', length(args))
  } else {
    names(args)
  }
  strs <- mapply(function(string, variables) {
    spargs <- if(string == '') list(variables) else c(list(string), as.list(variables))
    do.call(sprintf, spargs)
  }, string=templates, variables=args)
  paste(strs, collapse=sep)
}
