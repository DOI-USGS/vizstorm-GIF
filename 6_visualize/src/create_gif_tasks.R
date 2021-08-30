# create task tables for gif generation - first table is the animation showing
# how gages map to sparklines, second table shows the storm+precip+stage changes
# over time

create_intro_gif_tasks <- function(intro_config, folders, storm_track_cfg, storm_start_date){

  # set up a series of animation frame timesteps (not related to actual time,
  # just describe time within the explanatory animation)
  timestep <- seq_len(intro_config$n_frames)
  has_storm_track <- !is.null(storm_track_cfg$storm_code)

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
        "stage_data = stage_data,",
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
        "timesteps_ind = '2_process/out/timesteps.rds.ind',",
        "storm_points_sf = storm_points_sf,",
        "DateTime = I(NA),",
        "x_pos = legend_x_pos,",
        "y_pos = legend_y_pos,",
        "legend_text_cfg = legend_text_cfg,",
        "intro = TRUE)"
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
        "gage_sites_initial,",
        "gage2spark_fun_%s,"=cur_task$tn,
        "legend_fun_%s,"=cur_task$tn,
        "cities_fun,",
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

create_storm_gif_tasks <- function(timestep, storm_track_cfg, folders){

  has_storm_track <- !is.null(storm_track_cfg$storm_code)

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
      sprintf('gage_sites_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "prep_gage_sites_fun(",
        "stage_data = stage_data,",
        "gage_col_config = gage_color_config,",
        "DateTime = I('%s'))"=format(cur_task$timestep, "%Y-%m-%d %H:%M:%S")
      )
    }
  )

  if(has_storm_track) {
    storm_line_frame <- scipiper::create_task_step(
      step_name = 'storm_line_frame',
      target_name = function(task_name, step_name, ...){
        cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
        sprintf('storm_line_fun_%s', task_name)
      },
      command = function(task_name, ...){
        cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
        psprintf(
          "prep_storm_line_fun(",
          "storm_points_sf = storm_points_sf,",
          "DateTime = I('%s'),"=format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"),
          "storm_line_cfg = storm_line_cfg,",
          "timesteps = timesteps)"
        )
      }
    )
    storm_point_frame <- scipiper::create_task_step(
      step_name = 'storm_point_frame',
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
  }

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
        "stage_data,",
        "site_data,",
        "timestep_ind = '2_process/out/timesteps.rds.ind',",
        "sparkline_placement,",
        "gage_color_config,",
        "I('%s'),"=cur_task$timestep,
        "legend_text_cfg = legend_text_cfg)")
    }
  )

  timeline_frame <- scipiper::create_task_step(
    step_name = 'timeline_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('timeline_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "prep_timeline_fun(",
        "DateTime = I('%s'),"=cur_task$timestep,
        "date_lims = storm_date_limits_local,",
        "timeline_config = timeline_cfg,",
        "spark_config = sparkline_placement,",
        "legend_text_cfg = legend_text_cfg,",
        "local_tz = date_display_tz)")
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
        "timesteps_ind = '2_process/out/timesteps.rds.ind',",
        "storm_points_sf = storm_points_sf,",
        "DateTime = I('%s')," = format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"),
        "x_pos = legend_x_pos,",
        "y_pos = legend_y_pos,",
        "legend_text_cfg = legend_text_cfg)"
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
        "precip_raster_fun_%s,"=cur_task$tn,
        if(has_storm_track) c("storm_line_fun_%s,"=cur_task$tn),
        if(has_storm_track) c("storm_point_fun_%s,"=cur_task$tn),
        "spark_line_%s,"= cur_task$tn,
        "rivers_fun,",
        "gage_sites_fun_%s,"=cur_task$tn,
        "legend_fun_%s,"=cur_task$tn,
        "timeline_fun_%s,"=cur_task$tn,
        "cities_fun,",
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
        if(has_storm_track) c("storm_line_fun_%s,"=cur_task$tn),
        if(has_storm_track) c("storm_point_fun_%s,"=cur_task$tn),
        "legend_fun_%s,"=cur_task$tn,
        "bbox_fun,",
        "cities_fun,",
        "watermark_fun)",
        sep="\n      ")
    }
  )

  step_list <- list(
    sites_frame, if(has_storm_track) storm_line_frame,
    if(has_storm_track) storm_point_frame, precip_frame,
    spark_frame, timeline_frame, legend_frame, gif_frame, gif_test_frame)
  step_list <- step_list[!sapply(step_list, is.null)]
  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=step_list,
    add_complete=FALSE,
    final_steps='gif_frame',
    ind_dir=folders$log)
}

# helper function to sprintf a bunch of key-value (string-variableVector) pairs,
# then paste them together with a good separator for constructing remake recipes
psprintf <- function(..., sep='\n      ') {
  args <- list(...)
  non_null_args <- which(!sapply(args, is.null))
  args <- args[non_null_args]
  argnames <- sapply(seq_along(args), function(i) {
    nm <- names(args[i])
    if(!is.null(nm) && nm!='') return(nm)
    val_nm <- names(args[[i]])
    if(!is.null(val_nm) && val_nm!='') return(val_nm)
    return('')
  })
  names(args) <- argnames
  strs <- mapply(function(template, variables) {
    spargs <- if(template == '') list(variables) else c(list(template), as.list(variables))
    do.call(sprintf, spargs)
  }, template=names(args), variables=args)
  paste(strs, collapse=sep)
}
