# create task table for gif generation



create_gif_tasks <- function(timestep_ind, folders, storm_cfg){


  message('subsetting times to simplify for now')
  timestep <- readRDS(sc_retrieve(timestep_ind))

  cfgs <- c('a') # for now, just use one config, since > 1 results in duplication of input files
  #,'b') # dummy placement for different configurations; will eventually be configurations that hold information about size, aspect, ect...

  tasks <- tidyr::crossing(timestep, cfgs) %>%
    unite(task_name, cfgs, timestep, sep = '_', remove = F) %>%
    mutate(date_hour = strftime(timestep, format = '%Y%m%d_%H', tz = 'UTC'),
           task_name = sprintf("%s_%s", cfgs, date_hour))

  # function to sprintf a bunch of key-value (string-variableVector) pairs, then
  # paste them together with a good separator for constructing remake recipes
  psprintf <- function(..., sep='\n      ') {
    args <- list(...)
    strs <- mapply(function(string, variables) {
      spargs <- if(string == '') list(variables) else c(list(string), as.list(variables))
      do.call(sprintf, spargs)
    }, string=names(args), variables=args)
    paste(strs, collapse=sep)
  }

  point_frame <- scipiper::create_task_step(
    step_name = 'point_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('storm_point_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_storm_point_fun(storm_points_sf, I('%s'), hurricane_col)", cur_task$timestep)
    }
  )

  precip_frame <- scipiper::create_task_step( # not sure why this is called "frame".
    step_name = 'precip_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('precip_raster_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_precip_fun(precip_rasters, precip_bins, I('%s'))", cur_task$timestep)
    }
  )

  datetime_frame <- scipiper::create_task_step(
    step_name = 'datetime_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('datetime_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_datetime_fun(I('%s'))", cur_task$timestep)
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
        "create_storm_frame(",
        "png_file=target_name,",
        "config=storm_frame_config,",
        "view_fun,",
        "basemap_fun,",
        "rivers_fun,",
        "precip_raster_%s,"=cur_task$tn,
        "storm_line_fun,",
        "storm_point_%s,"= cur_task$tn,
        "datetime_%s,"= cur_task$tn,
        "legend_fun,",
        "watermark_fun)",
        #"streamdata_%s,"= cur_task$tn,
        sep="\n      ")
    }
  )

  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(point_frame, precip_frame, datetime_frame, gif_frame),
    add_complete=FALSE,
    final_steps='gif_frame',
    ind_dir=folders$log)
}
