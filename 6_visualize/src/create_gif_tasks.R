# create task table for gif generation



create_gif_tasks <- function(timestep_ind, folders, view_ind, basemap_ind, storm_line_ind, storm_cfg){

  message('subsetting times to simplify for now')
  timestep <- readRDS(sc_retrieve(timestep_ind))[1:3]

  cfgs <- c('a') # for now, just use one config, since > 1 results in duplication of input files
  #,'b') # dummy placement for different configurations; will eventually be configurations that hold information about size, aspect, ect...

  tasks <- tidyr::crossing(timestep, cfgs) %>%
    unite(task_name, cfgs, timestep, sep = '_', remove = F) %>%
    mutate(date_hour = strftime(timestep, format = '%Y%m%d-%H', tz = 'UTC'),
           task_name = gsub("-","_", sprintf("%s-%s", cfgs, date_hour)))
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
      sprintf('6_vizprep/out/storm_point_[%s].rds.ind', cur_task$date_hour)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_storm_point_fun(target_name, '2_process/out/storm_points_interp.rds.ind')", cur_task$date_hour)
    }
  )

  get_point_frame <- scipiper::create_task_step(
    step_name = 'get_point_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('6_vizprep/out/storm_point_[%s].rds', cur_task$date_hour)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("gd_get('6_vizprep/out/storm_point_[%s].rds.ind')", cur_task$date_hour)
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
        "'%s',"=view_ind,
        "'%s',"=basemap_ind,
        "'%s',"=storm_line_ind,
        "'6_vizprep/out/storm_point_[%s].rds.ind')"=cur_task$date_hour,
        #"'6_vizprep/out/precip_raster_[%s].rds.ind',"=cur_task$date_hour,
        #"'6_vizprep/out/streamdata_[%s].rds.ind')"=cur_task$date_hour,
        sep="\n      ")
    }
  )

  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(point_frame, get_point_frame, gif_frame),
    add_complete=FALSE,
    ind_dir=folders$log)
}
