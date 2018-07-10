# create task table for gif generation



create_gif_tasks <- function(timestep_ind, folders, storm_line_ind, storm_points_ind, precip_rasters_ind, stream_data_ind, cfg){

  timestep <- readRDS(sc_retrieve(timestep_ind))

  cfgs <- c('a','b','c') # dummy placement for different configurations; will eventually be configurations that hold information about size, aspect, ect...

  tasks <- tidyr::crossing(timestep, cfgs) %>%
    unite(task_name, cfgs, timestep, sep = '_', remove = F) %>%
    mutate(task_name = gsub(' ', '_', task_name))

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


  gif_frame <- scipiper::create_task_step(
    step_name = 'gif_frame',
    target_name = function(task_name, step_name, ...){
      file.path(folders$tmp, sprintf('gif_frame_%s.png', task_name))
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "create_storm_frame(",
        "png_file=target_name",
        "timestep=I('%s'),"=cur_task$timestep,
        "config=storm_frame_config",
        "view_polygon",
        "focus_geoms",
        "secondary_geoms",
        "storm_line_ind='%s',"=storm_line_ind,
        "storm_points_ind='%s',"=storm_points_ind,
        "precip_rasters_ind='%s',"=precip_rasters_ind,
        "stream_data_ind='%s',"=stream_data_ind,
        sep="\n      ")
    }
  )

  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(gif_frame),
    add_complete=FALSE,
    ind_dir=folders$log)

}
