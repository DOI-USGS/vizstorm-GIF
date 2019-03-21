create_fetch_snow_tasks <- function(snow_data_yml_name, snow_data_tmp_dir, crop_extent_raster_ind, dates){
  
  # viz_config <- yaml::yaml.load_file('viz_config.yml')
  # dates <- viz_config[["dates"]]
  
  dates_to_fetch <- seq.Date(from = as.Date(dates$start, tz = 'UTC'), 
                             to = as.Date(dates$end, tz = 'UTC'), by = "days")
  years <- unique(format(dates_to_fetch, "%Y"))
  months <- unique(format(dates_to_fetch, "%m"))
  months_str <- month.abb[as.numeric(months)]
  
  # new set of instructions per month/year combination
  tasks <- data.frame(ymd_str = format(dates_to_fetch, "%Y%m%d"), 
                      stringsAsFactors = FALSE) %>% 
    mutate(task_name = sprintf("snow_%s", ymd_str))
  
  download <- scipiper::create_task_step(
    step_name = 'download',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('1_fetch/out/%s.dat.ind', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "fetch_snow_data(",
        "ind_file = target_name,",
        sprintf("ymd_str = I('%s'),", cur_task$ymd_str),
        sprintf("tmp_dir = I('%s'))", snow_data_tmp_dir)
        )
    }
  )
  
  # need data file as target for sc_retrieve in process_snow_rasters to work
  get_data_as_target <- scipiper::create_task_step(
    step_name = 'get_data',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('1_fetch/out/%s.dat', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("gd_get('1_fetch/out/%s.dat.ind')", task_name)
    }
  )
  
  process_rasterize <- scipiper::create_task_step(
    step_name = 'process_rasterize',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('2_process/out/raster_%s.rds.ind', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "process_snow_raster(",
        "ind_file = target_name,",
        sprintf("snow_data_ind = I('1_fetch/out/%s.dat.ind'),", task_name),
        sprintf("snow_data_yml = I('%s'),", snow_data_yml_name),
        sprintf("crop_extent_raster_ind = I('%s'),", crop_extent_raster_ind),
        "proj_str = proj_str)"
      )
    }
  )
  
  get_raster_as_target <- scipiper::create_task_step(
    step_name = 'get_raster',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('2_process/out/raster_%s.rds', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("gd_get('2_process/out/raster_%s.rds.ind')", task_name)
    }
  )
  
  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(download, get_data_as_target, process_rasterize, get_raster_as_target),
    add_complete=FALSE,
    final_steps='process_rasterize',
    ind_dir='1_fetch/log')
}



create_fetch_snow_makefile <- function(makefile, task_plan, remake_file) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c('dplyr', 'scipiper', 'raster'),
    sources=c(
      '1_fetch/src/fetch_snow_data.R',
      '2_process/src/process_snow_rasters.R'),
    file_extensions=c('dat', 'ind'),
    ind_complete=TRUE)
}
