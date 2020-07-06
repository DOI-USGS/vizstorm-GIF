create_intro_gif_makefile <- function(makefile, task_plan, remake_file) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c('dplyr', 'scipiper'),
    sources=c(
      '6_visualize/src/prep_spark_line_fun.R',
      '6_visualize/src/prep_gage2spark_fun.R',
      '6_visualize/src/create_animation_frame.R'),
    file_extensions=c('feather','ind'))
}

create_storm_gif_makefile <- function(makefile, task_plan, remake_file) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c('dplyr', 'scipiper'),
    sources=c(
      '6_visualize/src/create_animation_frame.R',
      '6_visualize/src/prep_gage_sites_fun.R',
      '6_visualize/src/prep_storm_point_fun.R',
      '6_visualize/src/prep_precip_fun.R',
      '6_visualize/src/prep_spark_line_fun.R',
      '6_visualize/src/prep_timeline_fun.R',
      '6_visualize/src/combine_animation_frames.R'),
    file_extensions=c('feather','ind'))
}

# Outro makefile is weird because the only change needed is using
# the functions from the last frame. Don't need a whole task table
create_outro_gif_makefile <- function(target_name, template_fn, timesteps) {
  last_task <- strftime(tail(timesteps, 1), format = '%Y%m%d_%H', tz = 'UTC')
  readLines(template_fn) %>%
    whisker::whisker.render(data = list(last_task = last_task)) %>%
    writeLines(con = target_name)
}
