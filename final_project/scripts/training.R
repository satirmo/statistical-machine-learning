# Method: Executes the workflow, tunes on folds and parameter grid, and saves
# the output to a file.
# Parameter wkflow: The workflow.
# Parameter folds: The cross-validation folds.
# Parameter grid: The parameter grid.
# Parameter file_name: The name of the file to which the tuning output will be
# written.
tune_and_save = function(wkflow, folds, grid, file_name){
  wkflow %>%
    tune_grid(resamples=folds, grid=grid) %>%
    saveRDS(file_name)
}