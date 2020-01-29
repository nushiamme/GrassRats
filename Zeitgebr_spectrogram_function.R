spectrogram_fun <- function(var,
                        data,
                        period_range = c(hours(16), hours(32)),
                        resample_rate = 1 / mins(15),
                        FUN = cwt_spectrogram,
                        ...){
  
  n_val = var__ = id = . = .N = t0 = .SD = NULL
  
  var_of_interest <- deparse(substitute(var))
  regular_data <- resample(data, var_of_interest, resample_rate)
  
  data.table::setnames(regular_data, var_of_interest, "var__")
  
  reg_data_nval <- regular_data[, .(n_val = length(unique(var__))),
                                by = c(data.table::key(regular_data))]
  
  invalid <- reg_data_nval[n_val < 2, id]
  if(length(invalid) > 0)
    warning(sprintf("Removing individuals that have only one unique value for `val`:\n%s",paste(invalid, sep="\n")))
  
  regular_data <- regular_data[!(id %in% invalid)]
  out <- regular_data[, FUN(var__,
                            period_range = period_range,
                            sampling_rate = resample_rate,
                            ...),
                      by = c(data.table::key(data))]
  
  time_origin <- data[, .(t0 = .SD[1,t]),by=id]
  out[, t:= out[time_origin, on="id"][,t +t0]]
  out
}
