calc_scan_line_period <- function(intervals) {
  temp <- list()
  
  for (i in 1:length(intervals)) {
    temp[[length(temp) + 1]] = list(intervals[[i]][1], 1)
    temp[[length(temp) + 1]] = list(intervals[[i]][2], -1)
  }
  
  temp <- temp[
    order(
      sapply(temp, function(x) x[[1]]),
      sapply(temp, function(x) x[[2]])
    )
    ]
  
  result = temp[[length(temp)]][[1]] - temp[[1]][[1]]
  
  indicator = 0
  
  for (i in 1:length(temp)) {
    if (indicator == 0 & i != 1 & i != length(temp)) {
      result = result - (temp[[i]][[1]] - temp[[i - 1]][[1]]);
    }
    
    indicator = indicator + temp[[i]][[2]];
  }
  
  return (as.numeric(result, units="days"))
}