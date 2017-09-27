select.samples = function(dataindex, oocyteNo, gene, buffer)
{
  
  dataindex %>%
    filter(!`Disregard?` %in% 'yes') -> result
  if ( !missing(oocyteNo) )
  {
    result = filter(result, `OocyteNumber` %in% oocyteNo )
  }
  if ( !missing(gene) )
  {
    result = filter(result, `Gene` %in% gene)
  }
  if ( !missing(buffer))
  {
    result = filter(result, `Buffer` %in% buffer)
  }
  result = select(result, FileName, OocyteNumber, Gene, Buffer)
  
  invisible(result)
}

episode.remove = function(l_all_chan, episodes_to_remove)
{
  drops = episode.names(episodes_to_remove)
  result = list()
  for (i in 1:length(l_all_chan))
  {
    result[[i]] = l_all_chan[[i]][, !(names(l_all_chan[[i]]) %in% drops)]
  }
  
  invisible(result)
}

episode.remove_in_channel = function(df_single_chan, episodes_to_remove)
{
  drops = episode.names(episodes_to_remove)
  result = df_single_chan[, !(names(df_single_chan) %in% drops)]
  
  invisible(result)
}

episode.idx = function( episode_names )
{
  result = c()
  for (i in 1:length(episode_names))
  {
    # conver epiX (String) -> X (Number)
    tmp = substring(episode_names[[i]], 4)
    result[i] = as.integer(tmp)
  }
  
  invisible(result)
}

episode.names = function( episode_idx )
{
  result = c()
  for (i in 1:length(episode_idx))
  {
    result[i] = paste("epi", episode_idx[[i]], sep="")
  }
  
  invisible(result)
}

episode.avail = function( df )
{
  result = episode.idx(colnames(df))
}

abf2.find_interval = function(all_channels, sample_interval, current_channel, voltage_channel, target_voltage, voltage_threshold, restriction_div = 3)
{

  check.baseline = function(data, pts_start, pts_end, baseline, threshold)
  {
    # check every available episodes
    avail_epi = episode.avail(data)
    ok = TRUE
    for (i in 1:length(avail_epi))
    {
      interval_min = min(data[pts_start:pts_end, i])
      interval_max = max(data[pts_start:pts_end, i])
      #real_threshold = baseline[i] * threshold
      # some columns might have been removed, thus i does not necessarily correspond to baseline, instead we use idx from avail_epi
      idx = avail_epi[i]
      if ( abs(interval_min - baseline[idx]) > threshold || abs(interval_max - baseline[idx]) > threshold )
      {
        ok = FALSE
        break
      }
    }
    
    invisible(ok)
  }
  
  slope = function(data)
  {
    n = length(data)
    x = 1:n
    linear = lm(data ~ x)
    result = linear$coefficients[[2]]
    
    invisible(result)
  }
  
  total_points = nrow(all_channels[[1]])
  total_interval = total_points %/% sample_interval
  end_interval = total_interval - (total_interval %/% restriction_div)
  
  min_voltage_slope = NA
  min_current_slope = NA
  min_voltage_fluc = NA
  min_current_fluc= NA
  best_start = NA
  best_end = NA
  
  for (i in total_interval:end_interval)
  {
    ptr_start = (i-1) * sample_interval + 1
    ptr_end = ptr_start + sample_interval - 1
    
    # check voltage channel baseline check
    if ( !check.baseline(all_channels[[voltage_channel]], ptr_start, ptr_end, target_voltage, voltage_threshold))
      next
    
    # voltage channel satisfies target voltage
    # now calculate fluctuations. Can we simply sum local fluctuation?
    tmp = .0
    tmp_voltage = .0
    tmp_current = .0
    tmp_voltage_slope = .0
    tmp_current_slope = .0
    avail_epi = episode.avail(all_channels[[voltage_channel]])
    for (j in 1:length(avail_epi))
    {
      tmp = abs(slope(all_channels[[voltage_channel]][ptr_start:ptr_end, j]))
      if (tmp > tmp_voltage_slope)
        tmp_voltage_slope = tmp
      tmp = abs(slope(all_channels[[current_channel]][ptr_start:ptr_end, j]))
      if (tmp > tmp_current_slope)
        tmp_current_slope = tmp
      tmp_voltage = tmp_voltage + fluctuation.local(all_channels[[voltage_channel]][ptr_start:ptr_end, j])
      # corresponding current channel should present
      tmp_current = tmp_current + fluctuation.local(all_channels[[current_channel]][ptr_start:ptr_end, j])
    }
    if (is.na(best_start))
    {
      min_current_slope = tmp_current_slope
      min_voltage_slope = tmp_voltage_slope
      min_current_fluc = tmp_current
      min_voltage_fluc = tmp_voltage
      best_start = ptr_start
      best_end = ptr_end
    }
    # Criteria: 1. At least one better slope. 2. Smaller fluctuations in both channels
    else if ( ((tmp_current_slope < min_current_slope) || (tmp_voltage_slope < min_voltage_slope)) && ((tmp_current < min_current_fluc) && (tmp_voltage < min_voltage_fluc)) )
    {
      min_current_fluc = tmp_current
      min_voltage_fluc = tmp_voltage
      min_current_slope = tmp_current_slope
      min_voltage_slope = tmp_voltage_slope
      best_start = ptr_start
      best_end = ptr_end
    }
  }
  
  invisible(c(best_start,best_end))
}

abf2.get_meanVA = function(all_channels, interval)
{
  
  va = data.frame()
  lo = interval[1]
  hi = interval[2]
  for (i in 1:length(all_channels))
  {
    epi_avail = episode.avail(all_channels[[i]])
    for (j in 1:length(epi_avail))
    {
      m = mean(all_channels[[i]][lo:hi, j])
      va[i, epi_avail[j]] = m
    }
  }
  epi_names = episode.names(1:ncol(va))
  colnames(va) <- epi_names
  
  invisible(va)
}

abf2.plot_meanVA = function(df, current_channel = 1, voltage_channel = 2)
{
  Current = unlist(df[current_channel, ])
  Voltage = unlist(df[voltage_channel, ])
  plot(Voltage, Current, type="b")
}

abf2.get_meanChannel = function(alldatachannel, allintervals, chan_id)
{
  df = data.frame()
  for (i in 1:length(alldatachannel))
  {
    lo = allintervals[i, "Start"]
    hi = allintervals[i, "End"]
    epi_avail = episode.avail(alldatachannel[[i]][[chan_id]])
    for (j in 1:length(epi_avail))
    {
        m = mean(alldatachannel[[i]][[chan_id]][lo:hi, j])
        df[i, epi_avail[j]] = m
    }
  }
  epi_names = episode.names(1:ncol(df))
  colnames(df) <- epi_names
  
  invisible(df)
}

abf2.combine_meanChannel = function(voltagemeans, currentmeans)
{
  result = data.frame()
  n = colSums(!is.na(voltagemeans))
  for (i in 1:ncol(voltagemeans))
  {
    result[i, 1] = mean(voltagemeans[, i], na.rm = TRUE)
    result[i, 2] = sd(voltagemeans[, i], na.rm = TRUE) / sqrt(n[[i]])
    result[i, 3] = mean(currentmeans[, i], na.rm = TRUE)
    result[i, 4] = sd(currentmeans[, i], na.rm = TRUE) / sqrt(n[[i]])
  }
  colnames(result) <- c("Voltage", "SEM Voltage", "Current", "SEM Current")
  
  invisible(result)
}

# Calculate fluctuation in the interval between sample_start and sample_end COLUMN-WISE 

fluctuation.integrate = function(data)
{
  m = mean(data)
  n = length(data)
  
  y = c()
  y[1]= data[1]
  for (i in 2:n)
  {
    y[i] = y[i-1] + (data[i] - m)
  }
  
  invisible(y)
}

fluctuation.local = function(data)
{
  n = length(data)
  
  y = fluctuation.integrate(data)
  
  x = 1:n
  linear = lm(data ~ x)
  b = linear$coefficients[[1]]
  slope = linear$coefficients[[2]]
  
  #Evaluate fluctuation within the whole local data set
  Fn = 0.0
  for (i in 1:n)
  {
    yn = b + slope * i
    delta = (y[i] - yn)^2.0
    Fn = Fn + delta
  }
  Fn = sqrt(Fn / n)
  
  invisible(Fn)
}

# Simple plotting functions
abf2.xy_info = function(abfdata, time_unit)
{
  result = list()
  #Channel names and units
  chan_name = abfdata$ChannelNameGuess
  chan_unit = abfdata$ChannelUnit
  #X axis
  if (missing(time_unit))
  {
    result$x = abfdata$X_ticks
    xmax = max(abfdata$X_ticks)
    xmin = min(abfdata$X_ticks)
    result$xlimit = c(xmin, xmax)
    result$xlabel = "Time / ticks"
  }
  else if (time_unit == "ms")
  {
    result$x = abfdata$X_ms
    xmax = max(abfdata$X_ms)
    xmin = min(abfdata$X_ms)
    result$xlimit = c(xmin, xmax)
    result$xlabel = "Time / ms"
  }
  else if (time_unit == "s")
  {
    result$x = abfdata$X_s
    xmax = max(abfdata$X_s)
    xmin = min(abfdata$X_s)
    result$xlimit = c(xmin, xmax)
    result$xlabel = "Time / s"
  }
  else
  {
    result$x = abfdata$X_ticks
    xmax = max(abfdata$X_ticks)
    xmin = min(abfdata$X_ticks)
    result$xlimit = c(xmin, xmax)
    result$xlabel = "Time / ticks" 
  }
  result$ylabel = c()
  for (i in 1:length(chan_name))
    result$ylabel[i] = paste(chan_name[i], "/", chan_unit[i])
  
  invisible(result)
}

abf2.plot_channel = function(all_channels, chan_id, xy_info, plottitle)
{
  data = all_channels[[chan_id]]
  x = xy_info$x
  xlimit = xy_info$xlimit
  xlabel = xy_info$xlabel
  ymin = min(data)
  ymax = max(data)
  ylimit = c(ymin, ymax)
  ylabel = xy_info$ylabel[[chan_id]]
  plot(x, data[, 1], type = "l", ylim = ylimit, xlim = xlimit, xlab = xlabel, ylab = ylabel)
  if (!missing(plottitle))
  {
    title(plottitle)
  }
  # plot available episodes only
  for (i in 2:ncol(data))
  {
    lines(x, data[, i])
  }
}

abf2.inspect_interval = function(all_channels, interval, xy_info)
{
  lo = interval[1]
  hi = interval[2]
  #plot.new()
  par(mfrow=c(1,2))
  for (i in 1:length(all_channels))
  {
    abf2.plot_channel(all_channels, i, xy_info)
    abline(v = lo, lty = 3)
    abline(v = hi, lty = 3)
  }
}

abf2.voltage_setting = function(abfdata)
{
  init = abfdata$Sections$EpochPerDAC["fEpochInitLevel", 2]
  incr = abfdata$Sections$EpochPerDAC["fEpochLevelInc", 2]
  n = abfdata$NumOfEpisodes
  result = seq(from = init, by = incr, length.out = n)
  
  invisible(result)
}
