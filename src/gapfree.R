gapfree_pointavg =  function(result, n, time_unit)
{
  if (missing(time_unit))
  {
    intv = result$SampleInterval_s
  }
  else if (time_unit == "ms")
  {
    intv = result$SampleInterval_ms
  }
  else
  {
    intv = result$SampleInterval_s
  }
  l = length(result$data[, "Current"])
  df.len = l %/% n + 1
  df.data = array(dim = c(df.len, 2))
  for (i in 1:df.len)
  {
    p_start = (i - 1) * n + 1
    p_end = p_start + n - 1
    df.data[i, 1] = intv * i * n
    df.data[i, 2] = sum(result$data[, "Current"][p_start:p_end]) / n
  }
  df = data.frame(df.data)
  colnames(df) = c("Time", "Current")
  
  invisible(df)
}