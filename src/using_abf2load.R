fname = file.choose()
result = abf2.load(fname)

# result$data[datapoint, channel_number, episode_number]
# 1-500 points of the 1st channel in 3rd episode
result$data[1:500, 1, 3]

# all points of the 2nd channel in 6th episode
result$data[ , 2, 6]

# how many channels?
result$ChannelPerEpisode

# how many episodes?
result$NumOfEpisodes

# what's the name of 2nd channel?
result$ChannelName[2]

# what's the unit of 1st channel?
result$ChannelUnit[1]

# plot all episodes in channel 1 / current
# 1. figure out how many episodes
epi = result$NumOfEpisodes
# 2. loop
# Find maxima and minima, so we can set ylim for a proper plot.
# Hint: all data of chanel 1: result$data[, 1, ]
maxi = max(result$data[, 1, ])
mini = min(result$data[, 1, ])
# determin axis labels and limits
xlabel = "Time / ms"
ylabel = paste("I /", result$ChannelUnit[1])
#last element in SOMETHING: tail(SOMETHING, n = 1) 
xlast = tail(result$X_ms, n = 1)
xlimit = c(0, xlast)
ylimit = c(maxi, mini)
#plot the first line
# use X_ms as x axis                   plot lines      limits of x and y axes         and their labels
plot(result$X_ms, result$data[, 1, 1], type = "l", ylim = ylimit, xlim = xlimit, xlab = xlabel, ylab = ylabel)
for (i in 2:epi)
{
    #plot other episodes
    # use lines to overlap lines to the same plot
    lines(result$X_ms, result$data[, 1, i])
}

# plot all episodes in channel 2 / voltage
epi = result$NumOfEpisodes
maxi = max(result$data[, 2, ])
mini = min(result$data[, 2, ])
xlabel = "Time / ms"
ylabel = paste("V /", result$ChannelUnit[2])
xlast = tail(result$X_ms, n = 1)
xlimit = c(0, xlast)
ylimit = c(maxi, mini)
plot(result$X_ms, result$data[, 2, 1], type = "l", ylim = ylimit, xlim = xlimit, xlab = xlabel, ylab = ylabel)
for (i in 2:epi)
{
  #plot other episodes
  # use lines to overlap lines to the same plot
  lines(result$X_ms, result$data[, 2, i])
}
