fname = file.choose()
result = abf2.load(fname)

# result$data[datapoint, channel_number, episode_number]
# 1-500 points of the 1st channel in 3rd episode
result$data[1:500, 1, 3]

# all points of the 2nd channel in 6th episode
result$data[ , 2, 6]

# because time interval is constant, we can use arbitrary X axis when ploting
# how many points per channel/episode?
pts = result$PointsPerChannel
# make an arbitrary X axis
x = 1:pts
# plot all points of the 1st channel in 1st episode
#plot(x, result$data[, 1, 1])

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
# 2. make an X axis
pts = result$PointsPerChannel
x = 1:pts
# 3. loop
# Find maxima and minima, so we can set ylim for a proper plot.
# Hint: all data of chanel 1: result$data[, 1, ]
maxi = max(result$data[, 1, ])
mini = min(result$data[, 1, ])
plot(x, result$data[, 1, 1], type = "l", ylim = c(maxi, mini), xlim = c(1, pts), xlab = "Time", ylab = "I / nA")
for (i in 2:epi)
{
    #plot sth...
    # use lines to overlap lines to the same plot
    lines(x, result$data[,1,i])
}

# plot all episodes in channel 2 / voltage
# 1. figure out how many episodes
epi = result$NumOfEpisodes
# 2. make an X axis
pts = result$PointsPerChannel
x = 1:pts
# 3. loop
# Find maxima and minima, so we can set ylim for a proper plot.
# Hint: all data of chanel 1: result$data[, 2, ]
maxi = max(result$data[, 2, ])
mini = min(result$data[, 2, ])
#Plot 1st graph,          type is line      set y limit         set x limit       x label        y label
plot(x, result$data[, 2, 1], type = "l", ylim = c(maxi, mini), xlim = c(1, pts), xlab = "Time", ylab = "V / mV")
#loop through the rest episodes
for (i in 2:epi)
{
  # use lines to overlap lines to the same plot
  lines(x, result$data[, 2, i])
}
