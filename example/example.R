source("src/abf2load.R")

abfdata = abf2.load("example/example.abf")

#access loaded 3-dimensional data (or 2-dimensional in gap-free mode)
rawdata = abfdata$data

#access loaded data by channel
channel1.data = abfdata$ByChannel[[1]]

#access loaded data by episode (not available in gap-free mode)
episode5.data = abfdata$ByEpisode[[5]]

#meta data is loaded to Sections
abfdata$Sections$Protocol$fSecondsPerRun

#some other useful information is also parsed and saved, for example:
abfdata$ChannelName
abfdata$ChannelUnit

abfdata$NumOfEpisodes
abfdata$ChannelsPerEpisode
abfdata$PointsPerChannel

#you can easily plot using the provided X vectors
y_title = paste(abfdata$ChannelNameGuess[[1]], " / ", abfdata$ChannelUnit[[1]])
x_title = "Time / ms"
plot(x = abfdata$X_ms, y = abfdata$ByChannel[[1]]$epi1, type = "l", ylim = c(-15000, 15000), xlab = x_title, ylab = y_title)
for (i in 2:abfdata$NumOfEpisodes)
{
  lines(x = abfdata$X_ms, y = abfdata$ByChannel[[1]][[i]])
}

