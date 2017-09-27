####Stage 1.####
#Load and select data

library(tidyverse)

folder = "/home/junda/Projects/20170923 ALMT anion influx/"
fnamecsv = "/home/junda/Projects/20170923 ALMT anion influx/20170923 data file index.csv"
index = read_csv(fnamecsv)

#select samples
selected = select.samples(index, oocyteNo = c(8,9,10,11,12,38,39), gene = "A2", buffer = "B")
nselected = nrow(selected)

#determine plotting area
np = 0
for (i in 1:nselected)
  if (i^2 >= nselected)
  {
    np = i
    break
  }
# np x np plot
plot.new()
par(mfrow = c(np, np))
#plot all current plot
rawdata = abf2.loadfolder(folder, unlist(selected[, "FileName"]))
alldatachannel = list()
par(mar=c(2,2,2,2))
for (i in 1:nselected)
{
  alldatachannel[[i]] = rawdata[[i]]$ByChannel
  xyinfo = abf2.xy_info(rawdata[[i]])
  abf2.plot_channel(alldatachannel[[i]], 1, xyinfo, i)
}

# np x np plot
plot.new()
par(mfrow = c(np, np))
#plot all voltage plot
rawdata = abf2.loadfolder(folder, unlist(selected[, "FileName"]))
alldatachannel = list()
par(mar=c(2,2,2,2))
for (i in 1:nselected)
{
  alldatachannel[[i]] = rawdata[[i]]$ByChannel
  xyinfo = abf2.xy_info(rawdata[[i]])
  abf2.plot_channel(alldatachannel[[i]], 2, xyinfo, i)
}


###########################################################
#remove unneeded episodes
#according to the plots, now remove sample 2, episode 1, 2
alldatachannel[[2]] = episode.remove(alldatachannel[[2]], c(1,2))
#also remove sample6, episode 1
alldatachannel[[6]] = episode.remove(alldatachannel[[6]], c(1))
# np x np plot
plot.new()
par(mfrow = c(np, np))
#plot all voltage plot
rawdata = abf2.loadfolder(folder, unlist(selected[, "FileName"]))
par(mar=c(2,2,2,2))
for (i in 1:nselected)
{
  xyinfo = abf2.xy_info(rawdata[[i]])
  abf2.plot_channel(alldatachannel[[i]], 2, xyinfo, i)
}


