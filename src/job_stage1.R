######################################################
##############Load and select data####################
library(tidyverse)

folder = "/home/junda/tmp/abf_data/"
fnamecsv = "/home/junda/tmp/abf_data/20171109 data file index.csv"
index = read_csv(fnamecsv)

# select samples
selected = select.samples(index, gene = "A2", buffer = "88")
nselected = nrow(selected)
# Load data
rawdata = abf2.load_in_folder(folder, unlist(selected[, "FileName"]))
sample_list = list()
for (i in 1:nselected)
{
  sample_list[[i]] = rawdata[[i]]$ByChannel
}

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
#plot all sample currents
xyinfo = list()
par(mar=c(2,2,2,2))
for (i in 1:nselected)
{
  xyinfo[[i]] = vclamp.plot_xy_info(rawdata[[i]])
  vclamp.plot_channel(sample_list[[i]], 1, xyinfo[[i]], i)
}
# np x np plot
plot.new()
par(mfrow = c(np, np))
#plot all sample voltages
par(mar=c(2,2,2,2))
for (i in 1:nselected)
{
  vclamp.plot_channel(sample_list[[i]], 2, xyinfo[[i]], i)
}






######################################################
##############remove unneeded episodes################
# e.g.
# remove sample 2, episode 1, 2
sample_list[[2]] = episode.remove_multi(sample_list[[2]], c(1,2))
# remove sample6, episode 1
sample_list[[6]] = episode.remove_multi(sample_list[[6]], c(7))
# plot new sample votages
# np x np plot
plot.new()
par(mfrow = c(np, np))
#plot all voltage plot
par(mar=c(2,2,2,2))
for (i in 1:nselected)
{
  vclamp.plot_channel(sample_list[[i]], 2, xyinfo[[i]], i)
}
