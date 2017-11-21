######################################################
#######Now we calculate intervals and plot them#######
intervals = data.frame()
for (i in 1:nselected)
{
  #read voltage setting
  voltage = vclamp.voltage_setting(rawdata[[i]])
  #find interval
  #                    all your data       intervalsize  voltage setting
  t = vclamp.best_interval(sample_list[[i]], 150, 1, 2, voltage, 15.0)
  #                             current and voltage channel id   voltage threshold
  # inspect the selectd interval
  vclamp.plot_interval(sample_list[[i]], t, xyinfo[[i]])
  title(i)
  intervals[i, 1] = t[1]
  intervals[i, 2] = t[2]
}
colnames(intervals) <- c("Start", "End")






######################################################
#########Manual select intervals if needed############
#The 4th sample need to set intervals manually
sampleid = 4
intervals[sampleid, "Start"] = 7501
intervals[sampleid, "End"] = 7550
####Plot to see if suitable###
t = c(intervals[sampleid, "Start"], intervals[sampleid, "End"])
vclamp.plot_interval(sample_list[[sampleid]], t, xyinfo[[sampleid]])






######################################################
######Calculate mean V/A values for all samples#######
voltagemeans = vclamp.channel_mean(sample_list, intervals, 2)
currentmeans = vclamp.channel_mean(sample_list, intervals, 1)
combined = vclamp.report_channel_mean(voltagemeans, currentmeans)

#add this data frame to processed data
idx = length(proc_meanVA) + 1
proc_meanVA[[idx]] = combined
#set the name of processed data
proc_name[[idx]] = "BlahBlah"
