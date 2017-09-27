###############################
#Now we calculate intervals and plot them
allintervals = data.frame()
for (i in 1:nselected)
{
  #read voltage setting and xyinfo from raw data
  voltage = abf2.voltage_setting(rawdata[[i]])
  xyinfo = abf2.xy_info(rawdata[[i]], "ticks")
  #find interval
  #                    all your data       intervalsize  voltage setting
  t = abf2.find_interval(alldatachannel[[i]], 150, 1, 2, voltage, 15.0)
  #                             current and voltage channel id   voltage threshold
  # inspect the selectd interval
  abf2.inspect_interval(alldatachannel[[i]], t, xyinfo)
  title(i)
  allintervals[i, 1] = t[1]
  allintervals[i, 2] = t[2]
}
colnames(allintervals) <- c("Start", "End")





######################################################
#########Manual select intervals if needed###########
#The 4th sample need to set intervals manually
sampleid = 4
allintervals[sampleid, "Start"] = 7501
allintervals[sampleid, "End"] = 7550
####Plot to see if suitable###
xyinfo = abf2.xy_info(rawdata[[sampleid]], "ticks")
abf2.inspect_interval(alldatachannel[[sampleid]], allintervals[sampleid, ], xyinfo)




#########################################################
########Calculate mean V/A values for all samples########
voltagemeans = abf2.get_meanChannel(alldatachannel, allintervals, 2)
currentmeans = abf2.get_meanChannel(alldatachannel, allintervals, 1)

combined = abf2.combine_meanChannel(voltagemeans, currentmeans)
#add this data frame to processed data
idx = length(proc_meanVA) + 1
proc_meanVA[[idx]] = combined
#set the name of processed data
proc_name[[idx]] = "BlahBlah"
