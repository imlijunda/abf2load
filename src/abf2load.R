# Simple support for C typed I/O
ctypes.supported = c("char", "uchar", "int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "float", "double", "string", "unused")
# and their corresponding size (in bytes)
ctypes.bytesize = c(1, 1, 1, 1, 2, 2, 4, 4, 8, 4, 8, 0, 0)
sizeof = list()
# get size by sizeoff[["uint16"]]
sizeof = setNames(ctypes.bytesize, ctypes.supported)

.abf2.struct.read = function ( fp, struct.def, fp.offset=0 )
{
    result = list()
    seek(fp, where=fp.offset, origin = "start")
    byteread = 0
    byteskip = 0
    for ( i in 1:length(struct.def$field))
    {
        fd = as.character(struct.def$field[i])
        tp = as.character(struct.def$ctype[i])
        ss = as.integer(struct.def$ssize[i])
        sz = sizeof[[tp]]
        if ( startsWith(tp, "int") | (tp == "char") )
        {
            # read a signed integer
            result[[fd]] = readBin(fp, what = "integer", size = sz, signed = TRUE)
            byteread = byteread + sz
        }
        else if ( (tp == "float") | (tp == "double") )
        {
            # read a floating point numbers
            result[[fd]] = readBin(fp, what = "numeric", size = sz)
            byteread = byteread + sz
        }
        else if ( startsWith(tp, "uint") | (tp == "uchar") )
        {
            # read an unsigned integer, this gets a little bit tricky because R does not support unsigned long
            if ( sz == 4 )
            {
                # use numeric to store an unsigned long
                b1 = readBin(fp, what = "integer", size = 2, signed = FALSE)
                b2 = readBin(fp, what = "integer", size = 2, signed = FALSE)
                if ( .Platform$endian == "little" )
                {
                    result[[fd]] = 65536.0 * b2 + b1
                } else {
                    result[[fd]] = 65536.0 * b1 + b2
                }
            }
            else
            {
                result[[fd]] = readBin(fp, what = "integer", size = sz, signed = FALSE)
            }
            byteread = byteread + sz
        }
        else if ( tp == "string" )
        {
            if ( ss != 0 )
            {
                # do readChar read nul ending?
                tmpstr = readChar(fp, ss)
                result[[fd]] = tmpstr
                byteread = byteread + length(charToRaw(tmpstr))
            } else {
                # string character size is not defined, read into a raw byte vector with until first 0 byte
                tmpstr = c()
                idx = 0
                while ( TRUE )
                {
                    tmpchar = readBin(fp, what = "integer", size = 1, signed = FALSE)
                    idx = idx + 1
                    tmpstr[idx] = tmpchar
                    byteread = byteread + 1
                    if ( tmpchar == 0 )
                    {
                        break
                    }
                }
                result[[fd]] = tmpstr
            }
        }
        else if ( tp == "unused" )
        {
            # simply skip ss bytes of data
            seek(fp, where = ss, origin = "current")
            byteskip = byteskip + ss
            result[[fd]] = 0
        }
    }
    # result$byte.read = byteread
    # result$byte.skip = byteskip
    result$byte.total = byteread + byteskip

    return(result)
}

.abf2.struct.size = function ( struct.def )
{
    size = 0
    for ( i in 1:length(struct.def$field))
    {
        tp = as.character(struct.def$ctype[i])
        ss = as.integer(struct.def$ssize[i])
        size = size + sizeof[[tp]] + ss
    }

    return(size)
}

.abf2.load.section = function ( fp, sect.info, sect.def )
{
  ptr = sect.info$uBlockIndex * .ABF2.BlockSize
  if ( ptr == 0 )
    return(NULL)
  n = sect.info$llNumEntries
  m = length(sect.def$field)
  tmparr = array(dim = c(m, n))
  for (i in 1:n)
  {
    tmp = .abf2.struct.read(fp, sect.def, ptr)
    ptr = ptr + tmp$byte.total
    for (j in 1:m)
    {
      tmparr[j, i] = tmp[[j]]
    }
  }
  result = data.frame(tmparr, row.names = sect.def$field)
  colnames(result) = c(1:n)
  
  return(result)
}

.abf2.parseStrSect = function ( rawdata )
{
  result = list()
  offset = 45
  idx = 0
  st_ptr = offset
  for (i in offset:length(rawdata))
  {
    if (rawdata[[i]] == 0)
    {
      idx = idx + 1
      result[[idx]] = rawToChar(rawdata[st_ptr:(i-1)])
      st_ptr = i + 1
    }
  }
  
  return(result)
}

# Does R have multiple dispatch?
abf2.load = function ( filename )
{
    fp = file(filename, "rb")
    result = list()
    
    # read header
    result$Header = .abf2.struct.read(fp, .ABF2.Header.def)
    if (result$Header$fFileSignature != "ABF2")
    {
        close(fp)
        stop("Only ABF2 file format is supported.")
    }
    
    # read section info
    result$SectionInfo = list()
    fptr = result$Header$byte.total
    for (i in 1:length(.ABF2.SectionInfoList))
    {
        SectionName = .ABF2.SectionInfoList[[i]]
        result$SectionInfo[[SectionName]] = .abf2.struct.read(fp, .ABF2.SectionInfo.def, fp.offset = fptr)
        fptr = fptr + result$SectionInfo[[SectionName]]$byte.total
    }
    
    # read all known sections
    result$Sections = list()
    result$Sections$Protocol = .abf2.load.section(fp, result$SectionInfo$Protocol, .ABF2.Protocol.def)
    result$Sections$Math = .abf2.load.section(fp, result$SectionInfo$Math, .ABF2.Math.def)
    result$Sections$ADC = .abf2.load.section(fp, result$SectionInfo$ADC, .ABF2.ADC.def)
    result$Sections$DAC = .abf2.load.section(fp, result$SectionInfo$DAC, .ABF2.DAC.def)
    result$Sections$EpochPerDAC = .abf2.load.section(fp, result$SectionInfo$EpochPerDAC, .ABF2.EpochPerDAC.def)
    result$Sections$Epoch = .abf2.load.section(fp, result$SectionInfo$Epoch, .ABF2.Epoch.def)
    result$Sections$StatsRegion = .abf2.load.section(fp, result$SectionInfo$StatsRegion, .ABF2.StatsRegion.def)
    result$Sections$UserList = .abf2.load.section(fp, result$SectionInfo$UserList, .ABF2.UserList.def)
    
    # read strings section
    fptr = result$SectionInfo$Strings$uBlockIndex * .ABF2.BlockSize
    size = result$SectionInfo$Strings$uBytes
    seek(fp, fptr)
    data = c()
    for (i in 1:size)
    {
        data[i] = readBin(fp, what = "raw")
    }
    result$Sections$Strings = .abf2.parseStrSect(data)
    if (length(result$Sections$Strings) != result$SectionInfo$Strings$llNumEntries)
    {
        warning("llNumEntries and actual entries read in Strings section do not match. Please check file integrity.")
    }
    # sections yet to read: Tag, Scope, Delta, VoiceTag, Annotation, Stats
    # parse read sections

    # Channel names and units
    ch.Num = result$SectionInfo$ADC$llNumEntries
    ch.Name = c()
    ch.Unit = c()
    ch.NameGuess = c()
    for (i in 1:ch.Num)
    {
      ch.Name[i] = result$Sections$Strings[[1 + i*2]]
      ch.Unit[i] = result$Sections$Strings[[2 + i*2]]
      if (endsWith(ch.Unit[i], "V"))
      {
        ch.NameGuess[i] = "Voltage"
      }
      else if (endsWith(ch.Unit[i], "A"))
      {
        ch.NameGuess[i] = "Current"
      }
      else
      {
        ch.NameGuess[i] = ch.Name[i]
      }
    }
    result$ChannelName = ch.Name
    result$ChannelNameGuess = ch.NameGuess
    result$ChannelUnit = ch.Unit
    
    # Load data into memory for later use
    rawdata.size = result$SectionInfo$Data$uBytes
    if (rawdata.size == 2)
        rawdata.type = "integer"
    else if (rawdata.size == 4)
        rawdata.type = "numeric"
    else
        stop("Unknown data type")
    rawdata.num = result$SectionInfo$Data$llNumEntries
    rawdata.ptr = result$SectionInfo$Data$uBlockIndex * .ABF2.BlockSize
    seek(fp, where = rawdata.ptr)
    rawdata = array(dim = c(rawdata.num))
    for (i in 1:rawdata.num)
    {
        rawdata[i] = readBin(fp, what = rawdata.type, size = rawdata.size, signed = TRUE)
    }

    # caluclate scaling factors for every channel
    if (rawdata.type == "integer")
    {
        resol = result$Sections$Protocol["fADCRange", 1]/result$Sections$Protocol["lADCResolution", 1]
        scale = c()
        offset = c()
        for (i in 1:ch.Num)
        {
            #instrument scale factor
            scale[i] = result$Sections$ADC["fInstrumentScaleFactor", i]
            #signal gain
            scale[i] = scale[i] * result$Sections$ADC["fSignalGain", i]
            #programmable gain
            scale[i] = scale[i] * result$Sections$ADC["fADCProgrammableGain", i]
            #telegraph addit gain
            if (result$Sections$ADC["nTelegraphEnable", i])
            {
                scale[i] = scale[i] * result$Sections$ADC["fTelegraphAdditGain", i]
            }
            #instrument & signal offset
            offset[i] = result$Sections$ADC["fInstrumentOffset", i] - result$Sections$ADC["fSignalOffset", i]
        }
    }

    # sampling time interval
    result$SampleInterval_s = result$Sections$Protocol["fADCSequenceInterval", 1] * 1e-6
    result$SampleInterval_ms = result$Sections$Protocol["fADCSequenceInterval", 1] * 1e-3

    op.Mode = result$Sections$Protocol["nOperationMode", 1]
    if (op.Mode == 1)
    {
        # event-driven variable-length
        stop("Event-driven variable-length is not implemented yet.")
    }
    else if ((op.Mode == 2) | (op.Mode == 4) | (op.Mode == 5))
    {
        # event-driven fixed-length (2), high-speed oscilloscope (4), waveform fixed-length (5)
        # What we need: points per channel, channels per episode, number of episodes
        ptsPerCh = result$Sections$Protocol["lNumSamplesPerEpisode", 1] / ch.Num
        chPerEpi = ch.Num
        numEpi = result$Sections$Protocol["lEpisodesPerRun", 1]
        # Sanity test
        totPts = result$SectionInfo$Data$llNumEntries
        if (totPts != ptsPerCh * chPerEpi * numEpi)
            stop("The number of recorded data points does not match protocol setting. Please check file integrity.")
        # now map rawdata to a 3d array
        data = array(dim = c(ptsPerCh, chPerEpi, numEpi))
        # recording order:
        # data points for every channel at a time -> ptsPerChannel times -> repeat for every episode
        idx = 0
        for (k in 1:numEpi)
            for (i in 1:ptsPerCh)
                for (j in 1:chPerEpi)
                {
                    idx = idx + 1
                    data[i, j, k] = rawdata[idx]
                }
        # scale data if needed
        if (rawdata.type == "integer")
            for (i in 1:ptsPerCh)
                for (j in 1:chPerEpi)
                    for (k in 1:numEpi)
                        data[i, j, k] = data[i, j, k] / scale[j] * resol + offset[j]
        result$NumOfEpisodes = numEpi
        result$ChannelsPerEpisode = chPerEpi
        result$PointsPerChannel = ptsPerCh
        result$data = data
        
        # arrange data by channel
        ByChannel = list()
        for (i in 1:chPerEpi)
        {
          tmp = data[, i, ]
          tmpdf = data.frame(tmp)
          cols = episode.get_names(1:numEpi)
          colnames(tmpdf) = cols
          ByChannel[[i]] = tmpdf
        }
        result$ByChannel = ByChannel
        
        # make a x vector
        result$X_ticks = 1:ptsPerCh
        result$X_s = seq(from = 0, length = ptsPerCh, by = result$SampleInterval_s)
        result$X_ms = seq(from = 0, length = ptsPerCh, by = result$SampleInterval_ms)
    }
    else if (op.Mode == 3)
    {
        # gap-free
        result$NumOfChannel = ch.Num
        result$PointsPerChannel = result$SectionInfo$Data$llNumEntries / result$NumOfChannel
        # map rawdata to structured data
        data = array(dim = c(result$PointsPerChannel, result$NumOfChannel))
        idx = 0
        for (i in 1:result$PointsPerChannel)
          for (j in 1:result$NumOfChannel)
          {
              idx = idx + 1
              data[i, j] = rawdata[idx]
          }
          
        # scale data if needed
        if (rawdata.type == "integer")
          for (i in 1:result$PointsPerChannel)
            for (j in 1:result$NumOfChannel)
                data[i, j] = data[i, j] / scale[j] * resol + offset[j]

        result$data = data.frame(data)
        colnames(result$data) <- result$ChannelNameGuess

        # figure out some useful information
        timespan = result$PointsPerChannel * result$SampleInterval_s
        result$TimeSpan_s = timespan
        result$TimeSpan_ms = timespan * 1e3
    }
    else
    {
        stop("Unknown operation mode.")
    }

    close(fp)
    return(result)
}

abf2.load_in_folder = function ( folder, filename_list )
{
  getfilename = function( folder, filename )
  {
    fname = ""
    if ( endsWith(folder, "/") )
      fname = paste(folder, filename, sep = "")
    else
      fname = paste(folder, "/", filename, sep = "")
    if ( !endsWith(fname, ".abf") )
      fname = paste(fname, ".abf", sep = "")
    
    return(fname)
  }
  
  n = length(filename_list)
  result = list()
  for (i in 1:n)
  {
    # filename_list can be any type ... a vector, a list etc. I'm not sure if [[]] is suitable for all cases
    # Is there any type hint in R?!
    fname = getfilename(folder, filename_list[[i]])
    result[[i]] = abf2.load(fname)
  }
  
  return(result)
}

# Block size of ABF file
.ABF2.BlockSize = 512

# Definition of ABF2 header
.ABF2.Header.field = c("fFileSignature", #0
                      "fFileVersionNumber1", #4
                      "fFileVersionNumber2", #5
                      "fFileVersionNumber3", #6
                      "fFileVersionNumber4", #7
                      "uFileInfoSize", #8
                      "lActualEpisodes", #12
                      "uFileStartDate", #16
                      "uFileStartTimeMS", #20
                      "uStopwatchTime", #24
                      "nFileType", #28
                      "nDataFormat", #30
                      "nSimultaneousScan", #32
                      "nCRCEnable", #34
                      "uFileCRC", #36
                      "FileGUID1", #40
                      "FileGUID2", #48
                      "uCreatorVersion", #56
                      "uCreatorNameIndex", #60
                      "uModifierVersion", #64
                      "uModifierNameIndex", #68
                      "uProtocolPathIndex") #72
.ABF2.Header.ctype = c("string",
                      "int8",
                      "int8",
                      "int8",
                      "int8",
                      "uint32",
                      "uint32",
                      "uint32",
                      "uint32",
                      "uint32",
                      "int16",
                      "int16",
                      "int16",
                      "int16",
                      "uint32",
                      "int64",
                      "int64",
                      "uint32",
                      "uint32",
                      "uint32",
                      "uint32",
                      "uint32")
.ABF2.Header.ssize = c(4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
.ABF2.Header.def = data.frame(field = .ABF2.Header.field,
                             ctype = .ABF2.Header.ctype,
                             ssize = .ABF2.Header.ssize)

#Definition of section info
.ABF2.SectionInfo.field = c("uBlockIndex",
                           "uBytes",
                           "llNumEntries")
.ABF2.SectionInfo.ctype = c("uint32",
                           "uint32",
                           "int64")
.ABF2.SectionInfo.ssize = c(0, 0, 0)
.ABF2.SectionInfo.def = data.frame(field = .ABF2.SectionInfo.field,
                                  ctype = .ABF2.SectionInfo.ctype,
                                  ssize = .ABF2.SectionInfo.ssize)
.ABF2.SectionInfoList = c("Protocol",
                         "ADC",
                         "DAC",
                         "Epoch",
                         "ADCPerDAC",
                         "EpochPerDAC",
                         "UserList",
                         "StatsRegion",
                         "Math",
                         "Strings",
                         "Data",
                         "Tag",
                         "Scope",
                         "Delta",
                         "VoiceTag",
                         "SynchArray",
                         "Annotation",
                         "Stats")

# Sections
.ABF2.Protocol.field = c( "nOperationMode",
                         "fADCSequenceInterval",
                         "bEnableFileCompression",
                         "sUnused1",
                         "uFileCompressionRatio",
                         "fSynchTimeUnit",
                         "fSecondsPerRun",
                         "lNumSamplesPerEpisode",
                         "lPreTriggerSamples",
                         "lEpisodesPerRun",
                         "lRunsPerTrial",
                         "lNumberOfTrials",
                         "nAveragingMode",
                         "nUndoRunCount",
                         "nFirstEpisodeInRun",
                         "fTriggerThreshold",
                         "nTriggerSource",
                         "nTriggerAction",
                         "nTriggerPolarity",
                         "fScopeOutputInterval",
                         "fEpisodeStartToStart",
                         "fRunStartToStart",
                         "lAverageCount",
                         "fTrialStartToStart",
                         "nAutoTriggerStrategy",
                         "fFirstRunDelayS",
                         "nChannelStatsStrategy",
                         "lSamplesPerTrace",
                         "lStartDisplayNum",
                         "lFinishDisplayNum",
                         "nShowPNRawData",
                         "fStatisticsPeriod",
                         "lStatisticsMeasurements",
                         "nStatisticsSaveStrategy",
                         "fADCRange",
                         "fDACRange",
                         "lADCResolution",
                         "lDACResolution",
                         "nExperimentType",
                         "nManualInfoStrategy",
                         "nCommentsEnable",
                         "lFileCommentIndex",
                         "nAutoAnalyseEnable",
                         "nSignalType",
                         "nDigitalEnable",
                         "nActiveDACChannel",
                         "nDigitalHolding",
                         "nDigitalInterEpisode",
                         "nDigitalDACChannel",
                         "nDigitalTrainActiveLogic",
                         "nStatsEnable",
                         "nStatisticsClearStrategy",
                         "nLevelHysteresis",
                         "lTimeHysteresis",
                         "nAllowExternalTags",
                         "nAverageAlgorithm",
                         "fAverageWeighting",
                         "nUndoPromptStrategy",
                         "nTrialTriggerSource",
                         "nStatisticsDisplayStrategy",
                         "nExternalTagType",
                         "nScopeTriggerOut",
                         "nLTPType",
                         "nAlternateDACOutputState",
                         "nAlternateDigitalOutputState",
                         "fCellID1",
                         "fCellID2",
                         "fCellID3",
                         "nDigitizerADCs",
                         "nDigitizerDACs",
                         "nDigitizerTotalDigitalOuts",
                         "nDigitizerSynchDigitalOuts",
                         "nDigitizerType",
                         "sUnused")
.ABF2.Protocol.ctype = c("int16",
                        "float",
                        "int8",
                        "unused",
                        "uint32",
                        "float",
                        "float",
                        "int32",
                        "int32",
                        "int32",
                        "int32",
                        "int32",
                        "int16",
                        "int16",
                        "int16",
                        "float",
                        "int16",
                        "int16",
                        "int16",
                        "float",
                        "float",
                        "float",
                        "int32",
                        "float",
                        "int16",
                        "float",
                        "int16",
                        "int32",
                        "int32",
                        "int32",
                        "int16",
                        "float",
                        "int32",
                        "int16",
                        "float",
                        "float",
                        "int32",
                        "int32",
                        "int16",
                        "int16",
                        "int16",
                        "int32",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int32",
                        "int16",
                        "int16",
                        "float",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "float",
                        "float",
                        "float",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "int16",
                        "unused"  )
.ABF2.Protocol.ssize = c(0, 0, 0, 3,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 304)
.ABF2.Protocol.def = data.frame(field = .ABF2.Protocol.field,
                               ctype = .ABF2.Protocol.ctype,
                               ssize = .ABF2.Protocol.ssize)

.ABF2.Math.field = c("nMathEnable", "nMathExpression", "uMathOperatorIndex", "uMathUnitsIndex",
                    "fMathUpperLimit", "fMathLowerLimit", "nMathADCNum1", "nMathADCNum2",
                    "sUnused1", "fMathK1", "fMathK2", "fMathK3",
                    "fMathK4", "fMathK5", "fMathK6", "sUnused2")
.ABF2.Math.ctype = c("int16", "int16", "uint32", "uint32",
                    "float", "float", "int16", "int16",
                    "unused", "float", "float", "float",
                    "float", "float", "float", "unused")
.ABF2.Math.ssize = c(0, 0, 0, 0,
                    0, 0, 0, 0,
                    16, 0, 0, 0,
                    0, 0, 0, 64)
.ABF2.Math.def = data.frame(field = .ABF2.Math.field,
                           ctype = .ABF2.Math.ctype,
                           ssize = .ABF2.Math.ssize)

.ABF2.ADC.field = c("nADCNum",
                   "nTelegraphEnable",
                   "nTelegraphInstrument",
                   "fTelegraphAdditGain",
                   "fTelegraphFilter",
                   "fTelegraphMembraneCap",
                   "nTelegraphMode",
                   "fTelegraphAccessResistance",
                   "nADCPtoLChannelMap",
                   "nADCSamplingSeq",
                   "fADCProgrammableGain",
                   "fADCDisplayAmplification",
                   "fADCDisplayOffset",
                   "fInstrumentScaleFactor",
                   "fInstrumentOffset",
                   "fSignalGain",
                   "fSignalOffset",
                   "fSignalLowpassFilter",
                   "fSignalHighpassFilter",
                   "nLowpassFilterType",
                   "nHighpassFilterType",
                   "fPostProcessLowpassFilter",
                   "nPostProcessLowpassFilterType",
                   "bEnabledDuringPN",
                   "nStatsChannelPolarity",
                   "lADCChannelNameIndex",
                   "lADCUnitsIndex",
                   "sUnused")
.ABF2.ADC.ctype = c( "int16",
                    "int16",
                    "int16",
                    "float",
                    "float",
                    "float",
                    "int16",
                    "float",
                    "int16",
                    "int16",
                    "float",
                    "float",
                    "float",
                    "float",
                    "float",
                    "float",
                    "float",
                    "float",
                    "float",
                    "char",
                    "char",
                    "float",
                    "char",
                    "int8",
                    "int16",
                    "int32",
                    "int32",
                    "unused")
.ABF2.ADC.ssize = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 46)
.ABF2.ADC.def = data.frame(field = .ABF2.ADC.field,
                          ctype = .ABF2.ADC.ctype,
                          ssize = .ABF2.ADC.ssize)

.ABF2.DAC.field = c("nDACNum",
                   "nTelegraphDACScaeFactorEnable",
                   "fInstrumentHoldingLevel",
                   "fDACScaleFactor",
                   "fDACHoldingLevel",
                   "fDACCalibrationFactor",
                   "fDACCalibrationOffset",
                   "lDACChannelNameIndex",
                   "lDACChannelUnitsIndex",
                   "lDACFilePtr",
                   "lDACFileNumEpisodes",
                   "nWaveformEnable",
                   "nWaveformSource",
                   "nInterEpisodeLevel",
                   "fDACFileScale",
                   "fDACFileOffset",
                   "lDACFileEpisodeNum",
                   "nDACFileADCNum",
                   "nConditEnable",
                   "lConditNumPulses",
                   "fBaselineDuration",
                   "fBaselineLevel",
                   "fStepDuration",
                   "fStepLevel",
                   "fPostTrainPeriod",
                   "fPostTrainLevel",
                   "nMembTestEnable",
                   "nLeakSubtractType",
                   "nPNPolarity",
                   "fPNHoldingLevel",
                   "nPNNumADCChannels",
                   "nPNPosition",
                   "nPNNumPulses",
                   "fPNSettlingTime",
                   "fPNInterpulse",
                   "nLTPUsageOfDAC",
                   "nLTPPresynapticPulses",
                   "lDACFilePathIndex",
                   "fMembTestPreSettlingTimeMS",
                   "fMembTestPostSettlingTimeMS",
                   "nLeakSubtractADCIndex",
                   "sUnused")
.ABF2.DAC.ctype = c( "int16",
                    "int16",
                    "float",
                    "float",
                    "float",
                    "float",
                    "float",
                    "int32",
                    "int32",
                    "int32",
                    "int32",
                    "int16",
                    "int16",
                    "int16",
                    "float",
                    "float",
                    "int32",
                    "int16",
                    "int16",
                    "int32",
                    "float",
                    "float",
                    "float",
                    "float",
                    "float",
                    "float",
                    "int16",
                    "int16",
                    "int16",
                    "float",
                    "int16",
                    "int16",
                    "int16",
                    "float",
                    "float",
                    "int16",
                    "int16",
                    "int32",
                    "float",
                    "float",
                    "int16",
                    "unused" )
.ABF2.DAC.ssize = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 124)
.ABF2.DAC.def = data.frame(field = .ABF2.DAC.field,
                          ctype = .ABF2.DAC.ctype,
                          ssize = .ABF2.DAC.ssize)

.ABF2.EpochPerDAC.field = c("nEpochNum",
                           "nDACNum",
                           "nEpochType",
                           "fEpochInitLevel",
                           "fEpochLevelInc",
                           "lEpochInitDuration",
                           "lEpochDurationInc",
                           "lEpochPulsePeriod",
                           "lEpochPulseWidth",
                           "sUnused")
.ABF2.EpochPerDAC.ctype = c( "int16",
                            "int16",
                            "int16",
                            "float",
                            "float",
                            "int32",
                            "int32",
                            "int32",
                            "int32",
                            "unused" )
.ABF2.EpochPerDAC.ssize = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 18)
.ABF2.EpochPerDAC.def = data.frame(field = .ABF2.EpochPerDAC.field,
                                  ctype = .ABF2.EpochPerDAC.ctype,
                                  ssize = .ABF2.EpochPerDAC.ssize)

.ABF2.Epoch.field = c("nEpochNum",
                     "nDigitalValue",
                     "nDigitalTrainValue",
                     "nAlternateDigitalValue",
                     "nAlternateDigitalTrainValue",
                     "bEpochCompression",
                     "sUnused")
.ABF2.Epoch.ctype = c("int16",
                     "int16",
                     "int16",
                     "int16",
                     "int16",
                     "int8",
                     "unused")
.ABF2.Epoch.ssize = c(0, 0, 0, 0, 0, 0, 21)
.ABF2.Epoch.def = data.frame(field = .ABF2.Epoch.field,
                            ctype = .ABF2.Epoch.ctype,
                            ssize = .ABF2.Epoch.ssize)

.ABF2.StatsRegion.field = c("nRegionNum",
                           "nADCNum",
                           "nStatsActiveChannels",
                           "nStatsSearchRegionFlags",
                           "nStatsSelectedRegion",
                           "nStatsSmoothing",
                           "nStatsSmoothingEnable",
                           "nStatsBaseline",
                           "lStatsBaselineStart",
                           "lStatsBaselineEnd",
                           "lStatsMeasurements",
                           "lStatsStart",
                           "lStatsEnd",
                           "nRiseBottomPercentile",
                           "nRiseTopPercentile",
                           "nDecayBottomPercentile",
                           "nDecayTopPercentile",
                           "nStatsSearchMode",
                           "nStatsSearchDAC",
                           "nStatsBaselineDAC",
                           "sUnused")
.ABF2.StatsRegion.ctype = c( "int16",
                            "int16",
                            "int16",
                            "int16",
                            "int16",
                            "int16",
                            "int16",
                            "int16",
                            "int32",
                            "int32",
                            "int32",
                            "int32",
                            "int32",
                            "int16",
                            "int16",
                            "int16",
                            "int16",
                            "int16",
                            "int16",
                            "int16",
                            "unused"  )
.ABF2.StatsRegion.ssize = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 78)
.ABF2.StatsRegion.def = data.frame(field = .ABF2.StatsRegion.field,
                                  ctype = .ABF2.StatsRegion.ctype,
                                  sszie = .ABF2.StatsRegion.ssize)

.ABF2.UserList.field = c("nListNum",
                        "nULEnable",
                        "nULParamToVary",
                        "nULRepeat",
                        "lULParamValueListIndex",
                        "sUnused")
.ABF2.UserList.ctype = c("int16",
                        "int16",
                        "int16",
                        "int16",
                        "int32",
                        "unused")
.ABF2.UserList.ssize = c(0, 0, 0, 0, 0, 52)
.ABF2.UserList.def = data.frame(field = .ABF2.UserList.field,
                               ctype = .ABF2.UserList.ctype,
                               sszie = .ABF2.UserList.ssize)
