# Simple support for C typed I/O
.ctypes.supported <- c("char", "uchar", "int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "float", "double", "string", "unused")
# and their corresponding size (in bytes)
.ctypes.bytesize <- c(1, 1, 1, 1, 2, 2, 4, 4, 8, 4, 8, 0, 0)
.sizeof <- list()
# get size by sizeof[["uint16"]]
.sizeof <- setNames(.ctypes.bytesize, .ctypes.supported)

.abf2.struct.read <- function ( fp, struct.def, fp.offset = 0 )
{
  result <- list()
  seek(fp, where = fp.offset, origin = "start")
  byteread <- 0
  byteskip <- 0
  for ( i in 1:length(struct.def$field))
  {
    fd <- as.character(struct.def$field[i])
    tp <- as.character(struct.def$ctype[i])
    ss <- as.integer(struct.def$ssize[i])
    sz <- .sizeof[[tp]]
    if ( startsWith(tp, "int") | (tp == "char") )
    {
      # read a signed integer
      result[[fd]] <- readBin(fp, what = "integer", size = sz, signed = TRUE)
      byteread <- byteread + sz
    }
    else if ( (tp == "float") | (tp == "double") )
    {
      # read a floating point numbers
      result[[fd]] <- readBin(fp, what = "numeric", size = sz)
      byteread <- byteread + sz
    }
    else if ( startsWith(tp, "uint") | (tp == "uchar") )
    {
      # read an unsigned integer, this gets a little bit tricky because R does not support unsigned long
      if ( sz == 4 )
      {
        # use numeric to store an unsigned long
        b1 <- readBin(fp, what = "integer", size = 2, signed = FALSE)
        b2 <- readBin(fp, what = "integer", size = 2, signed = FALSE)
        if ( .Platform$endian == "little" )
        {
          result[[fd]] <- 65536.0 * b2 + b1
        }
        else
        {
          result[[fd]] <- 65536.0 * b1 + b2
        }
      }
      else
      {
        result[[fd]] <- readBin(fp, what = "integer", size = sz, signed = FALSE)
      }
      byteread <- byteread + sz
    }
    else if ( tp == "string" )
    {
      if ( ss != 0 )
      {
        # do readChar read nul ending?
        tmpstr <- readChar(fp, ss)
        result[[fd]] <- tmpstr
        byteread <- byteread + length(charToRaw(tmpstr))
      }
      else
      {
        # string character size is not defined, read into a raw byte vector with until first 0 byte
        tmpstr <- c()
        idx <- 0
        while ( TRUE )
        {
          tmpchar <- readBin(fp, what = "integer", size = 1, signed = FALSE)
          idx <- idx + 1
          tmpstr[idx] <- tmpchar
          byteread <- byteread + 1
          if ( tmpchar == 0 )
          {
            break
          }
        }
        result[[fd]] <- tmpstr
      }
    }
    else if ( tp == "unused" )
    {
      # simply skip ss bytes of data
      seek(fp, where = ss, origin = "current")
      byteskip <- byteskip + ss
      result[[fd]] <- 0
    }
  }
  # result$byte.read = byteread
  # result$byte.skip = byteskip
  result$byte.total <- byteread + byteskip

  return(result)
}

.abf2.struct.size <- function ( struct.def )
{
  size <- 0
  for ( i in 1:length(struct.def$field))
  {
    tp <- as.character(struct.def$ctype[i])
    ss <- as.integer(struct.def$ssize[i])
    size <- size + .sizeof[[tp]] + ss
  }

  return(size)
}

.abf2.load.section <- function( fp, sect.info, sect.def )
{
  ptr <- sect.info$uBlockIndex * .ABF2.BlockSize
  if ( ptr == 0)
    return(NULL)

  #pre-allocate result data frame
  n <- sect.info$llNumEntries
  m <- length(sect.def$field)
  result <- data.frame(matrix(ncol = m, nrow = n))
  colnames(result) <- sect.def$field

  for (i in 1:n)
  {
    tmp <- .abf2.struct.read(fp, sect.def, ptr)
    ptr <- ptr + tmp$byte.total
    for (j in 1:m)
    {
      result[i, j] <- tmp[[j]]
    }
  }

  return(result)
}

.abf2.parseStrSect <- function ( rawdata )
{
  result <- list()
  offset <- .ABF2.StringOffset
  idx <- 0
  str.ptr <- offset
  for (i in offset:length(rawdata))
  {
    if (rawdata[[i]] == 0)
    {
      idx <- idx + 1
      result[[idx]] <- rawToChar(rawdata[str.ptr:(i-1)])
      str.ptr <- i + 1
    }
  }

  return(result)
}

.abf2.epinames <- function ( n )
{
  return(sapply(1:n, function (i) paste("epi", i, sep="")))
}

.abf2.chnames <- function ( n )
{
  return(sapply(1:n, function (i) paste("ch", i, sep="")))
}

#' Load abf2 data file.
#'
#' @param filename
#'
#' @return Loaded abf2 data
#' @export
#'
#' @examples data <- abf2.load("data/example.abf")
abf2.load <- function ( filename )
{
  fp <- file(filename, "rb")
  result <- list()

  # read header
  result$Header <- .abf2.struct.read(fp, .ABF2.Header.def)
  if (result$Header$fFileSignature != "ABF2")
  {
    close(fp)
    stop("Only ABF2 file format is supported.")
  }

  # read section info
  result$SectionInfo <- list()
  fptr <- result$Header$byte.total
  for (i in 1:length(.ABF2.SectionInfoList))
  {
    SectionName <- .ABF2.SectionInfoList[[i]]
    result$SectionInfo[[SectionName]] <- .abf2.struct.read(fp, .ABF2.SectionInfo.def, fp.offset = fptr)
    fptr <- fptr + result$SectionInfo[[SectionName]]$byte.total
  }

  # read all known sections
  result$Sections <- list()
  result$Sections$Protocol    <- .abf2.load.section(fp, result$SectionInfo$Protocol,    .ABF2.Protocol.def)
  result$Sections$Math        <- .abf2.load.section(fp, result$SectionInfo$Math,        .ABF2.Math.def)
  result$Sections$ADC         <- .abf2.load.section(fp, result$SectionInfo$ADC,         .ABF2.ADC.def)
  result$Sections$DAC         <- .abf2.load.section(fp, result$SectionInfo$DAC,         .ABF2.DAC.def)
  result$Sections$EpochPerDAC <- .abf2.load.section(fp, result$SectionInfo$EpochPerDAC, .ABF2.EpochPerDAC.def)
  result$Sections$Epoch       <- .abf2.load.section(fp, result$SectionInfo$Epoch,       .ABF2.Epoch.def)
  result$Sections$StatsRegion <- .abf2.load.section(fp, result$SectionInfo$StatsRegion, .ABF2.StatsRegion.def)
  result$Sections$UserList    <- .abf2.load.section(fp, result$SectionInfo$UserList,    .ABF2.UserList.def)

  # read strings section
  fptr <- result$SectionInfo$Strings$uBlockIndex * .ABF2.BlockSize
  str.size <- result$SectionInfo$Strings$uBytes
  str.data <- vector(mode = "raw", length = str.size)
  seek(fp, fptr)
  str.data <- readBin(fp, what = "raw", n = str.size)

  result$Sections$Strings <- .abf2.parseStrSect(str.data)
  if (length(result$Sections$Strings) != result$SectionInfo$Strings$llNumEntries)
  {
    warning("llNumEntries and actual entries read in Strings section do not match. Please check file integrity.")
  }
  # sections yet to read: Tag, Scope, Delta, VoiceTag, Annotation, Stats
  # parse read sections

  # Channel names and units
  ch.Num <- result$SectionInfo$ADC$llNumEntries
  ch.Name <- c()
  ch.Unit <- c()
  ch.NameGuess <- c()
  for (i in 1:ch.Num)
  {
    ch.Name[i] <- result$Sections$Strings[[1 + i*2]]
    ch.Unit[i] <- result$Sections$Strings[[2 + i*2]]
    if (endsWith(ch.Unit[i], "V"))
    {
      ch.NameGuess[i] <- "Voltage"
    }
    else if (endsWith(ch.Unit[i], "A"))
    {
      ch.NameGuess[i] <- "Current"
    }
    else
    {
      ch.NameGuess[i] <- ch.Name[i]
    }
  }
  result$ChannelName <- ch.Name
  result$ChannelNameGuess <- ch.NameGuess
  result$ChannelUnit <- ch.Unit

  # Load data into memory for later use
  rawdata.size <- result$SectionInfo$Data$uBytes
  if (rawdata.size == 2)
    rawdata.type <- "integer"
  else if (rawdata.size == 4)
    rawdata.type <- "numeric"
  else
    stop("Unknown data type")
  rawdata.num <- result$SectionInfo$Data$llNumEntries
  rawdata.ptr <- result$SectionInfo$Data$uBlockIndex * .ABF2.BlockSize
  seek(fp, where = rawdata.ptr)
  rawdata <- vector(mode = rawdata.type, length = rawdata.num)
  rawdata <- readBin(fp, what = rawdata.type, size = rawdata.size, signed = TRUE, n = rawdata.num)

  # caluclate scaling factors for every channel, only happens when recording in integer type
  if (rawdata.type == "integer")
  {
    signal.resol <- result$Sections$Protocol$fADCRange[1] / result$Sections$Protocol$lADCResolution[1]
    signal.scale <- c()
    signal.offset <- c()
    for (i in 1:ch.Num)
    {
      #instrument scale factor
      signal.scale[i] <- result$Sections$ADC$fInstrumentScaleFactor[i]
      #signal gain
      signal.scale[i] <- signal.scale[i] * result$Sections$ADC$fSignalGain[i]
      #programmable gain
      signal.scale[i] <- signal.scale[i] * result$Sections$ADC$fADCProgrammableGain[i]
      #telegraph addit gain
      if (result$Sections$ADC$nTelegraphEnable[i])
      {
        signal.scale[i] <- signal.scale[i] * result$Sections$ADC$fTelegraphAdditGain[i]
      }
      #instrument & signal offset
      signal.offset[i] <- result$Sections$ADC$fInstrumentOffset[i] - result$Sections$ADC$fSignalOffset[i]
    }
  }

  op.Mode <- result$Sections$Protocol$nOperationMode[1]
  if (op.Mode == 1)
  {
    # event-driven variable-length
    stop("Event-driven variable-length is not implemented yet.")
  }
  else if ((op.Mode == 2) | (op.Mode == 4) | (op.Mode == 5))
  {
    # event-driven fixed-length (2), high-speed oscilloscope (4), waveform fixed-length (5)

    # sampling time interval
    result$SampleInterval_s <- result$Sections$Protocol$fADCSequenceInterval[1] * 1e-6
    result$SampleInterval_ms <- result$Sections$Protocol$fADCSequenceInterval[1] * 1e-3

    # What we need: points per channel, channels per episode, number of episodes
    ptsPerCh <- result$Sections$Protocol$lNumSamplesPerEpisode[1] / ch.Num
    chPerEpi <- ch.Num
    numEpi <- result$Sections$Protocol$lEpisodesPerRun[1]
    # Sanity test
    totPts <- result$SectionInfo$Data$llNumEntries
    if (totPts != ptsPerCh * chPerEpi * numEpi)
    {
      close(fp)
      stop("The number of recorded data points does not match protocol setting. Please check file integrity.")
    }

    data <- array(data = rawdata, dim = c(chPerEpi,ptsPerCh,numEpi))
    if (rawdata.type == "integer")
      for (i in 1:chPerEpi)
        data[i,,] <- data[i,,] / signal.scale[i] * signal.resol + signal.offset[i]

    result$NumOfEpisodes <- numEpi
    result$ChannelsPerEpisode <- chPerEpi
    result$PointsPerChannel <- ptsPerCh
    result$data <- data

    # arrange data by channel
    result$ByChannel <- list()
    for (i in 1:chPerEpi)
    {
      result$ByChannel[[i]] <- data.frame(data[i,,])
      cols <- .abf2.epinames(numEpi)
      colnames(result$ByChannel[[i]]) <- cols
    }

    # arrange data by episode
    result$ByEpisode <- list()
    for (i in 1:numEpi)
    {
      result$ByEpisode[[i]] <- data.frame(t(data[,,i]))
      cols <- .abf2.chnames(chPerEpi)
      colnames(result$ByEpisode[[i]]) <- cols
    }

    # make x vectors
    result$X_ticks <- 1:ptsPerCh
    result$X_s <- seq(from = 0, length = ptsPerCh, by = result$SampleInterval_s)
    result$X_ms <- seq(from = 0, length = ptsPerCh, by = result$SampleInterval_ms)
  }
  else if (op.Mode == 3)
  {
    # gap-free
    result$NumOfChannel <- ch.Num
    result$PointsPerChannel <- result$SectionInfo$Data$llNumEntries / result$NumOfChannel
    # map rawdata to structured data
    result$data <- array(data = rawdata, dim = c(result$NumOfChannel, result$PointsPerChannel))
    # scale data if needed
    if (rawdata.type == "integer")
      for (i in 1:result$NumOfChannel)
        result$data[i,] <- result$data[i,] / signal.scale[i] * signal.resol + signal.offset[i]

    result$ByChannel <- data.frame(t(result$data))
    colnames(result$ByChannel) <- .abf2.chnames(ch.Num)

    # figure out some useful information
    # sampling time interval
    result$SampleInterval_s <- result$Sections$Protocol$fADCSequenceInterval[1] * 1e-6
    result$SampleInterval_ms <- result$Sections$Protocol$fADCSequenceInterval[1] * 1e-3
    result$X_ticks <- 1:result$PointsPerChannel
    result$X_s <- seq(from = 0, length = result$PointsPerChannel, by = result$SampleInterval_s)
    result$X_ms <- seq(from = 0, length = result$PointsPerChannel, by = result$SampleInterval_ms)
  }
  else
  {
    close(fp)
    stop("Unknown operation mode.")
  }

  close(fp)
  return(result)
}

#' Load abf2 data files in a folder
#'
#' @param folder
#' @param filename_list
#'
#' @return A list of loaded abf2 data.
#' @export
#'
#' @examples data_list <- abf2.load("data", c("example.abf", "example2.abf"))
abf2.load_in_folder <- function ( folder, filename_list )
{
  getfilename <- function( folder, filename )
  {
    fname <- ""
    if ( endsWith(folder, "/") )
      fname <- paste(folder, filename, sep = "")
    else
      fname <- paste(folder, "/", filename, sep = "")
    if ( !endsWith(fname, ".abf") )
      fname <- paste(fname, ".abf", sep = "")

    return(fname)
  }

  n <- length(filename_list)
  result <- list()
  for (i in 1:n)
  {
    fname <- getfilename(folder, filename_list[[i]])
    result[[i]] <- abf2.load(fname)
  }

  return(result)
}

# Block size of ABF file
.ABF2.BlockSize <- 512
.ABF2.StringOffset <- 45

# Definition of ABF2 header
.ABF2.Header.field <- c("fFileSignature", #0
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
.ABF2.Header.ctype <- c("string",
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
.ABF2.Header.ssize <- c(4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
.ABF2.Header.def <- list()
.ABF2.Header.def$field <- .ABF2.Header.field
.ABF2.Header.def$ctype <- .ABF2.Header.ctype
.ABF2.Header.def$ssize <- .ABF2.Header.ssize

#Definition of section info
.ABF2.SectionInfo.field <- c("uBlockIndex",
                             "uBytes",
                             "llNumEntries")
.ABF2.SectionInfo.ctype <- c("uint32",
                             "uint32",
                             "int64")
.ABF2.SectionInfo.ssize <- c(0, 0, 0)
.ABF2.SectionInfo.def <- list()
.ABF2.SectionInfo.def$field <- .ABF2.SectionInfo.field
.ABF2.SectionInfo.def$ctype <- .ABF2.SectionInfo.ctype
.ABF2.SectionInfo.def$ssize <- .ABF2.SectionInfo.ssize

.ABF2.SectionInfoList <- c("Protocol",
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
.ABF2.Protocol.field <- c( "nOperationMode",
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
.ABF2.Protocol.ctype <- c("int16",
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
.ABF2.Protocol.ssize <- c(0, 0, 0, 3,
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
.ABF2.Protocol.def <- list()
.ABF2.Protocol.def$field <- .ABF2.Protocol.field
.ABF2.Protocol.def$ctype <- .ABF2.Protocol.ctype
.ABF2.Protocol.def$ssize <- .ABF2.Protocol.ssize

.ABF2.Math.field <- c("nMathEnable", "nMathExpression", "uMathOperatorIndex", "uMathUnitsIndex",
                      "fMathUpperLimit", "fMathLowerLimit", "nMathADCNum1", "nMathADCNum2",
                      "sUnused1", "fMathK1", "fMathK2", "fMathK3",
                      "fMathK4", "fMathK5", "fMathK6", "sUnused2")
.ABF2.Math.ctype <- c("int16", "int16", "uint32", "uint32",
                      "float", "float", "int16", "int16",
                      "unused", "float", "float", "float",
                      "float", "float", "float", "unused")
.ABF2.Math.ssize <- c(0, 0, 0, 0,
                      0, 0, 0, 0,
                      16, 0, 0, 0,
                      0, 0, 0, 64)
.ABF2.Math.def <- list()
.ABF2.Math.def$field <- .ABF2.Math.field
.ABF2.Math.def$ctype <- .ABF2.Math.ctype
.ABF2.Math.def$ssize <- .ABF2.Math.ssize

.ABF2.ADC.field <- c("nADCNum",
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
.ABF2.ADC.ctype <- c( "int16",
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
.ABF2.ADC.ssize <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 46)
.ABF2.ADC.def <- list()
.ABF2.ADC.def$field <- .ABF2.ADC.field
.ABF2.ADC.def$ctype <- .ABF2.ADC.ctype
.ABF2.ADC.def$ssize <- .ABF2.ADC.ssize

.ABF2.DAC.field <- c("nDACNum",
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
.ABF2.DAC.ctype <- c( "int16",
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
.ABF2.DAC.ssize <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 124)
.ABF2.DAC.def <- list()
.ABF2.DAC.def$field <- .ABF2.DAC.field
.ABF2.DAC.def$ctype <- .ABF2.DAC.ctype
.ABF2.DAC.def$ssize <- .ABF2.DAC.ssize

.ABF2.EpochPerDAC.field <- c("nEpochNum",
                             "nDACNum",
                             "nEpochType",
                             "fEpochInitLevel",
                             "fEpochLevelInc",
                             "lEpochInitDuration",
                             "lEpochDurationInc",
                             "lEpochPulsePeriod",
                             "lEpochPulseWidth",
                             "sUnused")
.ABF2.EpochPerDAC.ctype <- c( "int16",
                              "int16",
                              "int16",
                              "float",
                              "float",
                              "int32",
                              "int32",
                              "int32",
                              "int32",
                              "unused" )
.ABF2.EpochPerDAC.ssize <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 18)
.ABF2.EpochPerDAC.def <- list()
.ABF2.EpochPerDAC.def$field <- .ABF2.EpochPerDAC.field
.ABF2.EpochPerDAC.def$ctype <- .ABF2.EpochPerDAC.ctype
.ABF2.EpochPerDAC.def$ssize <- .ABF2.EpochPerDAC.ssize

.ABF2.Epoch.field <- c("nEpochNum",
                       "nDigitalValue",
                       "nDigitalTrainValue",
                       "nAlternateDigitalValue",
                       "nAlternateDigitalTrainValue",
                       "bEpochCompression",
                       "sUnused")
.ABF2.Epoch.ctype <- c("int16",
                       "int16",
                       "int16",
                       "int16",
                       "int16",
                       "int8",
                       "unused")
.ABF2.Epoch.ssize <- c(0, 0, 0, 0, 0, 0, 21)
.ABF2.Epoch.def <- list()
.ABF2.Epoch.def$field <- .ABF2.Epoch.field
.ABF2.Epoch.def$ctype <- .ABF2.Epoch.ctype
.ABF2.Epoch.def$ssize <- .ABF2.Epoch.ssize

.ABF2.StatsRegion.field <- c("nRegionNum",
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
.ABF2.StatsRegion.ctype <- c( "int16",
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
.ABF2.StatsRegion.ssize <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 78)
.ABF2.StatsRegion.def <- list()
.ABF2.StatsRegion.def$field <- .ABF2.StatsRegion.field
.ABF2.StatsRegion.def$ctype <- .ABF2.StatsRegion.ctype
.ABF2.StatsRegion.def$ssize <- .ABF2.StatsRegion.ssize

.ABF2.UserList.field <- c("nListNum",
                          "nULEnable",
                          "nULParamToVary",
                          "nULRepeat",
                          "lULParamValueListIndex",
                          "sUnused")
.ABF2.UserList.ctype <- c("int16",
                          "int16",
                          "int16",
                          "int16",
                          "int32",
                          "unused")
.ABF2.UserList.ssize <- c(0, 0, 0, 0, 0, 52)
.ABF2.UserList.def <- list()
.ABF2.UserList.def$field <- .ABF2.UserList.field
.ABF2.UserList.def$ctype <- .ABF2.UserList.ctype
.ABF2.UserList.def$ssize <- .ABF2.UserList.ssize
