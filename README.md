# abf2load
Read voltage clamp ABF2 file in R

Currently event-driven fixed-length (mode 2), high-speed oscilloscope (mode 4) and waveform fixed-length (mode 5) are supported. It can also read gap-free (mode 3) data but not fully implemented (data is read into a matrix but not tested, and the protocol is not parsed). The plan is to also implement event-driven variable-length (mode 1), however, I do not have any test data I can look into and work on at the moment. Some sections are not parsed nor read at the moment, because either they are not necessary or I don't know their stored structures. I plan to parse Tag sections in the future since it looks somehow important for some researchers. I also plan to submit to cran if the code is mature enough, so any help is welcomed.

## Usage:
Simply source the abf2load.R and look into the using_abf2load.R I believe you'll figure out how to use this utility function.

---
This work is originally made for helping my wife to patch process her voltage clamp data. I rarely program in R so forgive me if some implmentation seems wierd to you. If you have any further knowledge about the file structure, especially in mode 1 and mode 3 feel free to tell me or just raise an issue. Thanks.
