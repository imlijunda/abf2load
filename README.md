# this package is migrated to Crystal-YWu/abftools

# abf2load
Read voltage clamp ABF2 file in R

Currently event-driven fixed-length (mode 2), high-speed oscilloscope (mode 4), waveform fixed-length (mode 5) and gap-free (mode 3) are supported. Event-driven variable-length (mode 1) support is planned but not yet implemented.

## INSTALL:
```r
devtools::install_github("imlijunda/abf2load")
```

## USAGE:
```r
library(abf2load)
data = abf2.load("path/to/your/data.abf")

files_to_load = c("exp1.abf", "exp2.abf", "exp3.abf")
data_list = abf2.load_in_folder("data/folder/path", files_to_load)
```

You can also find examples in example folder.

---
This work was originally made for helping my wife to patch process her voltage clamp data. I rarely program in R so forgive me if some implmentation seems wierd to you. If you have any further knowledge/documents about the file structure, especially in mode 1 feel free to tell me or just raise an issue. Thanks.
