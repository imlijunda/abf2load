# abf2load
Read voltage clamp ABF2 file in R

Currently event-driven fixed-length (mode 2), high-speed oscilloscope (mode 4), waveform fixed-length (mode 5) and gap-free (mode 3) are supported. Event-driven variable-length (mode 1) support is planned but not yet implemented.

## Usage:
Simply source the abf2load.R and use abf.load(filename) to load your abf2 data. The returned loaded data is structured list and should be self-explanatory. An example is provided in example/example.R.

---
This work was originally made for helping my wife to patch process her voltage clamp data. I rarely program in R so forgive me if some implmentation seems wierd to you. If you have any further knowledge/documents about the file structure, especially in mode 1 feel free to tell me or just raise an issue. Thanks.
