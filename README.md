# Background

This semantic-priming experiment was reported in Masson, Rabe, & Kliegl (2017, Exp. 1, _Memory & Cognition_). It is a direct replication of an experiment reported in Masson & Kliegl (2013, Exp. 1, JEPLMC). Following a prime word a related or unrelated high- or low-frequency target word or a nonword was presented in clear or dim font. The subject’s task was to decide as quickly as possible whether the target was a word or a nonword, that is subjects performed a lexical decision task (LDT). The reaction time and the accuracy of the response were recorded.

Seventy-three subjects participated in the experiment. Following 32 practice trials (not included in data), the experiment comprised 480 trials with breaks after blocks of 48 trials. Experimental conditions were presented in eight different random orders.

# Structure of the data

The file `MRK17_Exp1.csv` contains 35040 (`= 480 * 73`) observations of the 10 variables


|Variable | Description|
|---------|----------- |
|Subj     | Subject identifier (`S01` to `S73`)|
|CC       | Eight different random trial sequences (`cc1` to `cc8`); between-subject factor; not used |
|trial    | Trial number (1 to 480 for each subject, performed in 10 blocks of 48)|
|Q        | Target quality is _clear_ (`clr`) or _degraded_  (`deg`) |
|F        | Target is _nonword_ (`NW`), _high frequency_ (`HF`) or _low frequency_ (`LF`)|
|P        | Prime word is _related_ (`rel`) or _unrelated_ (`unr`) to target |
|Pword    | Prime word; not used |
|Item     | Target word or nonword  |
|rt       | Reaction time [ms] |
|Score    | Response was correct (`C`) or error (`E`)|

# Preparing the data for analysis

For the analyses described in this package, the observations were restricted to correct responses to word targets with response times in the range 300 ms. to 3000 ms.  Responses outside that range are assumed to be spurious.  In addition to the variables described above lagged target type (`lT` - either _word_ (`WD`) or _nonword_ (`NW`)) and lagged quality (`lQ`) were added to the analysis data set.  The first trial in each block (i.e. the trial number has a remainder of 1 when divided by 48) is dropped because these lagged variables cannot be evaluated.

The resulting data set consists of 16409 observations of 9 variables.

# Scripts and documents

The data are read and prepared for analysis using `R` in `DataPrep.Rmd` with the analysis data set being saved in RDS format as `MRK17_Exp1.rds`.  This file can be read in Julia using either the `RData` or `RCall` packages.  (At present it seems safer to use `RCall`.)

The original CSV file is read and prepared for analysis with Julia directly in `DataPrep.jmd`.
