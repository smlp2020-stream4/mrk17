Background
==========

This semantic-priming experiment was reported in Masson, Rabe, & Kliegl
(2017, Exp. 1, Memory & Cognition). It is a direct replication of an
experiment reported in Masson & Kliegl (2013, Exp. 1, JEPLMC). Following
a prime word a related or unrelated high- or low-frequency target word
or a nonword was presented in clear or dim font. The subject’s task was
to decide as quickly as possible whether the target was a word or a
nonword, that is subjects performed a lexical decision task (LDT). The
reaction time and the accuracy of the response were recorded.

Seventy-three subjects participated in the experiment. Following 32
practice trials (not included in data), the experiment comprised 480
trials with breaks after blocks of 48 trials. Experimental conditions
were presented in eight different random orders.

Reading
=======

Upper-case variables are factors; lower-case variables are numeric.

<table>
<colgroup>
<col style="width: 45%" />
<col style="width: 55%" />
</colgroup>
<thead>
<tr class="header">
<th>Variable</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Subj</td>
<td>Subject identifier</td>
</tr>
<tr class="even">
<td>CC</td>
<td>Eight different random trial sequences ( <em>cc1</em> to <em>cc2</em> ); between-subject factor; not used</td>
</tr>
<tr class="odd">
<td>trial</td>
<td>Trial number</td>
</tr>
<tr class="even">
<td>Qlty</td>
<td>Target quality is <em>clear</em> or <em>dim</em></td>
</tr>
<tr class="odd">
<td>Frq</td>
<td>Target frequency is <em>high</em> or <em>low</em></td>
</tr>
<tr class="even">
<td>TPR</td>
<td>Target is <em>related</em> or <em>unrelated</em> to prime</td>
</tr>
<tr class="odd">
<td>Prime</td>
<td>Prime word; not used</td>
</tr>
<tr class="even">
<td>Item</td>
<td>Target (non-)word</td>
</tr>
<tr class="odd">
<td>rt</td>
<td>Reaction time [ms]</td>
</tr>
<tr class="even">
<td>Score</td>
<td>Correct (<em>C</em>) or error (<em>E</em>) response</td>
</tr>
</tbody>
</table>

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(forcats)

    data <- as_tibble(read.csv("MRK17_Exp1.csv"))
    str(data)

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    35040 obs. of  10 variables:
    ##  $ Subj : Factor w/ 73 levels "S01","S02","S03",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ CC   : Factor w/ 8 levels " cc1"," cc2",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ trial: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Qlty : Factor w/ 2 levels " Clear"," Degraded": 2 2 1 2 1 2 2 2 2 2 ...
    ##  $ Frq  : Factor w/ 3 levels " HF"," LF"," NW": 3 2 1 3 1 3 3 3 1 1 ...
    ##  $ TPR  : Factor w/ 2 levels " Rel"," Unr": 2 2 2 2 1 2 2 2 1 2 ...
    ##  $ Prime: Factor w/ 480 levels " abuse"," alcohol",..: 423 47 82 340 228 121 166 413 209 455 ...
    ##  $ Item : Factor w/ 480 levels " ABDESS"," ABLERM",..: 309 44 293 122 387 87 23 430 233 480 ...
    ##  $ rt   : int  762 794 506 612 491 631 637 690 486 482 ...
    ##  $ Score: Factor w/ 2 levels " C"," E": 1 1 1 1 1 1 1 1 1 1 ...

Preprocessing
=============

Lag variables
-------------

The innovative hypotheses of this experiment relate to experimental
effects associated with the last trial, specifically (1) its target
quality (clear or dim) and (2) whether the last target (lagTrg) required
a word or a nonword response (ignoring the frequency of the word). These
conditions are computed from the data. We also generate new labels for
factor levels.

    dat1 <- data %>% 
            mutate(Frq = fct_recode(Frq, "high" = ' HF', "low" = ' LF', "NW" = " NW"),
                   TPR = fct_recode(TPR, "related" = ' Rel', "unrelated" = " Unr"),
                   lagQlty = lag(data$Qlty),
                   lagTrg = lag(Frq),
                   lagTrg = fct_recode(lagTrg, "word" = 'high', "word" = 'low', "nonword" = "NW")
                   )  

Filtering observations
----------------------

The first of a ten blocks of 48 trials occurred after a pause. Therefore
it does not have valid lag information and these trials are excluded
from the analyses. Also, in an LDT, all nonword trials are routinely
left out from rt analyses. Finally, incorrectly answered word trials and
trials with very short (&lt; 300 ms) or very long rt’s are removed as
well. This leaves us with 16,409 observations.

    ix <- setdiff(1:480, seq(from=1, to=433, by=48))
    lag_trials <- which(dat1$trial %in% ix)
    dat2 <- dat1 %>% 
            slice(lag_trials) %>% 
            filter(Frq != "NW", Score == " C", between(rt, 300, 3000))
    dat2$Frq <- droplevels(dat2$Frq)  # get rid of empty "NW" level

Selecting variables
-------------------

We keep only the relevant variables and reorder them.

    dat3 <- dat2 %>% 
            select(Subj, Item, trial, Qlty, Frq, TPR, lagQlty, lagTrg, rt)
    dat3

    ## # A tibble: 16,409 x 9
    ##    Subj  Item       trial Qlty        Frq   TPR       lagQlty     lagTrg     rt
    ##    <fct> <fct>      <int> <fct>       <fct> <fct>     <fct>       <fct>   <int>
    ##  1 S01   " CAKE"        2 " Degraded" low   unrelated " Degraded" nonword   794
    ##  2 S01   " PARTY"       3 " Clear"    high  unrelated " Degraded" word      506
    ##  3 S01   " SPOT"        5 " Clear"    high  related   " Degraded" nonword   491
    ##  4 S01   " LONG"        9 " Degraded" high  related   " Degraded" nonword   486
    ##  5 S01   " YEAR"       10 " Degraded" high  unrelated " Degraded" word      482
    ##  6 S01   " COUNTRY"    11 " Clear"    high  related   " Degraded" word      542
    ##  7 S01   " HAVE"       15 " Degraded" high  related   " Degraded" nonword   766
    ##  8 S01   " TUSK"       21 " Degraded" low   unrelated " Clear"    nonword   993
    ##  9 S01   " CARDS"      22 " Clear"    low   unrelated " Degraded" word      552
    ## 10 S01   " FIGHT"      24 " Clear"    high  related   " Degraded" nonword   499
    ## # … with 16,399 more rows

    str(dat3)

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    16409 obs. of  9 variables:
    ##  $ Subj   : Factor w/ 73 levels "S01","S02","S03",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Item   : Factor w/ 480 levels " ABDESS"," ABLERM",..: 44 293 387 233 480 62 177 428 46 115 ...
    ##  $ trial  : int  2 3 5 9 10 11 15 21 22 24 ...
    ##  $ Qlty   : Factor w/ 2 levels " Clear"," Degraded": 2 1 1 2 2 1 2 2 1 1 ...
    ##  $ Frq    : Factor w/ 2 levels "high","low": 2 1 1 1 1 1 1 2 2 1 ...
    ##  $ TPR    : Factor w/ 2 levels "related","unrelated": 2 2 1 1 2 1 1 2 2 1 ...
    ##  $ lagQlty: Factor w/ 2 levels " Clear"," Degraded": 2 2 2 2 2 2 2 1 2 2 ...
    ##  $ lagTrg : Factor w/ 2 levels "word","nonword": 2 1 2 2 1 1 2 2 1 2 ...
    ##  $ rt     : int  794 506 491 486 482 542 766 993 552 499 ...

Save
====

The data (variables and observations) used by Masson et al. (2017) are
available in file `MRK17_Exp1.RDS`

<table>
<thead>
<tr class="header">
<th>Variable</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Subj</td>
<td>Subject identifier</td>
</tr>
<tr class="even">
<td>Item</td>
<td>Target (non-)word</td>
</tr>
<tr class="odd">
<td>trial</td>
<td>Trial number</td>
</tr>
<tr class="even">
<td>Qlty</td>
<td>Target quality is <em>clear</em> or <em>dim</em></td>
</tr>
<tr class="odd">
<td>Frq</td>
<td>Target frequency is <em>high</em> or <em>low</em></td>
</tr>
<tr class="even">
<td>TPR</td>
<td>Target is <em>related</em> or <em>unrelated</em> to prime</td>
</tr>
<tr class="odd">
<td>lagQlty</td>
<td>Lag target quality is <em>clear</em> or <em>dim</em></td>
</tr>
<tr class="even">
<td>lagTrg</td>
<td>Lag target require <em>word</em> or <em>nonword</em> response</td>
</tr>
<tr class="odd">
<td>rt</td>
<td>Reaction time [ms]</td>
</tr>
</tbody>
</table>

`lagQlty` and `lagTrg` refer to conditions on the previous trial.

    saveRDS(dat3, "MRK17_Exp1.rds")

Appendix
========

    sessionInfo()

    ## R version 3.6.2 (2019-12-12)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 19.10
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.8.0
    ## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.8.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] forcats_0.4.0 dplyr_0.8.3  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.3       knitr_1.26       magrittr_1.5     tidyselect_0.2.5
    ##  [5] R6_2.4.1         rlang_0.4.2      fansi_0.4.0      stringr_1.4.0   
    ##  [9] tools_3.6.2      xfun_0.11        utf8_1.1.4       cli_2.0.0       
    ## [13] htmltools_0.4.0  yaml_2.2.0       assertthat_0.2.1 digest_0.6.23   
    ## [17] tibble_2.1.3     crayon_1.3.4     purrr_0.3.3      vctrs_0.2.1     
    ## [21] zeallot_0.1.0    glue_1.3.1       evaluate_0.14    rmarkdown_2.0   
    ## [25] stringi_1.4.3    compiler_3.6.2   pillar_1.4.2     backports_1.1.5 
    ## [29] pkgconfig_2.0.3
