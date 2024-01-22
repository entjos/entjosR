# Summary table works with numeric values

    Code
      summary_table(brcancer, vars = c("x1"))
    Output
         varname mean p25 p75 min max n_category n_total n_NA
      1:      x1   53  46  61  21  80         NA     686    0

# Summary table works with factor values

    Code
      brcancer$x4 <- as.factor(brcancer$x4)
      summary_table(brcancer, vars = c("x4"))
    Output
         varname  mean p25 p75 min max n_category n_total n_NA
      1:    x4 1 0.118  NA  NA   0   1         81     686    0
      2:    x4 2 0.647  NA  NA   0   1        444     686    0
      3:    x4 3 0.235  NA  NA   0   1        161     686    0

# Summary table works with binary numeric values

    Code
      summary_table(brcancer, vars = c("hormon"))
    Output
         varname  mean p25 p75 min max n_category n_total n_NA
      1:  hormon 0.359  NA  NA   0   1        246     686    0

# Summary table works with strata

    Code
      summary_table(brcancer, vars = c("x7"), strata = "hormon")
    Output
         hormon varname mean p25   p75 min  max n_category n_total n_NA
      1:      0      x7   32   8  92.2   0  898         NA     440    0
      2:      1      x7   46   9 182.5   0 1144         NA     246    0

# Summary table works with rates

    Code
      brcancer$x2 <- as.factor(brcancer$x2)
      summary_table(brcancer, vars = c("x2"), strata = c("hormon"), get_rates = TRUE,
      event = "censrec", st = "rectime")
    Message
      > Rates will only be estimted for factor variables in `vars`.
    Output
         hormon varname  mean p25 p75 min max n_category n_total n_NA no_events
      1:      0    x2 1 0.525  NA  NA   0   1        231     440    0        97
      2:      0    x2 2 0.475  NA  NA   0   1        209     440    0       108
      3:      1    x2 1 0.240  NA  NA   0   1         59     246    0        22
      4:      1    x2 2 0.760  NA  NA   0   1        187     246    0        72
         t_at_risk  p_events     rate_est   rate_lower   rate_upper
      1:    238733 0.4731707 0.0004063117 0.0003294914 0.0004956658
      2:    227548 0.5268293 0.0004746251 0.0003893442 0.0005730339
      3:     81990 0.2340426 0.0002683254 0.0001681581 0.0004062479
      4:    223129 0.7659574 0.0003226833 0.0002524798 0.0004063662

