# Stable output

    Code
      fpm_test_dfs(Surv(surv_mm, status == "Dead: cancer") ~ age + year8594, dfs_bh = 1:
        3, dfs_tvc = list(age = 1:3, year8594 = 1:3), by_vars = c("sex", "stage"),
      same_dfs_tvc = TRUE, data = colon)
    Output
      data$filter_vars: Female Distant
        df_bh df_tvc_age df_tvc_year8594      aic      bic
      1     1          1               1 19910.36 19946.46
      2     2          1               1 19108.25 19150.36
      3     3          1               1 19103.12 19151.24
      4     1          2               2 19161.85 19209.97
      5     2          2               2 19083.41 19137.55
      6     3          2               2 19075.22 19135.37
      7     1          3               3 19157.92 19218.07
      8     2          3               3 19093.61 19159.78
      9     3          3               3 19079.55 19151.73
      ------------------------------------------------------------ 
      data$filter_vars: Female Localised
        df_bh df_tvc_age df_tvc_year8594      aic      bic
      1     1          1               1 12816.17 12853.39
      2     2          1               1 12777.10 12820.53
      3     3          1               1 12757.68 12807.31
      4     1          2               2 12778.44 12828.07
      5     2          2               2 12757.54 12813.37
      6     3          2               2 12748.10 12810.13
      7     1          3               3 12770.15 12832.18
      8     2          3               3 12759.63 12827.87
      9     3          3               3 12713.68 12788.13
      ------------------------------------------------------------ 
      data$filter_vars: Female Regional
        df_bh df_tvc_age df_tvc_year8594      aic      bic
      1     1          1               1 5952.186 5982.049
      2     2          1               1 5806.142 5840.983
      3     3          1               1 5793.497 5833.315
      4     1          2               2 5822.548 5862.366
      5     2          2               2 5790.532 5835.328
      6     3          2               2 5786.644 5836.416
      7     1          3               3 5809.844 5859.617
      8     2          3               3 5787.374 5842.124
      9     3          3               3 5789.635 5849.362
      ------------------------------------------------------------ 
      data$filter_vars: Female Unknown
        df_bh df_tvc_age df_tvc_year8594      aic      bic
      1     1          1               1 7318.245 7350.007
      2     2          1               1 7056.949 7094.005
      3     3          1               1 7047.460 7089.810
      4     1          2               2 7057.288 7099.637
      5     2          2               2 7059.040 7106.683
      6     3          2               2 7051.296 7104.233
      7     1          3               3 7049.032 7101.969
      8     2          3               3 7051.028 7109.259
      9     3          3               3 7048.968 7112.492
      ------------------------------------------------------------ 
      data$filter_vars: Male Distant
        df_bh df_tvc_age df_tvc_year8594      aic      bic
      1     1          1               1 13851.44 13885.39
      2     2          1               1 13322.99 13362.61
      3     3          1               1 13320.39 13365.66
      4     1          2               2 13367.60 13412.87
      5     2          2               2 13306.17 13357.10
      6     3          2               2 13299.90 13356.49
      7     1          3               3 13366.98 13423.57
      8     2          3               3 13314.53 13376.78
      9     3          3               3 13295.35 13363.26
      ------------------------------------------------------------ 
      data$filter_vars: Male Localised
        df_bh df_tvc_age df_tvc_year8594      aic      bic
      1     1          1               1 8797.116 8832.341
      2     2          1               1 8766.320 8807.417
      3     3          1               1 8738.177 8785.144
      4     1          2               2 8771.776 8818.744
      5     2          2               2 8760.261 8813.099
      6     3          2               2 8738.895 8797.604
      7     1          3               3 8750.802 8809.511
      8     2          3               3 8749.943 8814.523
      9     3          3               3 8714.162 8784.613
      ------------------------------------------------------------ 
      data$filter_vars: Male Regional
        df_bh df_tvc_age df_tvc_year8594      aic      bic
      1     1          1               1 3831.836 3859.270
      2     2          1               1 3804.296 3836.302
      3     3          1               1 3781.278 3817.856
      4     1          2               2 3807.213 3843.792
      5     2          2               2 3795.772 3836.922
      6     3          2               2 3781.959 3827.682
      7     1          3               3 3786.688 3832.411
      8     2          3               3 3784.731 3835.026
      9     3          3               3 3785.284 3840.152
      ------------------------------------------------------------ 
      data$filter_vars: Male Unknown
        df_bh df_tvc_age df_tvc_year8594      aic      bic
      1     1          1               1 3938.297 3967.011
      2     2          1               1 3846.137 3879.636
      3     3          1               1 3834.367 3872.652
      4     1          2               2 3849.247 3887.532
      5     2          2               2 3845.147 3888.218
      6     3          2               2 3822.626 3870.482
      7     1          3               3 3835.434 3883.290
      8     2          3               3 3829.243 3881.884
      9     3          3               3 3815.527 3872.954

# test same_dfs_tvs = false

    Code
      fpm_test_dfs(Surv(surv_mm, status == "Dead: cancer") ~ age + year8594, dfs_bh = 1:
        3, dfs_tvc = list(age = 1:2), same_dfs_tvc = FALSE, data = colon)
    Output
        df_bh dfs_tvc_age      aic      bic
      1     1           1 83424.72 83462.98
      2     2           1 80996.11 81042.03
      3     3           1 80964.51 81018.08
      4     1           2 81182.44 81228.35
      5     2           2 80907.35 80960.92
      6     3           2 80816.05 80877.27

