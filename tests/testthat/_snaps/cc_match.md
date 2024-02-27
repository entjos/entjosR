# Test replace TRUE

    Code
      cc_match(test_data, id = "id", case_indicator = "case", matching_factors = list(
        sex = "exact", time = function(x, y) y >= x), no_controls = 10, seed = 10,
      verbose = FALSE)
    Output
              id case riskset
         1:    1    1       1
         2: 3671    0       1
         3:  167    0       1
         4: 1491    0       1
         5: 1941    0       1
        ---                  
      1096: 8256    0     100
      1097:  370    0     100
      1098: 8210    0     100
      1099: 5022    0     100
      1100: 6982    0     100

# Test replace FALSE

    Code
      cc_match(test_data, id = "id", case_indicator = "case", matching_factors = list(
        sex = "exact", time = function(x, y) y >= x), no_controls = 10, seed = 10,
      replace = FALSE, verbose = FALSE)
    Output
              id case riskset
         1:    1    1       1
         2: 3671    0       1
         3:  167    0       1
         4: 1491    0       1
         5: 1941    0       1
        ---                  
      1096: 8256    0     100
      1097:  370    0     100
      1098: 8210    0     100
      1099: 5022    0     100
      1100: 6982    0     100

