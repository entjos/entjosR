# Test replace TRUE

    Code
      as.data.table(cc_match(cases = test_data[test_data$case == 1, ], controls = test_data[
        test_data$case == 0, ], id_name = "id", by = list(sex = "exact", time = function(
        x, y) y >= x), no_controls = 10, seed = 10, verbose = FALSE))
    Output
               id  case riskset
            <int> <num>   <int>
         1:     1     1       1
         2:  3671     0       1
         3:   167     0       1
         4:  1491     0       1
         5:  1941     0       1
        ---                    
      1096:  2954     0     100
      1097:  7460     0     100
      1098:   344     0     100
      1099:  7416     0     100
      1100:  4518     0     100

# Test replace FALSE

    Code
      as.data.table(cc_match(cases = test_data[test_data$case == 1, ], controls = test_data[
        test_data$case == 0, ], id_name = "id", by = list(sex = "exact", time = function(
        x, y) y >= x), no_controls = 10, seed = 10, replace = FALSE, verbose = FALSE))
    Output
               id  case riskset
            <int> <num>   <int>
         1:     1     1       1
         2:  3671     0       1
         3:   167     0       1
         4:  1491     0       1
         5:  1941     0       1
        ---                    
      1096:  8256     0     100
      1097:   370     0     100
      1098:  8210     0     100
      1099:  5022     0     100
      1100:  6982     0     100

# Test matching of character vectors

    Code
      as.data.table(cc_match(cases = test_data[test_data$case == 1, ], controls = test_data[
        test_data$case == 0, ], id_name = "id", by = list(sex = "exact", time = function(
        x, y) y >= x, letters = "exact"), no_controls = 10, seed = 10, replace = FALSE,
      verbose = FALSE, return_case_values = TRUE))
    Output
               id  case riskset case_sex case_time case_letters
            <int> <num>   <int>    <int>     <int>       <char>
         1:     1     1       1        0         1            A
         2:  3671     0       1        0         1            A
         3:   167     0       1        0         1            A
         4:  1491     0       1        0         1            A
         5:  1941     0       1        0         1            A
        ---                                                    
      1096:  8256     0     100        1         2            B
      1097:   370     0     100        1         2            B
      1098:  8210     0     100        1         2            B
      1099:  5022     0     100        1         2            B
      1100:  6982     0     100        1         2            B

