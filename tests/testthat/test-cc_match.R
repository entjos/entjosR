# Create test dataset
test_data <- data.frame(id = 1:10000,
                        case = c(rep(1, 100), rep(0, 9900)),
                        time = rep(1:2, 5000),
                        sex  = rep(0:1, 5000),
                        letters = rep(c("A", "B"), 5000))

test_that("Test replace TRUE", {
  expect_snapshot({
    cc_match(cases    = test_data[test_data$case == 1, ],
             controls = test_data[test_data$case == 0, ],
             id_name = "id",
             by = list(sex = 'exact',
                       time = function(x, y) y >= x),
             no_controls = 10,
             seed = 10,
             verbose = FALSE) |>
      as.data.table()
  })
})

test_that("Test replace FALSE", {

  expect_snapshot({
    cc_match(cases    = test_data[test_data$case == 1, ],
             controls = test_data[test_data$case == 0, ],
             id_name = "id",
             by = list(sex = 'exact',
                       time = function(x, y) y >= x),
             no_controls = 10,
             seed = 10,
             replace = FALSE,
             verbose = FALSE) |>
      as.data.table()
  })
})

test_that("Test replace FALSE with few comparators", {
  expect_true({
    suppressWarnings({
      temp <- cc_match(cases    = test_data[test_data$case == 1, ],
                       controls = test_data[test_data$case == 0, ],
                       id_name = "id",
                       by = list(sex = 'exact',
                                 time = function(x, y) y >= x),
                       no_controls = 10,
                       seed = 10,
                       replace = FALSE,
                       verbose = FALSE)

      identical(sum(temp$case == 0),
                length(unique(temp$id[temp$case == 0])))
    })
  })
})

test_that("All matching varaible are the same in risksets", {
  expect_equal({
    suppressWarnings({
      temp <- cc_match(cases    = test_data[test_data$case == 1, ],
                       controls = test_data[test_data$case == 0, ],
                       id_name = "id",
                       by = list(sex = 'exact',
                                 time = function(x, y) y >= x),
                       no_controls = 10,
                       seed = 10,
                       replace = FALSE,
                       verbose = FALSE,
                       return_case_values = TRUE) |>
        as.data.table()

      max(temp[, sum(var(case_sex), var(case_time)), by = riskset]$V1)

    })
  }, 0)
})

test_that("Test matching of character vectors", {

  expect_snapshot({
    cc_match(cases    = test_data[test_data$case == 1, ],
             controls = test_data[test_data$case == 0, ],
             id_name = "id",
             by = list(sex = 'exact',
                       time = function(x, y) y >= x,
                       letters = 'exact'),
             no_controls = 10,
             seed = 10,
             replace = FALSE,
             verbose = FALSE,
             return_case_values = TRUE) |>
      as.data.table()
  })
})

test_that("Test never match the same individual as the case", {

  expect_equal({
    suppressWarnings({
      temp <- cc_match(cases    = test_data[test_data$case == 1, ],
                       controls = test_data[test_data$case == 1, ],
                       id_name = "id",
                       by = list(sex = 'exact',
                                 time = function(x, y) y >= x,
                                 letters = 'exact'),
                       no_controls = 10,
                       seed = 10,
                       replace = TRUE,
                       verbose = FALSE,
                       return_case_values = TRUE) |>
        as.data.table()

      temp[case == 1, case_id := id]
      setnafill(temp, type = "locf", cols = "case_id")
      temp <- temp[, sum(id == case_id), by = riskset]

      max(temp$V1)

    })
  }, 1)
})

