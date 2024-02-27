# Create test dataset
test_data <- data.frame(id = 1:10000,
                        case = c(rep(1, 100), rep(0, 9900)),
                        time = rep(1:2, 5000),
                        sex  = rep(0:1, 5000))

test_that("Test replace TRUE", {
  expect_snapshot({
    cc_match(test_data,
             id = "id",
             case_indicator = "case",
             matching_factors = list(sex = 'exact',
                                     time = function(x, y) y >= x),
             no_controls = 10,
             seed = 10,
             verbose = FALSE)
  })
})

test_that("Test replace FALSE", {

  expect_snapshot({
    cc_match(test_data,
             id = "id",
             case_indicator = "case",
             matching_factors = list(sex = 'exact',
                                     time = function(x, y) y >= x),
             no_controls = 10,
             seed = 10,
             replace = FALSE,
             verbose = FALSE)
  })
})

test_that("Test replace FALSE with few comparators", {
  expect_true({
    suppressWarnings({
      temp <- cc_match(test_data[1:1000, ],
                       id = "id",
                       case_indicator = "case",
                       matching_factors = list(sex = 'exact',
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

test_that("Test if no compartors are included in data", {
  expect_error({
    cc_match(test_data[1:10, ],
             id = "id",
             case_indicator = "case",
             matching_factors = list(sex = 'exact',
                                     time = function(x, y) y >= x),
             no_controls = 10,
             seed = 10,
             replace = FALSE,
             verbose = FALSE)
  })
})
