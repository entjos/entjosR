require(rstpm2)
data("brcancer")

test_that("No of event cannot be calcualted without event argument", {
  expect_error({
    summary_table(brcancer,
                  vars      = c("x2"),
                  strata    = c("hormon"),
                  get_no_events = TRUE)
  }, regexp = "computing the number of events")
})

test_that("Event indicator that is not 0/1 coded", {
  expect_message({

    brcancer$x2 <- as.factor(brcancer$x2)
    brcancer$censrec[[1]] <- 5

    summary_table(brcancer,
                  vars      = c("x2"),
                  strata    = c("hormon"),
                  get_rates = TRUE,
                  event     = "censrec",
                  st        = "rectime")

  }, regexp = "The event variable is not 0/1 coded.")
})

test_that("Summary table works with numeric values", {
  expect_snapshot({
    summary_table(brcancer,
                  vars      = c("x1"))
  })
})

test_that("Summary table works with factor values", {
  expect_snapshot({

    brcancer$x4 <- as.factor(brcancer$x4)

    summary_table(brcancer,
                  vars      = c("x4"))
  })
})

test_that("Summary table works with binary numeric values", {
  expect_snapshot({

    summary_table(brcancer,
                  vars      = c("hormon"))
  })
})

test_that("Summary table works with strata", {
  expect_snapshot({

    summary_table(brcancer,
                  vars   = c("x7"),
                  strata = "hormon")
  })
})

test_that("Summary table works with rates", {
  expect_snapshot({

    brcancer$x2 <- as.factor(brcancer$x2)

    summary_table(brcancer,
                  vars      = c("x2"),
                  strata    = c("hormon"),
                  get_rates = TRUE,
                  event     = "censrec",
                  st        = "rectime")
  })
})

test_that("Summary table works with no events", {
  expect_snapshot({

    brcancer$x2 <- as.factor(brcancer$x2)

    summary_table(brcancer,
                  vars      = c("x2"),
                  strata    = c("hormon"),
                  event     = "censrec",
                  get_no_events = TRUE,
                  overall = TRUE)
  })
})
