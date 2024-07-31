require(rstpm2)
data("brcancer")

test_that("Stable output", {
  expect_snapshot({
    fpm_test_dfs(Surv(surv_mm, status == "Dead: cancer") ~ age + year8594,
                 dfs_bh  = 1:3,
                 dfs_tvc = list(age      = 1:3,
                                year8594 = 1:3),
                 by_vars = c("sex", "stage"),
                 same_dfs_tvc = TRUE,
                 data = colon)
  })
})

test_that("test same_dfs_tvs = false", {
  expect_snapshot({
    fpm_test_dfs(Surv(surv_mm, status == "Dead: cancer") ~ age + year8594,
                 dfs_bh  = 1:3,
                 dfs_tvc = list(age      = 1:2),
                 same_dfs_tvc = FALSE,
                 data = colon)
  })
})

