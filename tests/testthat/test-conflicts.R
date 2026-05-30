test_that("intentional actisensorlog re-exports are ignored", {
  expect_false(
    activerse:::activerse_conflict_should_report(
      "acti_read_sensorlog",
      c("package:actiread", "package:actisensorlog")
    )
  )

  expect_false(
    activerse:::activerse_conflict_should_report(
      "acti_summarize_sensorlog",
      c("package:actimetrics", "package:actisensorlog")
    )
  )
})

test_that("unexpected activerse conflicts are still reported", {
  expect_true(
    activerse:::activerse_conflict_should_report(
      "some_other_function",
      c("package:actiread", "package:actisensorlog")
    )
  )
})

test_that("activerse conflict winner follows attach precedence", {
  expect_identical(
    activerse:::activerse_conflict_winner(c("actiread", "actisensorlog")),
    "actisensorlog"
  )

  expect_identical(
    activerse:::activerse_conflict_winner(c("actimetrics", "actisensorlog")),
    "actisensorlog"
  )
})
