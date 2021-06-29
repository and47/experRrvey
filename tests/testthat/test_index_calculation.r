context("index calculation for EDA")
library(experRrvey)
library(dplyr)
library(forcats)

databite <- tribble(
             ~s_g, ~s_a,
             "M", "10-23",
             "M", "10-23",
             "F", "10-23",
             "F", "24-35",
             "M", "24-35",
             "M", "24-35",
             "M", "24-35",
             "M", "40-50",
             "F", "40-50",
             "F", "40-50")

databite$s_g <- as_factor(databite$s_g)
databite$s_a <- as_factor(databite$s_a)

databiteWidx <- calcIdx(grpVar = "s_g", idxVar = "s_a", databite)

test_that(
 "Index is calculated alright and added in the last idx column", {
  expect_equal(tolerance = 0.1,
               databiteWidx$idx,
               c(111.11111, 125.00000, 55.55556, 83.33333, 62.50000, 166.66667))
  expect_equal(databiteWidx[databiteWidx$s_g == 'M' &
                            databiteWidx$s_a == '24-35', ncol(databiteWidx)]$idx,
               50/40*100)
})

test_that(
 "Index is symmertical, its value is the same if we switch grpVar and idxVar", {
  expect_equal(
    databiteWidx,
    calcIdx(grpVar = "s_a", idxVar = "s_g", databite)[
        ,c('s_g', 's_a', 'idx')] %>% arrange(s_g, s_a)
  )
})




