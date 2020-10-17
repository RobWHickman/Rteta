context("Standardization of coordinates for Opta data")
library(Rteta)

m_to_yd <- 1.09361

test_that("opta goal line is mapped to standard goal line", {
  expect_equal(standardize_opta_x(data.frame(location_x = c(0, 100), PassEndX =  c(0, 100), BlockedX = c(0, 100)), unit = "meters"),
                                  data.frame(location_x = c(0, 105), PassEndX =  c(0, 105), BlockedX = c(0, 105)))
  expect_equal(standardize_opta_x(data.frame(location_x = c(0, 100), PassEndX =  c(0, 100), BlockedX = c(0, 100)), unit = "yards"),
               data.frame(location_x = c(0, 105*m_to_yd), PassEndX =  c(0, 105*m_to_yd), BlockedX = c(0, 105*m_to_yd)),
               tolerance = .0001)
})

test_that("opta sideline is mapped to standard sideline", {
  expect_equal(standardize_opta_y(data.frame(location_y = c(0, 100), PassEndY =  c(0, 100), BlockedY = c(0, 100), GoalMouthY = c(0, 100)), unit = "meters"),
               data.frame(location_y = c(0, 68), PassEndY =  c(0, 68), BlockedY = c(0, 68), GoalMouthY = c(0, 68)))
  expect_equal(standardize_opta_y(data.frame(location_y = c(0, 100), PassEndY =  c(0, 100), BlockedY = c(0, 100), GoalMouthY = c(0, 100)), unit = "yards"),
               data.frame(location_y = c(0, 68*m_to_yd), PassEndY =  c(0, 68*m_to_yd), BlockedY = c(0, 68*m_to_yd), GoalMouthY = c(0, 68*m_to_yd)),
               tolerance = .0001)
})

test_that("opta half line is mapped to standard half line", {
  expect_equal(standardize_opta_x(data.frame(location_x = c(50), PassEndX =  c(50), BlockedX = c(50)), unit = "meters"),
               data.frame(location_x = c(105/2), PassEndX =  c(105/2), BlockedX = c(105/2)))
  expect_equal(standardize_opta_x(data.frame(location_x = c(50), PassEndX =  c(50), BlockedX = c(50)), unit = "yards"),
               data.frame(location_x = c(105/2*m_to_yd), PassEndX =  c(105/2*m_to_yd), BlockedX = c(105/2*m_to_yd)),
               tolerance = .0001)
  })

test_that("opta penalty spot is mapped to standard penalty spot", {
  expect_equal(standardize_opta_x(data.frame(location_x = c(11.5, 88.5), PassEndX =  c(11.5, 88.5), BlockedX = c(11.5, 88.5)), unit = "meters"),
               data.frame(location_x = c(0 + 10.97, 105 - 10.97), PassEndX =  c(0 + 10.97, 105 - 10.97), BlockedX = c(0 + 10.97, 105 - 10.97)))
  expect_equal(standardize_opta_y(data.frame(location_y = c(50), PassEndY =  c(50), BlockedY = c(50), GoalMouthY = c(50)), unit = "meters"),
               data.frame(location_y = c(34), PassEndY =  c(34), BlockedY = c(34), GoalMouthY = c(34)))
  expect_equal(standardize_opta_x(data.frame(location_x = c(11.5, 88.5), PassEndX =  c(11.5, 88.5), BlockedX = c(11.5, 88.5)), unit = "yards"),
               data.frame(location_x = c(0 + 12, 105*m_to_yd - 12), PassEndX =  c(0 + 12, 105*m_to_yd - 12), BlockedX = c(0 + 12, 105*m_to_yd - 12)),
               tolerance = .0001)
  expect_equal(standardize_opta_y(data.frame(location_y = c(50), PassEndY =  c(50), BlockedY = c(50), GoalMouthY = c(50)), unit = "yards"),
               data.frame(location_y = c(34*m_to_yd), PassEndY =  c(34*m_to_yd), BlockedY = c(34*m_to_yd), GoalMouthY = c(34*m_to_yd)),
               tolerance = .0001)
})
