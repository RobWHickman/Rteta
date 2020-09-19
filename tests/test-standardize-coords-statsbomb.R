context("Standardization of coordinates for Statsbomb data")
library(Rteta)

m_to_yd <- 1.09361

test_that("statsbomb goal line is mapped to standard goal line", {
  expect_equal(
    standardize_statsbomb_x(
      data.frame(location.x = c(0, 120),
                 carry.end_location.x =  c(0, 120),
                 pass.end_location.x = c(0, 120),
                 shot.end_location.x = c(0, 120),
                 location.x.GK = c(0, 120)
      ), unit = "meters"),
    data.frame(location.x = c(0, 105),
               carry.end_location.x =  c(0, 105),
               pass.end_location.x = c(0, 105),
               shot.end_location.x = c(0, 105),
               location.x.GK = c(0, 105)
    ),
    tolerance = .0001)

  expect_equal(
    standardize_statsbomb_x(
      data.frame(location.x = c(0, 120),
                 carry.end_location.x =  c(0, 120),
                 pass.end_location.x = c(0, 120),
                 shot.end_location.x = c(0, 120),
                 location.x.GK = c(0, 120)
      ), unit = "yards"),
    data.frame(location.x = c(0, 105*m_to_yd),
               carry.end_location.x = c(0, 105*m_to_yd),
               pass.end_location.x = c(0, 105*m_to_yd),
               shot.end_location.x = c(0, 105*m_to_yd),
               location.x.GK = c(0, 105*m_to_yd)
    ),
    tolerance = .0001)
})

test_that("statsbomb sideline is mapped to standard sideline and flipped", {
  expect_equal(
    standardize_statsbomb_y(
      data.frame(location.y = c(0, 80),
                 carry.end_location.y =  c(0, 80),
                 pass.end_location.y = c(0, 80),
                 shot.end_location.y = c(0, 80),
                 location.y.GK = c(0, 80)
      ), unit = "meters"),
    data.frame(location.y = c(68, 0),
               carry.end_location.y =  c(68, 0),
               pass.end_location.y = c(68, 0),
               shot.end_location.y = c(68, 0),
               location.y.GK = c(68, 0)
    ),
    tolerance = .0001)

  expect_equal(
    standardize_statsbomb_y(
      data.frame(location.y = c(0, 80),
                 carry.end_location.y =  c(0, 80),
                 pass.end_location.y = c(0, 80),
                 shot.end_location.y = c(0, 80),
                 location.y.GK = c(0, 80)
      ), unit = "yards"),
    data.frame(location.y = c(68*m_to_yd, 0),
               carry.end_location.y =  c(68*m_to_yd, 0),
               pass.end_location.y = c(68*m_to_yd, 0),
               shot.end_location.y = c(68*m_to_yd, 0),
               location.y.GK = c(68*m_to_yd, 0)
    ),
    tolerance = .0001)
})

test_that("statsbomb half line is mapped to standard half line", {
  expect_equal(standardize_statsbomb_x(data.frame(location.x = c(60)), cols = c("location.x"), unit = "meters"),
               data.frame(location.x = c(105/2)),
               tolerance = .0001)
  expect_equal(standardize_statsbomb_x(data.frame(location.x = c(60)), cols = c("location.x"), unit = "yards"),
               data.frame(location.x = c(105/2*m_to_yd)),
               tolerance = .0001)
})

test_that("statsbomb penalty spot is mapped to standard penalty spot", {
  expect_equal(standardize_statsbomb_x(data.frame(location.x = c(12, 108)), cols = c("location.x"), unit = "meters"),
               data.frame(location.x = c(0 + 10.97, 105 - 10.97)),
               tolerance = .0001)
  expect_equal(standardize_statsbomb_y(data.frame(location.y = c(40)), cols = c("location.y"), unit = "meters"),
               data.frame(location.y = c(34)),
               tolerance = .0001)

  expect_equal(standardize_statsbomb_x(data.frame(location.x = c(12, 108)), cols = c("location.x"), unit = "yards"),
               data.frame(location.x = c(0 + 12, 105*m_to_yd - 12)),
               tolerance = .0001)
  expect_equal(standardize_statsbomb_y(data.frame(location.y = c(40)), cols = c("location.y"), unit = "yards"),
               data.frame(location.y = c(34*m_to_yd)),
               tolerance = .0001)

})
