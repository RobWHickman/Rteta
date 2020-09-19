context("Standardization of coordinates for Statsbomb data")
library(Rteta)
library(StatsBombR, quietly = TRUE, warn.conflicts = FALSE)

skip_on_cran() # skip on cran as loading data takes long and we don't want to trigger StatsBomb API too much

m_to_yd <- 1.09361

# Silence verbose StatsBomb warnings
oldw <- getOption("warn")
options(warn = -1)

comp <- FreeCompetitions()
matches <- FreeMatches(comp)
events <- get.matchFree(matches[1,])
events_clean <- allclean(events)

options(warn = oldw)

penalty_events <- events_clean %>%
  filter(shot.type.name == "Penalty")

throw_in_events <- events_clean %>%
  filter(pass.type.name == "Throw-in")

test_that("statsbomb penalty spot is mapped to standard penalty spot", {
  skip_on_cran() # skip on cran as loading data takes long and we don't want to trigger StatsBomb API too much
  expect_equal(standardize_coordinates(penalty_events, provider = "statsbomb", unit = "meters") %>% select(location.x, location.y),
               tibble(location.x = c(105 - 10.97), location.y = c(34)),
               tolerance = .01) #increased tolerance necessary as there is noise in real data
})

test_that("statsbomb throw-ins are mapped to standard side lines", {
  skip_on_cran() # skip on cran as loading data takes long and we don't want to trigger StatsBomb API too much

  throw_in_events <- standardize_coordinates(throw_in_events, provider = "statsbomb", unit = "meters")
  throw_ins_left <- throw_in_events %>% filter(location.y > 34) %>% mutate(location.y_bool = (location.y > 66 & location.y < 70))
  throw_ins_right <- throw_in_events %>% filter(location.y < 34)  %>% mutate(location.y_bool = location.y < 2)
  expect_true(all(throw_ins_left$location.y_bool))
  expect_true(all(throw_ins_right$location.y_bool))
})
