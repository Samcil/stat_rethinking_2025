test_that("build_animation_frames combines frames correctly", {
  frame1 <- data.frame(x = 1:3, y = 1:3)
  frame2 <- data.frame(x = 4:6, y = 4:6)
  frame3 <- data.frame(x = 7:9, y = 7:9)
  
  result <- build_animation_frames(list(frame1, frame2, frame3))
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 9)
  expect_true("frame" %in% names(result))
  expect_equal(unique(result$frame), c(1, 2, 3))
  expect_equal(sum(result$frame == 1), 3)
  expect_equal(sum(result$frame == 2), 3)
  expect_equal(sum(result$frame == 3), 3)
  
  # Test custom frame variable name
  result2 <- build_animation_frames(list(frame1, frame2), frame_var = "time")
  expect_true("time" %in% names(result2))
  expect_false("frame" %in% names(result2))
  
  # Test input validation
  expect_error(build_animation_frames(list()))
  expect_error(build_animation_frames(list(1, 2, 3)))
})


test_that("create_animation_sequence generates correct sequences", {
  # Test linear sequence
  seq <- create_animation_sequence(0, 10, n_frames = 11)
  expect_length(seq, 11)
  expect_equal(seq[1], 0)
  expect_equal(seq[11], 10)
  
  # Test that it's evenly spaced for linear ease
  diffs <- diff(seq)
  expect_true(all(abs(diffs - diffs[1]) < 1e-10))
  
  # Test with easing function
  seq_eased <- create_animation_sequence(0, 10, n_frames = 50, 
                                        ease_function = function(x) x^2)
  expect_length(seq_eased, 50)
  expect_equal(seq_eased[1], 0)
  expect_equal(seq_eased[50], 10)
  # Eased sequence should not be linear
  diffs_eased <- diff(seq_eased)
  expect_false(all(abs(diffs_eased - diffs_eased[1]) < 1e-3))
  
  # Test reverse sequence
  seq_rev <- create_animation_sequence(10, 0, n_frames = 11)
  expect_equal(seq_rev[1], 10)
  expect_equal(seq_rev[11], 0)
  
  # Test input validation
  expect_error(create_animation_sequence(0, 10, n_frames = 1))
  expect_error(create_animation_sequence("a", 10))
})


test_that("prepare_path_animation creates animation-ready data", {
  library(dplyr)
  
  # Create simple path data
  paths <- tibble(
    x = c(1:5, 1:5),
    y = c(1:5, 5:1),
    path_id = rep(c(1, 2), each = 5)
  )
  
  result <- prepare_path_animation(paths, n_frames = 10)
  
  expect_s3_class(result, "tbl_df")
  expect_true("frame" %in% names(result))
  expect_equal(max(result$frame), 10)
  
  # Check that early frames have fewer points
  frame1_rows <- nrow(result[result$frame == 1, ])
  frame10_rows <- nrow(result[result$frame == 10, ])
  expect_true(frame1_rows <= frame10_rows)
  
  # Test without path_id column
  paths_no_id <- tibble(x = 1:10, y = 1:10)
  result2 <- prepare_path_animation(paths_no_id)
  expect_s3_class(result2, "tbl_df")
  
  # Test custom variable names
  paths_custom <- tibble(
    longitude = 1:5,
    latitude = 1:5,
    trajectory = 1
  )
  result3 <- prepare_path_animation(
    paths_custom, 
    x_var = "longitude", 
    y_var = "latitude",
    path_id_var = "trajectory"
  )
  expect_s3_class(result3, "tbl_df")
  
  # Test input validation
  expect_error(prepare_path_animation(data.frame()))
  expect_error(prepare_path_animation(paths, x_var = "nonexistent"))
})


test_that("create_discrete_frames handles categorical states", {
  library(dplyr)
  
  # Create data with states
  data <- tibble(
    state = c("A", "A", "B", "B", "C", "C"),
    x = 1:6,
    y = 6:1
  )
  
  result <- create_discrete_frames(data, "state")
  
  expect_s3_class(result, "tbl_df")
  expect_true("frame" %in% names(result))
  
  # Should have 3 frames (one per state)
  expect_equal(max(result$frame), 3)
  
  # Test with repeat_each
  result2 <- create_discrete_frames(data, "state", repeat_each = 3)
  expect_equal(max(result2$frame), 9)  # 3 states × 3 repeats
  
  # Each state should appear in consecutive frames
  frame1_state <- unique(result2$state[result2$frame == 1])
  expect_length(frame1_state, 1)
  
  # Test input validation
  expect_error(create_discrete_frames(data.frame(), "state"))
  expect_error(create_discrete_frames(data, "nonexistent"))
  expect_error(create_discrete_frames(data, "state", repeat_each = 0))
})
