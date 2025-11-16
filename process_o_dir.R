#
#
# HELPER FUNCTIONS FOR PROCESSING ORIENTATION AND DIRECTION
#
#
rad2deg <- function(rad) {
  return(rad * 180 / pi)
}

principal_angle <- function(v1, v2) {
  # Calculate the norms (magnitudes) of the vectors
  # Note: R's norm() function is for matrices,
  # so we use sqrt(sum(v^2)) or sqrt(v %*% v)
  norm_v1 <- sqrt(sum(v1^2))
  norm_v2 <- sqrt(sum(v2^2))
  
  # Handle the case of zero vectors to avoid division by zero
  if (norm_v1 == 0 || norm_v2 == 0) {
    # The angle is undefined if one vector is zero,
    # but we return 0.0 as a convention.
    return(0.0)
  }
  
  # Calculate the dot product
  # In R, the dot product is v1 %*% v2
  dot_product <- v1 %*% v2
  
  # Calculate the cosine of the angle
  cos_theta <- dot_product / (norm_v1 * norm_v2)
  
  # We clip the value to the range [-1.0, 1.0] to avoid
  # numerical errors (e.g., getting 1.0000001) that
  # would cause acos() to return NaN.
  # We use pmin and pmax for element-wise min/max
  cos_theta_clipped <- pmin(pmax(cos_theta, -1.0), 1.0)
  
  # Calculate the angle in radians using the arccosine
  # acos() returns the angle
  angle_rad <- acos(cos_theta_clipped)
  # The result of acos() might be a 1x1 matrix,
  # so we convert it to a simple numeric value
  return(rad2deg(as.numeric(angle_rad)))
}

process_angle <- function(x, ball_land_x, angle) {
  
  # ifelse() is the vectorized version of if-else.
  # It will check the condition (x > ball_land_x) for every element
  # and return the corresponding element from either (angle + 180) or (angle).
  
  result <- ifelse(x > ball_land_x, angle + 180, angle)
  
  return(result)
  
  # take the difference between angles and make sure its 180 degree calculated
}

angle_difference <- function(input_angle, angle_to_ball) {
  
  # 1. Calculate the absolute difference for all elements at once
  return_angle <- abs(input_angle - angle_to_ball)
  
  # 2. Use ifelse() to check the condition for every element
  # If return_angle > 180, use (360 - return_angle)
  # Otherwise, just use return_angle
  result <- ifelse(return_angle > 180, 360 - return_angle, return_angle)
  
  return(result)
}

#
#
# HERE ARE THE FUNCTIONS TO ACTUALLY PROCESS THE O AND DIR VARIABLES FOR THE INPUT DATASET. JUST PUT IN THE INPUT DATASET AND IT RETURNS
# THE FIXED DIR AND 0 VARIABLES REPECTIVELY.
#
#

process_dir <- function(input){
  x_abs <- abs(input$x - input$ball_land_x)
  y_abs <- abs(input$y - input$ball_land_y)
  
  # Use mapply to iterate over x_abs and y_abs in parallel
  # The anonymous function(x, y) is called for each pair:
  # 1. principal_angle(c(1,0), c(x_abs[1], y_abs[1]))
  # 2. principal_angle(c(1,0), c(x_abs[2], y_abs[2]))
  # ...and so on.
  theta_rad_vector <- mapply(function(x, y) {
    principal_angle(c(1, 0), c(x, y))
  }, x_abs, y_abs)
  
  # Assuming process_angle is vectorized (it can take a vector)
  theta <- process_angle(input$x,input$ball_land_x,theta_rad_vector)
  
  # Assuming angle_difference is vectorized
  return(angle_difference(input$dir, theta))
}

process_o <- function(input){
  x_abs <- abs(input$x - input$ball_land_x)
  y_abs <- abs(input$y - input$ball_land_y)
  
  # Use mapply to iterate over x_abs and y_abs in parallel
  # The anonymous function(x, y) is called for each pair:
  # 1. principal_angle(c(1,0), c(x_abs[1], y_abs[1]))
  # 2. principal_angle(c(1,0), c(x_abs[2], y_abs[2]))
  # ...and so on.
  theta_rad_vector <- mapply(function(x, y) {
    principal_angle(c(1, 0), c(x, y))
  }, x_abs, y_abs)
  
  # Assuming process_angle is vectorized (it can take a vector)
  theta <- process_angle(input$x,input$ball_land_x,theta_rad_vector)
  
  # Assuming angle_difference is vectorized
  return(angle_difference(input$o, theta))
}

