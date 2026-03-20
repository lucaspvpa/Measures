estimate_g_correlation <- function(x, y, train_ratio = 0.6) {
  data <- data.frame(x = x, y = y)
  n <- nrow(data)
  q <- floor(train_ratio * n)
  indices <- sample(1:n, q, replace = FALSE)
  T <- data[indices, ]
  E <- data[-indices, ]
  
  #y_sorted <- sort(T$y)
  #if (q %% 2 == 1) {
  #  y_tilde <- y_sorted[(q + 1) / 2]
  #} else {
  #  y_tilde <- mean(y_sorted[c(q / 2, (q+1)/2)])
  #}
  y_tilde <- median(T$y)
  
  T_sorted <- T[order(T$x), ]
  
  # Candidatos para c: médias entre valores consecutivos de x
  x_values <- T_sorted$x
  c_candidates <- (head(x_values, -1) + tail(x_values, -1)) / 2
  
  best_accuracy <- 0
  best_c <- NA
  q <- nrow(E)
  
  for (c in c_candidates) {
    p1 <- sum(E$x <= c & E$y < y_tilde)
    p2 <- sum(E$x > c & E$y > y_tilde)
    accuracy <- max(p1 + p2, q - (p1 + p2)) / q
    
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_c <- c
    }
  }
  
  omega <- best_accuracy
  
  return(omega)
}
