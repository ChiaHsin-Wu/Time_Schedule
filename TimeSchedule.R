init_vars <- function(N) {
  list(
    in_degree = rep(0, N),
    out_edges = vector("list", N),
    visited = rep(FALSE, N),
    start_time = rep(0, N)
  )
}

prepare_data <- function(task_times, dependencies = NULL) {
  N <- length(task_times)
  vars <- init_vars(N)
  if (!is.null(dependencies)) {
    for (i in 1:nrow(dependencies)) {
      pre <- dependencies[i, 1]
      post <- dependencies[i, 2]
      vars$out_edges[[pre]] <- append(vars$out_edges[[pre]], post)
      vars$in_degree[post] <- vars$in_degree[post] + 1
    }
  }
  
  list(
    N = N,
    task_times = task_times,
    in_degree = vars$in_degree,
    out_edges = vars$out_edges,
    visited = vars$visited,
    start_time = vars$start_time,
    dependencies = dependencies
  )
}

solve_schedule <- function(data) {
  queue <- which(data$in_degree == 0)
  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    if (data$visited[node]) next
    data$visited[node] <- TRUE
    if (is.null(data$out_edges[[node]]) || length(data$out_edges[[node]]) == 0) next
    for (v in data$out_edges[[node]]) {
      if ((data$start_time[node] + data$task_times[node]) > data$start_time[v]) {
        data$start_time[v] <- data$start_time[node] + data$task_times[node]
      }
      data$in_degree[v] <- data$in_degree[v] - 1
      if (data$in_degree[v] == 0) {
        queue <- c(queue, v)
      }
    }
  }
  total_time <- max(data$start_time + data$task_times)
  
  list(start_time = data$start_time, total_time = total_time)
}

plot_schedule <- function(start_time, task_times, task_names = NULL) {
  N <- length(task_times)
  if (is.null(task_names)) task_names <- paste("Task", 1:N)
  task_order <- order(start_time)
  x_left <- start_time[task_order]
  x_right <- x_left + task_times[task_order]
  labels <- task_names[task_order]
  lanes <- c()
  y_positions <- integer(N)
  for (i in seq_len(N)) {
    available_lanes <- which(x_left[i] >= lanes)
    if (length(available_lanes) == 0) {
      lanes <- c(lanes, x_right[i])
      y_positions[i] <- length(lanes)
    } else {
      chosen_lane <- available_lanes[which.min(lanes[available_lanes])]
      y_positions[i] <- chosen_lane
      lanes[chosen_lane] <- x_right[i]
    }
  }
  
  plot(
    NULL, xlim = c(0, max(x_right) + 0.5), ylim = c(0.5, max(y_positions) + 0.5),
    xlab = "時間 (hr)", ylab = "", yaxt = "n", main = "任務排程圖"
  )
  title(ylab = "任務", line = 1)
  xticks <- seq(0, max(x_right), by = 1)
  axis(side = 1, at = xticks)
  abline(v = xticks, col = "gray", lty = 2)
  for (i in seq_len(N)) {
    y <- y_positions[i]
    rect(x_left[i], y - 0.3, x_right[i], y + 0.3, col = "#b7c0ee", border = "#7067cf", lwd = 1.2)
    text((x_left[i] + x_right[i]) / 2, y + 0.2, labels[i], font = 2, cex = 0.9, col = "#330c2f")
    text((x_left[i] + x_right[i]) / 2, y - 0.2, paste0(task_times[task_order[i]], "小時"), cex = 0.7, col = "#330c2f")
  }
}

# -------------------------------
# 範例使用
# -------------------------------
task_times <- c(1, 6, 3, 8, 3)
dependencies <- matrix(c(1, 4, 4, 2, 3, 4, 4, 5), ncol = 2, byrow = TRUE)
task_names <- c("吃飯", "上課", "看影片", "睡覺", "寫作業")
input_data <- prepare_data(task_times, dependencies)
result <- solve_schedule(input_data)
plot_schedule(result$start_time, task_times, task_names)
cat("完成所有任務最短時間: ", result$total_time, "小時\n")

