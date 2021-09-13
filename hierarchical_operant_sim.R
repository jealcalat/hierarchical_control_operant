library(labeling)
library(grid)


hierarchical_ <- function(ns = 1, # Set number of sessions (only the last of the sessions will be plotted)
                          sp = 100, # Lever-press limit for a session
                          tl = 3600, # Elapsed-time limit for a session
                          st = 0, # Session time (time until the nth pellet is earned)
                          d = 1, # Drive (at the start of the session)
                          v = 0.001, # Associative Strength (of feedback)
                          cp = 0, # Cumulative pellets received (in the current session)
                          np = 1, # Number of pellets received
                          r = 0.002 # Learning rate
) {
  
  # Time to response outcome (lever press; reciprocal of response rate)
  t <- 1 / (d * v)
  # Duration of pellet handling and consuming
  pt <- np * 2.5 * (1 / d)
  
  # Variables to be plotted - prealocation
  satiety_signal <- drive_decay <- cummulative_record <- rep(NA, sp) 
  latency <- associative_gain <- prediction_error <- rep(NA, sp) 
  
  for (j in 1:ns) {
    # Reset values of session initial variables
    st <- 0
    d <- 1
    cp <- 0
    t <- 1 / (d * v)
    for (i in 1:sp) {
      latency[i] <- t
      # Error correction mechanism via a simple delta rule weighted by drive and learning rate
      delta <- d * r * (1 + log(np, base = 10) - v)
      prediction_error[i] <- 1 + log(np, base = 10) - v
      v <- v + delta
      associative_gain[i] <- v
      cummulative_record[i] <- st
      st <- st + latency[i]
      cp <- cp + np
      # Drive decay is modeled by a stretched exponential function
      d <- exp(-0.0001 * cp^(5 / 3))
      satiety_signal[i] <- 1 - (exp(-0.0001 * cp^(5 / 3)))
      drive_decay[i] <- d
      pt <- np * 2.5 * (1 / d)
      # Time to the next response outcome
      t <- 1 / (d * v) + pt
      exit <- 0
      if (st > tl) {
        exit <- 1
        break
      }
    }
  }
  
  time <- st
  lever <- i
  pellets <- cp
  
  if (exit == 1) {
    time <- tl
    lever <- i - 1
    pellets <- cp - np
  }
  
  data_inset_params <- data.frame(
    session_time = time / 60,
    lever_rate = lever / (time / 60),
    pellets = pellets,
    lever = lever,
    ns = ns,
    st = st,
    plot_stop = i
  )
  
  
  plot_data <- data.frame(
    associative_gain = associative_gain,
    prediction_error = prediction_error,
    cummulative_record = cummulative_record,
    drive_decay = drive_decay
  )
  
  list(
    plot_data  = plot_data,
    data_inset_params = data_inset_params
  )
  
}

n_sessions <- c(1, 3, 9, 27)
np <- c(1, 10)
# directory to save the plots
path_save <- "~/Dropbox/2021B/JEAB_SNS_feedback/"

for (k in np){
  cairo_pdf(
    sprintf(
      "%shierarchical_%d_pellets.pdf", path_save, k
    ), width = 6.5, height = 6
  )
  par(
    mar = c(2, 1.5, 1, 0),
    oma = c(2, 3.8, 4, 2),
    mgp = c(2, 0.5, 0),
    las = 1,
    tck = -0.02,
    family = 'Latin Modern Sans', # if error, change this font for "s
    cex.lab = 1.2
  )
  plt_layout <- matrix(c(1:12), ncol = 4)
  layout(plt_layout)
  
  for (i in n_sessions) {
    # simulate for i sessions and k pellets
    h_data <- hierarchical_(
      ns = i,
      np = k
      # all other parameters are by default
    )
    max_x <- length(na.omit(h_data$plot_data$associative_gain))
    max_y <- (1 + log(k, base = 10))
    st <- h_data$data_inset_params$st
    # associative gain

    plot(
      na.omit(h_data$plot_data$associative_gain)/max_y,
      ylim = c(0, max_y)/max_y,
      xlim = c(0, max_x), #max_x + 10 - max_x %% 10),
      axes = FALSE,
      ylab = "", #ifelse(i == 1, "Associative gain", "J"),
      xlab = "",# "Trials",
      type = 'l',
      lwd = 2.5,
      col = 'blue',
      panel.first = {
        box()
        lb <- c(as.character(seq(0, 1 - 0.25, 0.25)), paste0(max_y/max_y, "*lambda"))
        if (i == 1){
          axis(2, at = seq(0, 1, 0.25), 
               labels = parse(text = lb), 
               lwd = 1.5, col = "gray40", col.axis = "gray40")
        }
        axis(1, at = matplotlib(1, ceiling(max_x), 4), lwd = 1.5, col = "gray40", col.axis = "gray40")
      }
    )
    
    # cum responses
    plot(
      h_data$plot_data$cummulative_record[1:max_x], 
      1:max_x,
      xlim = c(0, st),
      ylim = c(0, 100),#max_x + 100 - max_x %% 100),
      type = 'l',
      lwd = 2.5,
      col = 'blue',
      axes = FALSE,
      ylab = "", #ifelse(i==1, "Cumulative responses", ""),
      xlab = "", #  "Time",
      panel.first = {
        box()
        axis(1,
             labels = FALSE,
             # tick = FALSE,
             # line = 1,
             tck = 0,
             at = matplotlib(0, ceiling(st), 4), 
             lwd = 1.5,
             col = "gray40", col.axis = "gray40")
        if(i==1){
          axis(2, at = matplotlib(1, 100, 4), lwd = 1.5, col = "gray40", col.axis = "gray40")
        }

      }
    )
    # mtext(side=1, line=0.5, "Time", cex=1)
    mtext(
      sprintf(
        " response rate = %0.2f",h_data$data_inset_params$lever_rate
      ), side = 3, line = -1.4, cex = 0.74, col = "red3", adj = 0
    )
    # drive decay
    
    plot(
      h_data$plot_data$drive_decay,
      ylim = c(0, 1),
      xlim = c(0, max_x + 10 - max_x %% 10),
      # xlim = c(0, st),
      type = 'l',
      lwd = 2.5,
      col = 'blue',
      axes = FALSE,
      ylab = "",#ifelse(i==1, "Drive decay", ""),
      xlab = "",#"Trials",
      panel.first = {
        box()
        axis(1, at = matplotlib(0, max_x, 4), lwd = 1.5, col = "gray40", col.axis = "gray40")
        if(i==1){
          axis(2, at = matplotlib(0, 1, 4), lwd = 1.5, col = "gray40", col.axis = "gray40")
        }

      }
    )
    mtext(
      sprintf(
        " #pellets\n consumed = %d",h_data$data_inset_params$pellets
      ), side = 3, line = -8.3, cex = 0.72, col = "red3", adj = 0, padj = 1
    )
    
  }
  grid.text(
    label = paste(n_sessions, "° Session", sep=""),
    x = c(0.19, 0.41, 0.63, 0.85)+0.01,
    y = rep(0.95, 4) - 0.031,
    gp = gpar(fontsize = 13, fontfamily = 'Latin Modern Sans', fontface = 1)
  )
  
  grid.text(
    label = c("Trials", "Time", "Trials"),
    x = rep(0.54, 3),
    y = c(0.675, 0.405, 0.09) - 0.05,
    gp = gpar(fontsize = 12, fontfamily = 'Latin Modern Sans')
  )
  
  grid.text(
    label = sprintf("%d pellet per lever press", k),
    x = 0.54,
    y = 0.97, 
    gp = gpar(fontsize = 14, fontfamily = 'Latin Modern Sans', fontface = 2)
  )
  
  grid.text(
    label = c("Associative\n gain", "Cumulative\n Responses", "Drive Decay"),
    x = rep(0.028, 3),
    y = c(0.65, 0.35, 0.06) + 0.15,
    rot = 90,
    gp = gpar(fontsize = 12, fontfamily = 'Latin Modern Sans', lineheight = 0.9)
  )
  
  dev.off()
  
}




