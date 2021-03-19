library(agricolae)

# Randomised Complete Block Design
trt <- LETTERS[1:11]
rep <- 4
outdesign_rcbd_plot <- design.rcbd(trt = trt, r = rep, seed = 42)

# Randomised Complete Block Design2
trt <- LETTERS[1:6]
rep <- 4
outdesign_rcbd_plot2 <- design.rcbd(trt = trt, r = rep, seed = 42)

# Randomised Complete Block Design2
trt <- LETTERS[1:6]
rep <- 4
outdesign_rcbd_plot3 <- design.rcbd(trt = trt, r = rep, seed = 42)

# Latin Square Design
trt <- c("S1", "S2", "S3", "S4")
outdesign_lsd_plot <- design.lsd(trt, seed = 42)

# Factorial Design (Crossed, Completely Randomised)
trt <- c(3, 2) # Factorial 3 x 2
rep <- 3
outdesign_crossed_plot <- design.ab(trt, r = rep, design = "crd", seed = 42)

# Factorial Design (Crossed, RCBD)
trt <- c(3, 2) # Factorial 3 x 2
rep <- 3
outdesign_crossed_rcbd_plot <- design.ab(trt, r = rep, design = "rcbd", seed = 42)

trt <- c(3, 2) # Factorial 3 x 2
rep <- 3
outdesign_crossed_rcbd_plot_2 <- design.ab(trt, r = rep, design = "rcbd", seed = 42)

trt <- c(3, 2) # Factorial 3 x 2
rep <- 4
outdesign_crossed_rcbd_plot_3 <- design.ab(trt, r = rep, design = "rcbd", seed = 42)

# Factorial Design (Crossed, LSD)
trt <- c(3, 2) # Factorial 3 x 2
rep <- 3
outdesign_crossed_lsd_plot <- design.ab(trt, r = rep, design = "lsd", seed = 42)

# Factorial Design (Nested, Latin Square)
trt <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3")
outdesign_nested_plot <- design.lsd(trt, seed = 42)

# Split plot design
trt1 <- c("A", "B")
trt2 <- 1:4
rep <- 4
outdesign_split_plot <- design.split(trt1, trt2, r = rep, seed = 42)
