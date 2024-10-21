library(magick)

generate_shape <- function(type, width, height, color, transparency) {
  if (type == "rectangle") {
    shape <- image_blank(width = width, height = height, color = adjustcolor(color, alpha.f = transparency))
  } else if (type == "circle") {
    shape <- image_blank(width = width, height = height, color = "none")
    shape <- image_draw(shape)
    symbols(x = width / 2, y = height / 2, circles = min(width, height) / 2, bg = adjustcolor(color, alpha.f = transparency), inches = FALSE, add = TRUE)
    dev.off()
  }
  return(shape)
}

add_pattern <- function(shape, width, height) {
  for (j in seq(1, width, by = 10)) {
    line_color <- sample(colors, 1)
    shape <- image_draw(shape)
    segments(x0 = j, y0 = 0, x1 = j, y1 = height, col = line_color, lwd = 2)
    segments(x0 = 0, y0 = j, x1 = width, y1 = j, col = line_color, lwd = 2)
    dev.off()
  }
  return(shape)
}

canvas_width <- 800
canvas_height <- 800
num_shapes <- 150
shape_types <- c("rectangle", "circle")
colors <- c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#FF69B4", "#8A2BE2", "#00FF7F", "#FF4500")

canvas <- image_blank(width = canvas_width, height = canvas_height, color = "white")

set.seed(42)
radius <- 300
for (i in 1:num_shapes) {
  angle <- runif(1, min = 0, max = 2 * pi)
  x <- canvas_width / 2 + radius * cos(angle)
  y <- canvas_height / 2 + radius * sin(angle)
  width <- runif(1, min = 50, max = 100)
  height <- runif(1, min = 50, max = 100)
  shape_type <- sample(shape_types, 1)
  color <- sample(colors, 1)
  transparency <- runif(1, min = 0.3, max = 1.0)

  shape <- generate_shape(shape_type, width, height, color, transparency)
  shape <- add_pattern(shape, width, height)

  canvas <- image_composite(canvas, shape, offset = sprintf("+%d+%d", as.integer(x), as.integer(y)))
}

for (i in 1:(num_shapes - 1)) {
  angle1 <- runif(1, min = 0, max = 2 * pi)
  angle2 <- runif(1, min = 0, max = 2 * pi)
  x1 <- canvas_width / 2 + radius * cos(angle1)
  y1 <- canvas_height / 2 + radius * sin(angle1)
  x2 <- canvas_width / 2 + radius * cos(angle2)
  y2 <- canvas_height / 2 + radius * sin(angle2)

  line_color <- sample(colors, 1)
  canvas <- image_draw(canvas)
  segments(x0 = x1, y0 = y1, x1 = x2, y1 = y2, col = line_color, lwd = 1)
  dev.off()
}

save_path <- "enhanced_art_with_patterns.png"

image_write(canvas, path = save_path, format = "png")

browseURL(save_path)

cat("Bild wurde gespeichert unter:", save_path, "\n")
