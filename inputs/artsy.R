library(aRtsy)

# Set the seed for reproducibility
set.seed(234233)

# Open a new plotting window with specified dimensions
dev.new(width = 12, height = 3)  # Adjust width and height as needed

# Generate the flame art
canvas_flame(
  colors = colorPalette("origami"), variations = c(1, 2),
  blend = FALSE, weighted = TRUE, iterations = 1e8
)

set.seed(16431)

canvas_forest(colors = colorPalette("azul"))

canvas_blacklight(colors = colorPalette("azul", n = 88))

canvas_chladni(colors = colorPalette("neo3", n = 18))
