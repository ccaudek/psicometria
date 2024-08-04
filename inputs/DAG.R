library(igraph)

# Function to create a graph and plot it
plot_graph <- function(edges, vertex_shapes, vertex_labels, layout, title) {
  g <- graph(edges, directed = TRUE)
  
  V(g)$shape <- vertex_shapes
  V(g)$label <- vertex_labels
  V(g)$size <- 40
  V(g)$color <- "white"
  V(g)$frame.color <- "black"
  E(g)$arrow.size <- 0.75
  
  plot(g, layout = layout, main = title, vertex.label.cex = 2.0)
}

# Set up the plotting area for 3 plots
par(mfrow = c(1, 3), mar = c(1, 1, 1, 1))

# Graph 1
edges1 <- c("X", "Y", "U", "Y")
vertex_shapes1 <- c("square", "square", "circle")
vertex_labels1 <- c("X", "Y", "U")
layout1 <- matrix(c(0, 0, 1, 0, 0.5, 1), ncol = 2, byrow = TRUE)
plot_graph(edges1, vertex_shapes1, vertex_labels1, layout1, "A")

# Graph 2
edges2 <- c("X", "Y", "U", "X", "U", "Y")
vertex_shapes2 <- c("square", "square", "circle")
vertex_labels2 <- c("X", "Y", "U")
layout2 <- matrix(c(0, 0, 1, 0, 0.5, 1), ncol = 2, byrow = TRUE)
plot_graph(edges2, vertex_shapes2, vertex_labels2, layout2, "B")

# Graph 3
edges3 <- c("X", "Y", "U", "Y", "Z", "X", "Z", "U")
vertex_shapes3 <- c("square", "square", "circle", "circle")
vertex_labels3 <- c("X", "Y", "U", "Z")
layout3 <- matrix(c(0, 0, 1, 0, 0.75, 1, 0.25, 1), ncol = 2, byrow = TRUE)
plot_graph(edges3, vertex_shapes3, vertex_labels3, layout3, "C")

