# 
# VIZUALIZACIA
# 
plot_euclidean_lsa_space <- function(word_space = wordspace, doc_space = docspace, query_coords = qr,
                                     doc_labels = T, word_labels = T) {
  plot(t(doc_space))
  points(word_space, col = "red")
  if(word_labels) text(word_space, labels = rownames(word_space), pos = 1)
  if(doc_labels) text(t(doc_space), labels = colnames(doc_space), pos = 1)
  points(t(query_coords), col = "green", pch = 17)
  text(t(query_coords), "query", pos = 1)
}

# nakresli ciary podobnosti medzi dvoma dokumentami alebo slovami
# pri slovach treba maticu transponovat
draw_cossim_lines <- function(v1,v2,space = docspace){
  segments(0,0,space[,v1][1],space[,v1][2])
  segments(0,0,space[,v2][1],space[,v2][2])
}

# circle for cosine space
draw_circle <- function(new_plot = F) {
  if(new_plot)  
    plot(c(-1, 0), c(-1, 1), type = "n")
  # prepare "circle data"
  radius <- 1
  theta <- seq(0, 2 * pi, length = 200)
  # draw the circle
  lines(x = radius * cos(theta), y = radius * sin(theta))
  segments(0,1,0,-1,lty=2,col = "grey")
  segments(-1,0,1,0,lty=2, col = "grey")
}

# help function to two upper functions
# 1 - for word space
# 2 - for document space
add_cosine_space_points <- function(space = docspace, space_type = 2, color = "red", text_labels = T,
                                    pl_fcn = points, lty = 1, ...) { 
  if(class(space)!= "matrix") space <- (as.matrix(space))
  space <- t(apply(space, space_type, function(x) x/sqrt(sum(x^2)) ))
  pl_fcn(space, col = color, ...)#,pch=16)
  segments(0,0,space[,1],space[,2], col = color, lty = lty)
  if(text_labels)
    text(space,labels = rownames(space), font = 2, pos = 3, cex = 0.5)
}

# draw_circle(T)
# add_cosine_space_points(docspace,2,text_labels = T,pl_fcn = points)
# plot_euclidean_lsa_space(word_space = NA,query_coords = NA)

# visual using ggplot
cosine_ggplot  <- function(docspace, cl_assign) {
    library(ggplot2)
    library(ggrepel)
    ggspace <- as.data.frame(t(apply(docspace, 2, function(x) x/sqrt(sum(x^2)) )))
    set.seed(42)
    colnames(ggspace)<-c("a","b")
    ggplot(ggspace) +
      geom_point(aes(a, b)) +
      geom_label_repel(
        aes(a, b, fill = factor(cl_assign), label = rownames(ggspace)),
        fontface = 'bold', color = 'white', size = 3,
        box.padding = unit(0.25, "lines")
      ) +
      theme_classic(base_size = 16)
}

