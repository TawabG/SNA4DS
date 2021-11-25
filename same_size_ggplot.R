

same.size.ggplot <- function(vector.string.graph, # a vector of strings which correspond to Robject ggplot graphs
                             reference.string.graph, # a string of a  Robject ggplot graphs where height and/or height will be taken for reference
                             width = T, # if you wanna adapat only the width
                             height = F # if you wanna adapat only the height
) {
  
  # example: same.size.ggplot(p0rep(c("a", "b"), thre), "a30") 
  
  
  which(vector.string.graph %in% reference.string.graph)
  
  newref <- ggplotGrob(get(reference.string.graph))
  ref.width <- newref$widths
  ref.height <- newref$heights
  
  assign(reference.string.graph, newref, env = parent.frame(1))
  
  for(i in seq_along(vector.string.graph)) {
    if(vector.string.graph[i] != reference.string.graph) {
      new <- ggplotGrob(get(vector.string.graph[i]))
      if( width ) {
        new$widths <- ref.width
      }
      if( height ) {
        new$heights <- ref.height
      }
      assign(vector.string.graph[i], new, env = parent.frame(1))
    }
  }
}
