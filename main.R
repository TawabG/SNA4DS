# Mijn path naar het bestand
path_tawab <- 'SCHOOL LOCALHOST/Semester 2.1/SNA/SNA4DS_Team14/first_100_movie_ratings.csv'

netflix_data <- read.csv(file = path_tawab)
head(netflix_data)
summary(netflix_data)

# Gebruik van testdata
# source: https://github.com/miriamposner/cytoscape_tutorials/blob/master/readme.md
vertices_data <- 'SCHOOL LOCALHOST/Semester 2.1/SNA/SNA4DS_Team14/Data/test_node_list.csv'
edge_data <- 'SCHOOL LOCALHOST/Semester 2.1/SNA/SNA4DS_Team14/Data/test_edge_list.csv'

  
nodes <- utils::read.csv(vertices_data, header = T, as.is = T)
links <- utils::read.csv(edge_data, header = T, as.is = T)

net <- igraph::graph_from_data_frame(d=links, vertices=nodes, directed = T) 

plot(net)
