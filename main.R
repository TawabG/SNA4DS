# Gebruik van test data eerste 10 rows
node_data_2310 <- 'SCHOOL LOCALHOST/Semester 2.1/SNA/SNA4DS_Team14/Data/2310/node_list.csv'
edge_data_2310 <- 'SCHOOL LOCALHOST/Semester 2.1/SNA/SNA4DS_Team14/Data/2310/edge_list.csv'

# node A.K.A vertex
node_data <- 'SCHOOL LOCALHOST/Semester 2.1/SNA/SNA4DS_Team14/Data/node_list_150_movies.csv'
edge_data <- 'SCHOOL LOCALHOST/Semester 2.1/SNA/SNA4DS_Team14/Data/edge_list_150_movies.csv'

node_list <- utils::read.csv(node_data_2310, header = T, as.is = T)
edge_list <- utils::read.csv(edge_data_2310, header = T, as.is = T)

net <- igraph::graph_from_data_frame(d=edge_list, vertices=node_list, directed = T) 

summary(net)

plot(net)


## source: https://github.com/miriamposner/cytoscape_tutorials/blob/master/readme.md
# vertices_data_test <- 'SCHOOL LOCALHOST/Semester 2.1/SNA/SNA4DS_Team14/Data/test_node_list.csv'
# edge_data_test <- 'SCHOOL LOCALHOST/Semester 2.1/SNA/SNA4DS_Team14/Data/test_edge_list.csv'
# nodes_test <- utils::read.csv(vertices_data_test, header = T, as.is = T)
# links_test <- utils::read.csv(edge_data_test, header = T, as.is = T)
# net_test <- igraph::graph_from_data_frame(d=links_test, vertices=nodes_test, directed = T) 
# plot(net_test)



