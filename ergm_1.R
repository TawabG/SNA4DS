el <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/edgelist.csv", header=T, as.is=T)
attributes <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/nodelist.csv", header=T, as.is=T)

el <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/data_2611/edgelist.csv", header=T, as.is=T)
attributes <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/data_2611/nodelist.csv", header=T, as.is=T)



net <- igraph::graph_from_data_frame(d=el, vertices=attributes, directed=FALSE)
class(net)
plot(net)

# Thriller = 1, Horror = 2 en Action = 3
igraph::V(net) # The vertices of the "net" object
#igraph::V(net)$genre # Vertex attribute "genre" (nog niet in final dataset)
igraph::V(net)$decade # Vertex attribute "decade"

# Edge attribute (pas gebruiken voor RQ2)
# Gebruiken voor amount of users
#igraph::E(net) # The edges of the "net" object
#igraph::E(net)$amount 

# Plot met Movie ID's
plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=igraph::V(net)$id, vertex.label.color="black",
     vertex.label.cex=.7)

# Adhv. genre verschillende node kleuren
# Gray = 1, dus thriller
kleuren <- c("gray50", "tomato", "gold")
igraph::V(net)$color <- kleuren[igraph::V(net)$genre]
legend(x=-1.5, y=-1.1, c("Thriller","Horror", "Action"), pch=21,
       col="#777777", pt.bg=kleuren, pt.cex=2, cex=.8, bty="n", ncol=1)
plot(net)







# Create network
# Create network
# Create network
# Nodig om een ERGM model te runnen
net2 <- network::as.network(el, matrix.type="edgelist", directed=F)


# Add Node attributes
# SELFNOTE: Genre not in data-set yet!!!
net2 <- network::set.vertex.attribute(net2, 'genre', value = attributes$genre)
net2 <- network::set.vertex.attribute(net2, 'decade', value = attributes$decade)

# Add Edge attribute
net2 <- network::set.edge.attribute(net2, 'number_of_links', value = el$number_of_links)

class(net2)
plot(net2)
net2
summary(net2)

# Voorbeelden van ERGM terms
search.ergmTerms('triangle')

# ERGM Statistical analyses
model.01 <- ergm::ergm(net2 ~ edges)
summary(model.01)

# Deploy ERGM models to experiment to which extent 
#   a movie made in a decade increases the probability of a movie being watches

# Nodecov is needed because undirected numeric var
model.02 <- ergm::ergm(net2 ~ edges + nodecov("decade")) 
summary(model.02)

# absdiff used to measure in terms of distance similarity
model.03 <- ergm::ergm(net2 ~ edges + absdiff("decade")) 
summary(model.03)

model.04 <- ergm::ergm(net2 ~ edges + nodecov("decade") + absdiff("decade"))
summary(model.04)

model.05 <- ergm::ergm(net2 ~ edges + degree(2)) 
summary(model.05)

# Nodefactor definitely used for Genre!
# Nodematch (Homophily) also used for Genre!
# Source: https://snanalyst.github.io/SNA4DS_2021_Slides/Lab_ERGM1.html#24



# Compare multiple models
texreg::screenreg(list(model.01, model.02))
# Lower AIC & BIC = better!


# Explanatory round (parameter 'diff=TRUE' werkt niet!!)
model.xx <- ergm::ergm(net2 ~ edges +
                       nodematch('genre', diff = FALSE) +
                       nodefactor('genre'))
summary(model.xx)



# Volgende stap stap = uitzoeken welke ergm terms belangrijk zijn! (iteratief proces)
# Het idee is hoeveel genre uitmaakt op het maken van een edge.

# MCMC (kun je alleen runnen met meerdere ERGM terms)
set.seed(1234)
fit <- ergm::ergm(net2 ~ edges + degree(1))
ergm::mcmc.diagnostics(fit)

# Goodness of Fit
fit.gof <- ergm::gof(fit)
fit.gof

# gof's output is composed of 4 Parts
## Goodness-of-fit for degree
## Goodness-of-fit for edgewise shared partner
## Goodness-of-fit for minimum geodesic distance
## Goodness-of-fit for model statistics

# An edge is created if an user watched the same movie.
# TODO
# Create new attribute on edge_list.
# This attribute contains "amount of users that have rated two movies (an edge)" 
# Weight = onderveeld in 5 klasses. 
# OF weight = letterlijke amount of users, dus een count.
# Uitzoeken welke ERGM terms interessant zijn voor ons model: http://statnet.org/nme/d2-ergmterms.html
# Dylan bericht sturen dataset

