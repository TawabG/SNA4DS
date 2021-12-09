#el <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/edgelist.csv", header=T, as.is=T)
#attributes <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/nodelist.csv", header=T, as.is=T)

#el <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/data_2611/edgelist.csv", header=T, as.is=T)
#attributes <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/data_2611/nodelist.csv", header=T, as.is=T)

el <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/SNA4DS_Team14/data_0512/edgelist.csv", header=T, as.is=T)
attributes <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/SNA4DS_Team14/data_0512/nodelist.csv", header=T, as.is=T)


net <- igraph::graph_from_data_frame(d=el, vertices=attributes, directed=FALSE)
class(net)
plot(net)

# Thriller = 1, Horror = 2 en Action = 3
igraph::V(net) # The vertices of the "net" object
igraph::V(net)$genre # Vertex attribute "genre" 
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
kleuren <- c("gray50", "tomato", "gold", "aliceblue", "aquamarine1", "azure4",
             "chocolate4", "cornsilk4", "blue1", "darkgreen")
igraph::V(net)$color <- kleuren[igraph::V(net)$genre]
legend(x=-1.5, y=-1.1, c("Comedy","Drama", "Horror", "Action", "Thriller", "Adventure", "Musical", 
                         "Sci-Fi", "Documentary", "Western"), pch=21,
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

# ERGM Statistical analyses (dyadic independent terms)
model.01 <- ergm::ergm(net2 ~ edges)
summary(model.01)

# model.011 <- ergm::ergm(net2 ~ density)
# summary(model.011)

# model.012 <- ergm::ergm(net2 ~ triangles) # DUURT LANG
# summary(model.012)

# this term adds one network statistic to the model, 
# equal to the number of ties incident on each actor beyond the first.
model.013 <- ergm::ergm(net2 ~ concurrentties("genre")) # Ca. 5-10min
summary(model.013)

model.014 <- ergm::ergm(net2 ~ absdiffcat("decade"))
summary(model.014)

model.015 <- ergm::ergm(net2 ~ edges + absdiffcat("decade"))
summary(model.015)


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

# model.05 <- ergm::ergm(net2 ~ edges + degree(2)) # DUURT LANG
# summary(model.05)


model.016 <- ergm::ergm(net2 ~ edges + absdiffcat("decade") + nodecov("decade") + absdiff("decade") )
summary(model.016)

# Nodefactor definitely used for Genre!
# Nodematch (Homophily) also used for Genre!
# Source: https://snanalyst.github.io/SNA4DS_2021_Slides/Lab_ERGM1.html#24
model.06 <- ergm::ergm(net2 ~ edges + nodefactor("genre"))
summary(model.06)

model.07 <- ergm::ergm(net2 ~ edges + nodematch("genre"))
summary(model.07)

# Explanatory round (parameter 'diff=TRUE' werkt niet!!)
model.08 <- ergm::ergm(net2 ~ edges +
                       nodematch('genre', diff = FALSE) +
                       nodefactor('genre'))
summary(model.08)


model.09 <- ergm::ergm(net2 ~ edges + absdiffcat("decade") + 
                          nodecov("decade") + 
                          absdiff("decade") +
                          nodematch('genre', diff = FALSE) +
                          nodefactor('genre'), 
                       control = ergm::control.ergm(MCMLE.maxit = 1, force.main=TRUE))


summary(model.09)

ergm::mcmc.diagnostics(model.09)


# Testing MCMC
model.xx <- ergm::ergm(net2 ~ edges + nodematch("genre"),
                       control = ergm::control.ergm(MCMLE.maxit = 1, force.main=TRUE))
ergm::mcmc.diagnostics(model.xx)
model.xx.gof <- ergm::gof(model.xx)
plot(model.xx.gof)





# Compare multiple models
texreg::screenreg(list(model.01, model.02, model.03))
# Lower AIC & BIC = better!



# Volgende stap stap = uitzoeken welke ergm terms belangrijk zijn! (iteratief proces)
# Het idee is hoeveel genre uitmaakt op het maken van een edge.

# MCMC (kun je alleen runnen met meerdere ERGM terms)
# MCMC does not run for dyadic independent models
set.seed(1234)
fit <- ergm::ergm(net2 ~ edges + degree(1))
ergm::mcmc.diagnostics(fit)

# Goodness of Fit
# Gof's output is composed of 4 Parts
## Goodness-of-fit for degree
## Goodness-of-fit for edgewise shared partner
## Goodness-of-fit for minimum geodesic distance
## Goodness-of-fit for model statistics
fit.gof <- ergm::gof(fit)
plot(fit.gof)
fit.gof


# An edge is created if an user watched the same movie.
# TODO
# Uitzoeken welke ERGM terms interessant zijn voor ons model: http://statnet.org/nme/d2-ergmterms.html
