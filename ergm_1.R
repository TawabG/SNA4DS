#el <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/edgelist.csv", header=T, as.is=T)
#attributes <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/nodelist.csv", header=T, as.is=T)

#el <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/data_2611/edgelist.csv", header=T, as.is=T)
#attributes <- read.csv("D:/Documents/SCHOOL LOCALHOST/Semester 2.1/SNA/Debugging/ERGM_debug/data_2611/nodelist.csv", header=T, as.is=T)

# An edge is created if an user watched the same movie.
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
?ergm

# ERGM Statistical analyses (dyadic independent terms)
model.01 <- ergm::ergm(net2 ~ edges)
summary(model.01)

model.01.1 <- ergm::ergm(net2 ~ density)
summary(model.01.1)

model.01.2 <- ergm::ergm(net2 ~ triangles) # DUURT LANG
summary(model.01.2)


# this term adds one network statistic to the model, 
# equal to the number of ties incident on each actor beyond the first.
model.02 <- ergm::ergm(net2 ~ concurrentties("genre")) # Ca. 5-10min
summary(model.02)

model.03 <- ergm::ergm(net2 ~ absdiffcat("decade"))
summary(model.03)

model.04 <- ergm::ergm(net2 ~ edges + absdiffcat("decade"))
summary(model.04)

# Deploy ERGM models to experiment to which extent 
#   a movie made in a decade increases the probability of a movie being watches

# Nodecov is needed because undirected numeric var
# This means that an interaction between two dyad-independent terms can be 
# interpreted the same way as it would be in the corresponding logistic regression for each potential edge
model.05 <- ergm::ergm(net2 ~ edges + nodecov("decade")) 
summary(model.05)
SNA4DS::Ef_int(model.05, type = "odds")

# absdiff used to measure in terms of distance similarity
model.06 <- ergm::ergm(net2 ~ edges + absdiff("decade")) 
summary(model.06)

model.06.1 <- ergm::ergm(net2 ~ edges + absdiffcat("decade")) 
summary(model.06.1)

model.07 <- ergm::ergm(net2 ~ edges + nodecov("decade") + absdiff("decade"))
summary(model.07)

model.08 <- ergm::ergm(net2 ~ edges + nodecov("decade") + absdiff("decade") + absdiffcat("decade"))
summary(model.08)
SNA4DS::Ef_int(model.08, type = "odds")

# Nodefactor definitely used for Genre!
# Nodematch (Homophily) also used for Genre!
# Source: https://snanalyst.github.io/SNA4DS_2021_Slides/Lab_ERGM1.html#24
model.09 <- ergm::ergm(net2 ~ edges + nodefactor("genre"))
summary(model.09)

model.10 <- ergm::ergm(net2 ~ edges + nodematch("genre"))
summary(model.10)

model.11 <- ergm::ergm(net2 ~ edges + nodemix("genre", base = c(1)))
summary(model.11)
                      
model.12 <- ergm::ergm(net2 ~ edges + nodefactor("genre") + nodematch("genre"))
summary(model.12)

model.13 <- ergm::ergm(net2 ~ edges + nodefactor("genre") + nodematch("genre") + nodemix("genre", base = c(1)))
summary(model.13)
SNA4DS::Ef_int(model.13, type = "odds")


# Explanatory round (parameter 'diff=TRUE' werkt niet!!)
#model.12  <- ergm::ergm(net2 ~ edges + nodematch('genre', diff = FALSE) + nodefactor('genre'))
#summary(model.12)

# Continuous variables With nodefactor added
model.14 <- ergm::ergm(net2 ~ edges + nodecov("decade") + absdiff("decade") 
                       + absdiffcat("decade")+ nodefactor("genre"))
summary(model.14)

# With nodematch added
model.15 <- ergm::ergm(net2 ~ edges + nodecov("decade") + absdiff("decade") 
                       + absdiffcat("decade") + nodematch("genre"))
summary(model.15)

# With nodefactor AND nodematch added
model.16 <- ergm::ergm(net2 ~ edges + nodecov("decade") + absdiff("decade") 
                       + absdiffcat("decade")+ nodefactor("genre") + nodematch("genre"))
summary(model.16)
SNA4DS::Ef_int(model.16, type = "odds")

# With nodefactor, nodematch and nodemix added
model.17 <- ergm::ergm(net2 ~ edges 
                       + nodecov("decade") 
                       + absdiff("decade") 
                       + absdiffcat("decade")
                       + nodefactor("genre") 
                       + nodematch("genre")
                       + nodemix("genre", base = c(1)))
summary(model.17)




# Compare multiple models
# Lower AIC & BIC = better!
comparing_models <- texreg::screenreg(list(model.01, model.02, model.03,
                       model.04, model.05, model.06,
                       model.07, model.08, model.09,
                       model.10, model.11, model.12,
                       model.13, model.14, model.15,
                       model.16, model.17, model.10.1))


# export to html
texreg::htmlreg(list(model.01, model.02, model.03,
                     model.04, model.05, model.06,
                     model.07, model.08, model.09,
                     model.10, model.11, model.12,
                     model.13, model.14, model.15,
                     model.16, model.17, model.10.1),file='models.html')



# >>>>>>BEST MODEL>>>>>
# Run Monte Carlo maximum likelihood estimation (MCMLE) on best model
# MCMC (kun je alleen runnen met meerdere ERGM terms)
# MCMC does not run for dyadic independent models
model.04.best <- ergm::ergm(net2 ~ edges + absdiffcat("decade"),
                       control = ergm::control.ergm(
                         MCMC.samplesize = 500,
                         MCMC.burnin = 500, 
                         MCMLE.maxit = 2,
                         force.main=TRUE))

model.04.best.mcmc <- ergm::mcmc.diagnostics(model.04.best)
sink(file = 'best_model_mcmc_output.txt')
model.04.best.mcmc
sink()


# Goodness of Fit
# Gof's output is composed of 4 Parts
## Goodness-of-fit for degree
## Goodness-of-fit for edgewise shared partner
## Goodness-of-fit for minimum geodesic distance
## Goodness-of-fit for model statistics
model.04.best.gof <- ergm::gof(model.04.best)

sink(file = 'best_model_gof.txt')
model.04.best.gof
sink()

# ERROR: Error in eval(predvars, data, env) : object 'degree' not found
model.04.best.gof.test <- ergm::ergm(net2 ~ edges + degree(1))
model.04.best.gof.test.2 <- ergm::gof(model.04.best.gof.test)
plot(model.04.best.gof.test.2)




# Odds Ratio
# OR > 1 means greater odds of association with the exposure and outcome.
# OR = 1 means there is no association between exposure and outcome.
# OR < 1 means there is a lower odds of association between the exposure and outcome
SNA4DS::Ef_int(model.04.best, type = "odds")


# Probabilities
SNA4DS::Ef_int(model.04.best, type = "prob")
