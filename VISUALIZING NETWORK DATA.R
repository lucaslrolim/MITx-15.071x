edges = read.csv("./data/edges.csv")
users = read.csv("./data/users.csv")
(nrow(edges)*2)/nrow(users)
table(users$locale, users$school)
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
sum(degree(g) >= 10)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
degree(g)[(which.max(degree(g)))]
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
V(g)$color[V(g)$school == "A"] = "blue"
V(g)$color[V(g)$school == "AB"] = "green"
V(g)$color[V(g)$locale == "A"] = "orange"
V(g)$color[V(g)$locale == "B"] = "yellow"