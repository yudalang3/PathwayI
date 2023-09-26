root <- NULL;
system.time({
  root <<- process_createNodes(15)
})
lobstr::obj_size(root)

# 32768 个节点

mat <- matrix(data = rnorm(32768 * 4), ncol = 4)
dist <- dist(mat)

hc <- NULL
system.time({
  hc <- hclust(dist, "ave")
})

lobstr::obj_size(hc)

system.time({
dendrogram <<- as.dendrogram(hc)
})

plot(dendrogram)

lobstr::obj_size(dendrogram)


attributes(dendrogram)

nodP

local({
  attr(dendrogram,'height') <-  10
})

attr(dendrogram,'height') <-  10

system.time({
tree <<- process_dendrogram2intuitiveTree(dendrogram = dendrogram)
})

lobstr::obj_size(dendrogram)
lobstr::obj_size(tree)
