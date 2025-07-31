library(networkD3)

# 2. Define your nodes
nodes <- data.frame(
  name = c("Income", "Savings", "Expenses", "Investments")
)

# 3. Define the flows (links)
#    source/target are zero-based indices into the nodes data.frame
links <- data.frame(
  source = c(0,     0,     0,     1),
  target = c(1,     2,     3,     3),
  value  = c(5000,  3000,  2000,  1500)
)

# 4. Create the Sankey diagram
sankeyNetwork(
  Links    = links,
  Nodes    = nodes,
  Source   = "source",
  Target   = "target",
  Value    = "value",
  NodeID   = "name",
  fontSize = 12,
  nodeWidth = 30
)
