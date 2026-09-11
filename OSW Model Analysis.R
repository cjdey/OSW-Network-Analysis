require(igraph)
require(readxl)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(ggraph)


#read in edgelists
file <- "OSW Model Edgelists_v2.xlsx"
sheets <- excel_sheets(file)

data <- map(
  sheets,
  ~ read_excel(file, sheet = .x)
)

names(data) <- sheets

#make graphs
graphs <- data %>%
  map(~ graph_from_data_frame(.x, directed = TRUE))




#IF you want to align some terminolgogy you can rename nodes here
# #
# node_corrections <- c(
#   
#  #  "Reduced Survival" = "Endpoints",
#   
#   "Temporary Habitat Loss" = "Effective Habitat Loss",
#   "Direct Habitat Loss" = "Effective Habitat Loss",
#   "Habitat Loss_[Effect]" = "Effective Habitat Loss",
#   "Altered Energetic Cost" = "Altered Energy Cost"
# 
# )
# 
# graphs <- graphs %>%
#   map(function(g) {
#     V(g)$name <- recode(V(g)$name, !!!node_corrections)
#     g
#   })




###Get all paths across all models

get_paths <- function(g, graph_name) {
  
  nodes <- V(g)$name
  
  map_dfr(nodes, function(from) {
    
    map_dfr(nodes[nodes != from], function(to) {
      
      paths <- all_simple_paths(
        g,
        from = from,
        to = to,
        mode = "out"
      )
      
      if (length(paths) == 0) {
        return(tibble())
      }
      
      tibble(
        from = from,
        to = to,
        path = map_chr(paths, ~ paste(V(g)$name[.x], collapse = ", ")),
        graph = graph_name
      )
    })
  })
}


#Get paths from the first 7 models (excluding accidents)
all_paths <- imap_dfr(graphs[1:7], get_paths)


#collapse to show similar paths across different models
path_table <- all_paths %>%
  group_by(from, to, path) %>%
  summarise(
    graphs = paste(graph, collapse = ", "),
    n_graphs = n_distinct(graph),
    .groups = "drop"
  )

##filter the path table to just go to from the pressures to the individual endpoints
filtered_paths = path_table %>% filter(
  from %in% c("Artificial Lighting", "Habitat Loss", "Artificial Structures", "Noise", "Artificial Structures (Turbines and Substations)"),
  to %in% c("Reduced Survival", "Reduced Reproductive Success"))%>%
  arrange(-n_graphs)

# write_excel_csv(filtered_paths, "OSW Pathways.csv")




#######Overlay plot##########

#calculating how many models each edge is in
edge_counts <- purrr::imap_dfr(
  graphs[1:7],
  ~ tibble(
    from = igraph::ends(.x, E(.x))[, 1],
    to   = igraph::ends(.x, E(.x))[, 2],
    network = .y
  )
) %>%
  distinct(network, from, to) %>%
  count(from, to, name = "n_networks")

#create the consensus network
consensus_graph <- graph_from_data_frame(
  edge_counts,
  directed = TRUE
)

#shorten the labels with text wrapping
V(consensus_graph)$label <- str_wrap(
  V(consensus_graph)$name,
  width = 15
)



#Set the layout for the plot
#Note that if you change any of the node labels (eg to align between similar nodes) you will need to update this structure
node_rows <- list(
  c("Artificial Lighting", "Artificial Structures", "Noise", "Habitat Loss", "Artificial Structures (Turbines and Substations)" ),
   c("Collision Mortality", "Disorientation", "Altered Habitat Quality", "Change in Movement Paths", "Direct Habitat Loss", "Effective Habitat Loss",
     "Habitat Loss_[Effect]", "Heightened Collision Risk", "Roosting/Perching Opportunities", "Starvation", "Temporary Habitat Loss"),
  c("Heightened Stranding Risk", "Altered Energy Cost", "Altered Foraging Efficiency", "Altered Prey Abundance","Change in Habitat Connectivity",
    "Lower Body Condition", "Masked Communication" ),
  c("Reduced Survival", "Reduced Reproductive Success"),
  c("Reduced Population Abundance", "Changed Population Distribution")
)


#create the x and y layout
layout_df <- purrr::imap_dfr(
  node_rows,
  function(nodes, row) {
    
    n <- length(nodes)
    
    tibble(
      name = nodes,
      x = (seq_len(n) - mean(seq_len(n))) * ifelse(row %in% c(1, 4, 5), 0.8, 0.7),
      y = (6 - row) +
        if (row == 2) {
          rep(c(-0.2, 0.2), length.out = n)
        } else if (row == 3) {
          rep(c(0.2, -0.2), length.out = n)
        } else {
          0
        }
    )
  }
)

layout <- as.matrix(layout_df[, c("x", "y")])
rownames(layout) <- layout_df$name
layout <- layout[V(consensus_graph)$name, , drop = FALSE]

#set custom shapes and colors
#unfortunately there is no diamond shape available
node_row <- sapply(
  V(consensus_graph)$name,
  function(node) which(sapply(node_rows, function(x) node %in% x))
)

#node shape
V(consensus_graph)$shape <- ifelse(
  node_row %in% c(1, 2, 3),
  "rectangle",
  "circle"
)

#node color
V(consensus_graph)$color <- ifelse(
  node_row == 1,
  "white",      # row 1
  ifelse(
    node_row %in% c(2, 3),
    "grey70",   # rows 2 and 3
    "white"     # rows 4 and 5
  )
)

#colors for the edges
E(consensus_graph)$color <- gray(
  0.75 * (1 - (E(consensus_graph)$n_networks - 1) / 6)
)


#Recommended
#If you want to plot without the pop level endpoints
drop_nodes <- node_rows[[5]]

consensus_graph <- delete_vertices(
  consensus_graph,
  drop_nodes
)

node_rows <- node_rows[1:4]
layout <- layout[V(consensus_graph)$name, , drop = FALSE]



#making the plot
#save as svg if want to import to powerpoint (and mural?)
svg("consensus_network.svg", width = 12, height = 10)

plot(
  consensus_graph,
  layout = layout,
  edge.color = E(consensus_graph)$color,
  edge.width = 0.7,
  edge.arrow.size = 0.2,
  edge.curved = 0.1,
 
  vertex.shape = V(consensus_graph)$shape,
  vertex.color = V(consensus_graph)$color,
  
  vertex.size = 17,
  vertex.size2 = 10,
  
  vertex.label.cex = 0.5,
  vertex.label.color = "black"
)



 dev.off()


####



