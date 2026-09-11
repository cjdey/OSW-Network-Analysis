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


#IF you want to align some terminology you can rename nodes here
# you will also need to update the plot layout for the plot below (by removing any nodes that are no longer included)

# #
# node_corrections <- c(
#   "Temporary Habitat Loss" = "Effective Habitat Loss",
#   "Direct Habitat Loss" = "Effective Habitat Loss",
#   "Habitat Loss_[Effect]" = "Effective Habitat Loss"  
#  )
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
      )  })  })
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

#add columns for sorting
colnames(filtered_paths)[1]<-"Pressure"
colnames(filtered_paths)[2]<-"Individual_Endpoint"
colnames(filtered_paths)[4]<-"Project_Phase"
colnames(filtered_paths)[5]<-"N_Project_Phases"

#new columns for the effects
filtered_paths = filtered_paths %>%
  mutate(Effects = sub("^[^,]*,\\s*", "", path),
         Effects = sub(",[^,]*$", "", Effects))

filtered_paths = filtered_paths %>% select(Pressure, Effects, Individual_Endpoint, N_Project_Phases, Project_Phase)

#write_excel_csv(filtered_paths, "OSW Pathways.csv")


######Overlay plot_all paths##########

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
  c("Artificial Lighting", "Artificial Structures", "Artificial Structures (Turbines and Substations)", "Noise", "Habitat Loss"),
   c("Disorientation", "Heightened Collision Risk", "Roosting/Perching Opportunities", "Change in Movement Paths", "Altered Habitat Quality",  "Direct Habitat Loss", "Temporary Habitat Loss", "Effective Habitat Loss",
     "Habitat Loss_[Effect]",   "Starvation"),
  c("Collision Mortality", "Lower Body Condition", "Heightened Stranding Risk", "Altered Energy Cost", "Altered Foraging Efficiency", "Altered Prey Abundance",  "Masked Communication", "Change in Habitat Connectivity"
     ),
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
  drop_nodes)

node_rows <- node_rows[1:4]
layout <- layout[V(consensus_graph)$name, , drop = FALSE]


#making the plot
#save as svg if want to import to powerpoint (and mural?)
# svg("consensus_network.svg", width = 12, height = 10)

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

# dev.off()

###


#####Subgraphs based on pressure####

# Pressures to make separate plots for
pressures <- c(
  "Artificial Lighting",
  "Artificial Structures",
  "Noise",
  "Habitat Loss",
  "Artificial Structures (Turbines and Substations)"
)


# Create one plot for each pressure
for (pressure in pressures) {
  
  # Find all nodes downstream of the pressure
  downstream <- subcomponent(
    consensus_graph,
    v = which(V(consensus_graph)$name == pressure),
    mode = "out"
  )
  
  # Keep only the downstream nodes
  subgraph <- induced_subgraph(
    consensus_graph,
    vids = downstream
  )
  
  # Get the layout for these nodes
  sub_layout <- layout[V(subgraph)$name, , drop = FALSE]
  
  # Make sure node properties are retained
  V(subgraph)$shape <- V(consensus_graph)$shape[
    match(V(subgraph)$name, V(consensus_graph)$name)
  ]
  
  V(subgraph)$color <- V(consensus_graph)$color[
    match(V(subgraph)$name, V(consensus_graph)$name)
  ]
  
  # Plot filename
  filename <- paste0(
    gsub("[^A-Za-z0-9]+", "_", pressure),
    ".svg"
  )
  
  # Save as SVG
  svg(filename, width = 12, height = 10)
  
  plot(
    subgraph,
    layout = sub_layout,
    
    edge.color = E(subgraph)$color,
    edge.width = 0.7,
    edge.arrow.size = 0.2,
    edge.curved = 0.1,
    
    vertex.shape = V(subgraph)$shape,
    vertex.color = V(subgraph)$color,
    
    vertex.size = 17,
    vertex.size2 = 10,
    
    vertex.label.cex = 0.5,
    vertex.label.color = "black"
  )
  
  dev.off()
}
####



