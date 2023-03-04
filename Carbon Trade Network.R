setwd("")

library(tidyverse)
library(visNetwork)


# reading data
nodes <- read.csv("nodelist2.csv", header=T, as.is=T)

project <- read.csv("project2.csv") %>%
  # set column names as expected in edges data set
  rename(from=Project, to=Label, 
         title=Exchange) %>%
  # set edge colors by creating "color" column in data
  # using "title" because changed the name of "Exchange" above
  mutate(color = case_when(
    title == "Financial Support" ~ "green",
    title == "Leadership" ~ "red",
    title == "Carbon credit" ~ "brown",
    title == "Partnership" ~ "black", 
    TRUE ~ "gray"),
    arrows.from.type=case_when(
      title == "Financial Support"~"arrow",
      title == "Partnership"~"arrow"),
    arrows.to.type=case_when(
      title == "Carbon credit"~"arrow"))



# create nodes data frame from project data
project_nodes <- data.frame(id=c(unique(project$from), unique(project$to))) %>% 
  left_join(nodes, by=c("id"="Label"))%>%
  mutate(id.y = as.character(id.y),
         label = coalesce(id.y, id),
         group = ifelse(startsWith(id, "5"), "project", Category),
         title = id)  %>% #ini tambahan variablenya
  select(-id.y, -Category)


#plotting network
visNetwork( nodes=project_nodes,edges=project) %>% 
  visNodes(size=30) %>% 
  visEdges(width = 3.5) %>% 
  visGroups(groupname = "project", color = "pink", shape = 'star') %>%
  visGroups(groupname = "group", color = rainbow(10), shape = 'circle') %>% 
  visIgraphLayout(type = "full",layout = "layout_nicely", physics = F, smooth = F, randomSeed = 123) %>% 
  visOptions(selectedBy = "group", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visPhysics(stabilization = FALSE) %>% 
  visLegend()
  
  #visLegend(addEdges = ledges) #ini bisa diilangin klo ga perlu legend
  
  
  #ledges <- data.frame(color = c("green", "black", "red", "purple"),
                       #label = c("Money", "Partnership", "Carbon credit", "Leadership"))
  
  # counting edges
  money <- sum(project$title == "Financial Support")
  carboncredit <- sum(project$title == "Carbon credit")
  partnership <- sum(project$title == "Partnership")
  leadership <- sum(project$title == "Leadership")
  
  # counting vertices
  bank <- sum(nodes$Category == "Bank")
  corporation <- sum(nodes$Category == "Corporation")
  bingo <- sum(nodes$Category == "BINGO")
  donor <- sum(nodes$Category == "Donor Agency")
  foundation <- sum(nodes$Category == "Foundation")
  govt <- sum(nodes$Category == "Government")
  io <- sum(nodes$Category == "Intergovernmental Organization")
  ri <- sum(nodes$Category == "Research Institute")
  uni <- sum(nodes$Category == "University")
  na <- sum(nodes$Category == "N/A")
  ngo <- sum(nodes$Category == "NGO")

