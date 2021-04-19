setwd("D:/Term-5/MTSMB")
library(tidyverse)
library(igraph)
library(sqldf)

flights1.1 <- read.csv("airOT201001.csv")
flights1.2 <- read.csv("airOT201002.csv")
flights1.3 <- read.csv("airOT201003.csv")
flights1.4 <- read.csv("airOT201004.csv")
flights1.5 <- read.csv("airOT201005.csv")
flights1.6 <- read.csv("airOT201006.csv")
flights1.7 <- read.csv("airOT201007.csv")
flights1.8 <- read.csv("airOT201008.csv")
flights1.9 <- read.csv("airOT201009.csv")
flights1.10 <- read.csv("airOT201010.csv")
flights1.11 <- read.csv("airOT201011.csv")
flights1.12 <- read.csv("airOT201012.csv")
flights1.13 <- read.csv("airOT201101.csv")
flights1.14 <- read.csv("airOT201102.csv")
flights1.15 <- read.csv("airOT201103.csv")
flights1.16 <- read.csv("airOT201104.csv")
flights1.17 <- read.csv("airOT201105.csv")
flights1.18 <- read.csv("airOT201106.csv")
flights1.19 <- read.csv("airOT201107.csv")
flights1.20 <- read.csv("airOT201108.csv")
flights1.21 <- read.csv("airOT201109.csv")
flights1.22 <- read.csv("airOT201110.csv")
flights1.23 <- read.csv("airOT201111.csv")
flights1.24 <- read.csv("airOT201112.csv")
flights1.25 <- read.csv("airOT201201.csv")
flights1.26 <- read.csv("airOT201202.csv")
flights1.27 <- read.csv("airOT201203.csv")
flights1.28 <- read.csv("airOT201204.csv")
flights1.29 <- read.csv("airOT201205.csv")
flights1.30 <- read.csv("airOT201206.csv")
flights1.31 <- read.csv("airOT201207.csv")
flights1.32 <- read.csv("airOT201208.csv")
flights1.33 <- read.csv("airOT201209.csv")
flights1.34 <- read.csv("airOT201210.csv")
flights1.35 <- read.csv("airOT201211.csv")
flights1.36 <- read.csv("airOT201212.csv")


flights <- rbind(flights1.1, flights1.2, flights1.3,flights1.4,flights1.5,flights1.6,flights1.7,flights1.8,flights1.9,flights1.10,flights1.11,flights1.12,flights1.13,flights1.14,flights1.15,flights1.16,flights1.17,flights1.18,flights1.19,flights1.20,flights1.21,flights1.22,flights1.23,flights1.24,flights1.25,flights1.26,flights1.27,flights1.28,flights1.29,flights1.30,flights1.31,flights1.32,flights1.33,flights1.34,flights1.35,flights1.36)

flights2 <- flights

flights2 <- flights2[c("YEAR", "MONTH", "DAY_OF_MONTH", "DAY_OF_WEEK", "FL_DATE", "ORIGIN_AIRPORT_ID", "ORIGIN", "ORIGIN_STATE_ABR", "DEST_AIRPORT_ID", "DEST", "DEST_STATE_ABR", "ARR_DELAY_NEW", "CRS_ELAPSED_TIME", "DISTANCE")]

flights2 <- flights2 %>% filter((ARR_DELAY_NEW/CRS_ELAPSED_TIME) >= 0.1)


edges.df.igraph <- flights2 %>%
  select(ORIGIN, DEST) %>%
  distinct() 

ig = graph.data.frame(edges.df.igraph)

ig_degree_in = sort(degree(ig, mode = "in"),decreasing = TRUE)
ig_degree_in 

ig_degree_out = sort(degree(ig, mode = "out"),decreasing = TRUE)
ig_degree_out

V(ig)[neighbors(ig, v=V(ig)['ATL'])]

E(ig)[incident(ig, v=V(ig)['ATL'])]

ig_betweeness = sort(betweenness(ig,directed = TRUE), decreasing = TRUE)
ig_betweeness[1:10]

ig_betweeness_0 <- ig_betweeness[which(ig_betweeness == 0.00)]
length(ig_betweeness_0)

ig_shortest_path = sort (shortest.paths(graph = ig)['COS','GUC'],decreasing = TRUE)
ig_shortest_path

ig_closeness = sort(closeness(ig),decreasing = TRUE)
head(ig_closeness,5)

tail(ig_closeness,5)

ig_transitivity = transitivity(ig)
ig_transitivity

ig_coreness = sort(coreness(ig),decreasing = TRUE)
ig_coreness

flights2 %>%
  select(ORIGIN,DEST,ARR_DELAY_NEW,CRS_ELAPSED_TIME) %>%
  na.omit()%>%
  group_by(ORIGIN,DEST) %>%
  summarize(nflights = n(),
            Arr_delay_Perc =  mean(ARR_DELAY_NEW/CRS_ELAPSED_TIME, na.rm=TRUE))  %>%
  group_by() %>% 
  {.} -> edges.df.new

edges.df.new <- graph.data.frame(edges.df.new)

plot(edges.df.new,
     vertex.color="blue", vertex.size=1,
     vertex.frame.color="gray", vertex.label.color="black",
     vertex.label.cex=0.5, vertex.label.dist=0.1,
     edge.curved=0.1, edge.arrow.size=0.03,
     edge.color="light blue")

