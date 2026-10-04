\name{inc.contacts}

\alias{inc.contacts}

\title{
Subset edgelist network data to identify contacts
}

\description{
Subset network data in edgelist format to include a specified vertex of interest and the vertices connected to it. 
}

\usage{
inc.contacts(graph, nodes, order = 2, mode = "in")
}

\arguments{
  \item{graph}{an object of class \code{igraph} defining the vertex connections.}
  \item{nodes}{character scalar or character vector, specifying the individual vertex (or vertices) of interest.}
  \item{order}{numeric scalar defining the search distance. See details.}
  \item{mode}{character scalar. Options are \code{in}, \code{out} or \code{all}. Use \code{out} to return only the outgoing edges from the vertex (or vertices) of interest. Use \code{in} to return the edges incoming to the vertex (or vertices) of interest. Use \code{all} to return outgoing and incoming edges to the vertex (or vertices) of interest. This argument is ignored for undirected graphs.}
}

\details{
This function would typically be used for tracing movement events onto and off livestock premises in the event of an infectious disease outbreak. The user would load movement event data into R and create an \code{igraph} object from that data. They would then specify a location of interest and use the \code{inc.trace} function to take a subset of the entire network that includes the location of interest and all locations connected to it. 

The \code{order} argument controls the number of connections to the location of interest. Setting \code{order = 2} includes the first and second order connections, that is, the location directly connected to the location of interest as well as locations connected to those locations. Care is advised when setting the \code{order} argument. Setting it too high (for example, greater than 4) can result in a very large subset graph. 

The \code{tkplot} option, as shown in the example below, provides a convenient way to re-arrange the position of vertices on a network plot for clear display.}

\value{
Returns an \code{igraph} object which is a subset of the object \code{graph}.
}

\examples{
## EXAMPLE 1:
\dontrun{
library(igraph)

data(inc.network)
index.herd <- "N094650321-1"
inc.network$vert.dat$col <- "light blue"
inc.network$vert.dat$col[inc.network$vert.dat$herd == index.herd] <- "red"

## Create a graph object from using the edge and vertex data:
tmp.edg <- graph.data.frame(d = inc.network$edg.dat, directed = TRUE, 
   vertices = inc.network$vert.dat)
trace.edg <- inc.contacts(graph = tmp.edg, nodes = index.herd, order = 2, 
   mode = "in")

## Plot the subsetted network. The vertex of interest is shown in red. Date
## and direction of transactions between vertices are labelled:
plot(trace.edg, vertex.size = 10, vertex.label.cex = 0.75, 
   vertex.color = get.vertex.attribute(graph = trace.edg, name = "col"), 
   vertex.frame.color = "transparent", 
   edge.arrow.size = 0.50, 
   edge.color = "black", 
   edge.label = get.edge.attribute(graph = trace.edg, name = "mdate"), 
   edge.label.cex = 0.60)

## The same, using using the tkplot function in package igraph:
tkplot(trace.edg, 
   edge.label = get.edge.attribute(graph = trace.edg, name = "mdate"), 
   vertex.color = get.vertex.attribute(graph = trace.edg, name = "col"))
 }
}
