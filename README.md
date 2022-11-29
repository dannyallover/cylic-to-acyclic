# Cylic to Acyclic

This api takes a directed graph with cycles and implements an algorithm to turn it into a directed acylic graph (DAG). It does so by building the graph, edge by edge, and once a connected component is formed (implies cycle), a set of heuristics are enforced to determine the problem edge and removes it, repeating this process until all edges have been processed.
