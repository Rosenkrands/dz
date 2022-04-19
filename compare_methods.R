library(dz)

# input parameters
inst <- test_instances$p7_chao
k <- 3
L <- 100

variances <- generate_variances(inst)
info <- generate_information(inst, r = 20)

# clustering
clust_rb <- rb_clustering(
  inst,
  L,
  k,
  num_routes = 100,
  variances,
  info,
  dispute_obj = "most_frequent"
)
plot(clust_rb, inst)

clust_ls <- clustering(
  inst,
  k,
  L,
  eps = 0,
  variances,
  info,
  cluster_method = "local_search",
  alpha = 0
)
plot(clust_ls)

# routing
zone_list <- list("heuristic" = clust_ls$cl$zones, "routing_based" = clust_rb$zones)
starting_route_list <- lapply(zone_list, function(x) starting_routes(inst, x, L))
updated_route_list <- lapply(starting_route_list, function(x) update_routes(x, L, variances, info))

# compare results
lapply(updated_route_list, function(x) sum(do.call(c, x$s_total)))
