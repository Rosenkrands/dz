library(dz)
set.seed(10)

# Setting parameters
inst <- test_instances$p7_chao
L <- 80
k <- 4
variances <- generate_variances(inst)
info <- generate_information(inst)
num_routes <- 100
plot(inst)

# Prepare instance
p_inst <- prepare_instance(inst, variances, info)
plot(p_inst)

# Clustering
rb_clust <- rb_clustering(p_inst, L, k, num_routes, info)
plot(rb_clust)

zones <- rb_clust$zones
system.time(sr <- starting_routes(inst, zones, L))
plot(sr, inst)

ur <- update_routes(sr, L, variances, info)
plot(ur, inst)
