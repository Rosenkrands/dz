library(dz)

# Setting parameters
inst <- test_instances$p7_chao
L <- 100
k <- 3
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
do.call(sum, sr$s_total)
plot(sr, inst)

ur <- update_routes(sr, L, variances, info)
do.call(sum, ur$s_total)
plot(ur, inst)
