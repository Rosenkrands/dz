library(dz)

# Setting parameters
inst <- test_instances$p7_chao
L <- 200
k <- 2
variances <- generate_variances(inst)
info <- generate_information(inst)
num_routes <- 1000
plot(inst)

# Prepare instance
p_inst <- prepare_instance(inst, variances, info)
plot(p_inst)

# Clustering
rb_clust <- rb_clustering(p_inst, L, k, num_routes, info, top_percentile = .15)
plot(rb_clust)

zones <- rb_clust$zones
system.time(sr <- starting_routes(inst, zones, L))
do.call(sum, sr$total_score)
plot(sr, inst)

ur <- update_routes2(p_inst, zones, L, k, sr, info)
do.call(sum, ur$total_score)
plot(ur, inst)
