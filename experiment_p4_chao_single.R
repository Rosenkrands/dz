library(dz)
# set.seed(1)
pbapply::pboptions(use_lb = T)

# Variables
top_percentile = .5

repetitions <- 1:1
inst <- test_instances$p2_tsiligirides
L <- 20
k <- 3

variances <- generate_variances(inst)
info <- generate_information(inst)
p_inst <- prepare_instance(inst, variances, info)

num_routes <- 100

rb_clust <- rb_clustering(p_inst, L, k, num_routes, info, top_percentile = top_percentile, weigthed = F)
plot(rb_clust)

clust_ls <- clustering(p_inst, k=3, L = 50, eps = 0, variances, info, cluster_method = "local_search", alpha = 1)
plot(clust_ls) + coord_fixed()
ggsave("./figures_for_report/p2_example.png", width = 5, height = 5)
plot(test_instances$p2_tsiligirides) + coord_fixed()
ggsave("./figures_for_report/p2.png", width = 5, height = 5)
