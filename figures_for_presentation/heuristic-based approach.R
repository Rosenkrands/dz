library(tidyverse)

inst <- test_instances$p7_chao
variances <- generate_variances(inst)
info <- generate_information(inst)
p_inst <- prepare_instance(inst, variances, info)

clust <- clustering(p_inst, k = 4, L = 120, eps = 0, variances, info, cluster_method = "local_search", alpha = 0)

plot(clust)

animate_local_search(clust, filename = "HR.gif")
