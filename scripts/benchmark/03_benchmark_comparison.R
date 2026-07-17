################################################################################
################################################################################
#################### Bechnmark COmparison of Algorithms ########################
################################################################################
################################################################################

library(tidyverse)
library(xtable)

result_data <- readRDS(
  file.path("results", "benchmark", "full_result.rds")
)

tasks_meta_features <- readRDS(
  file.path("results", "benchmark", "tasks_meta_features.rds")
)

#' Create Table number of wins
resultData %>% 
  group_by(Algorithm, task_id) %>%
  summarise(mcr = mean(mcr))%>%
  ungroup() %>% 
  group_by(task_id) %>%
  mutate(r = rank(mcr))%>%
  ungroup() %>%
  group_by(Algorithm) %>%
  summarize(`No. Wins` = sum(r==1),
            `Mean Rank` = mean(r)) %>%
  ungroup()%>%
  arrange(Algorithm) %>%
  select(Algorithm, `No. Wins`, `Mean Rank`) %>%
  xtable(caption = 'Performance of the parallel tree ensembles on the benchmark data sets.',
         label = "tbl:benchMarkSum") %>%
  print( include.rownames=FALSE, caption.placement = "top")

#' Create complete table of missclassifications
resultData %>%
  group_by(Algorithm, task_id) %>%
  summarise(mcr = mean(mcr))%>%
  ungroup() %>%
  group_by(task_id) %>%
  mutate(r = rank(mcr)) %>%
  ungroup() %>%
  mutate(mcr = round(mcr, 3),
         mcr = as.character(mcr), 
         mcr = if_else(r==1, paste0('\\textbf{', mcr, '}'), mcr), 
         task_id = as.character(task_id)) %>%
  select(Algorithm, mcr, 'Tasks ID' = task_id) %>%
  arrange(Algorithm) %>%
  pivot_wider(names_from = Algorithm, values_from = mcr) %>%
  xtable(caption = 'Average miss classification rate for each algorithm over all folds of each task.',
         label = "tbl:BenchMarkFullRes") %>%
  print(sanitize.text.function = identity, include.rownames=FALSE, 
        tabular.environment = "longtable",
        floating = FALSE)


#' Create table of pairwise comparison
tmp_data = resultData %>%
  group_by(Algorithm, task_id) %>%
  summarise(mcr = mean(mcr))%>%
  ungroup() %>%
  select(task_id, Algorithm, mcr) %>%
  arrange(Algorithm) %>%
  pivot_wider(names_from = Algorithm, values_from = mcr)

pairwiseComp = sapply(2:ncol(tmp_data), \(i){
  sapply(2:ncol(tmp_data), \(j){
    mean(tmp_data[,i] < tmp_data[j])
  })
})

colnames(pairwiseComp) = colnames(tmp_data)[-1]
rownames(pairwiseComp) = colnames(tmp_data)[-1]

pairwiseComp %>%
  xtable(caption = 'Matrix of pairwise comparisons: Each cell gives the fraction of tasks in which the algorithm on the top of the column performs better than the algorithm on the left.',
         label = "tbl:pairWiseComp") %>%
  print(caption.placement = "top")

################################################################################

df = resultData %>%
  group_by(task_id, Algorithm) %>%
  summarize(mcr = mean(mcr)) %>%
  ungroup() %>%
  group_by(task_id) %>%
  mutate(r = rank(mcr)) %>%
  ungroup() %>%
  select(-mcr)

library(dplyr)
library(tidyr)
library(proxy)

# df = deine Tabelle mit task_id, Algorithm, r

# 1. Long -> Wide: eine Zeile pro task_id, Spalten = Algorithmen
rank_wide <- df %>%
  select(task_id, Algorithm, r) %>%
  pivot_wider(
    names_from = Algorithm,
    values_from = r
  ) %>%
  arrange(task_id)

# 2. task_id als rownames speichern
rank_mat <- rank_wide %>%
  tibble::column_to_rownames("task_id") %>%
  as.matrix()

# 3. Kendall-Tau-Distanz definieren
kendall_dist <- function(x, y) {
  tau <- cor(x, y, method = "kendall", use = "pairwise.complete.obs")
  return(1 - tau)
}

# 4. Distanzmatrix berechnen
dist_mat <- proxy::dist(rank_mat, method = kendall_dist)

# 5. Hierarchisches Clustering
hc <- hclust(dist_mat, method = "average")

# 6. Dendrogramm plotten
plot(hc, main = "Hierarchisches Clustering mit Kendall-Tau-Distanz",
     xlab = "task_id", sub = "")

# 7. Cluster schneiden, z.B. in 3 Cluster
clusters <- cutree(hc, k = 6)

# 8. Cluster den task_ids zuordnen
cluster_result <- data.frame(
  task_id = as.numeric(names(clusters)),
  cluster = clusters
)

cluster_result %>%
  group_by(cluster) %>%
  summarize(n())



resultData %>%
  inner_join(cluster_result) %>%
  select(-task_id, -alg_id, -Algorithm, -mcr, -fold)%>%
  group_by(cluster) %>%
  summarise_all(mean) %>%
  t()


df %>%
  inner_join(cluster_result) %>% 
  select(-task_id) %>%
  group_by(cluster, Algorithm) %>%
  summarise(r = mean(r)) %>%
  ungroup() %>%
  pivot_wider(names_from = Algorithm, values_from = r)


tasksMetaFeats <- readRDS(paste0(workingDir, 'Benchmark/tasksMetaFeats.rds'))

tasksMetaFeats %>%
  inner_join(cluster_result, join_by(id == task_id)) %>% 
  select(-id) %>%
  group_by(cluster) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  pivot_longer(names_to = 'feat', values_to = 'val', -cluster) %>%
  mutate(val = round(val, 2)) %>%
  pivot_wider(names_from = cluster, values_from = val) %>%
  print(n=22)


################################################################################
df = resultData %>%
  group_by(task_id, Algorithm) %>%
  summarize(mcr = mean(mcr)) %>%
  ungroup() %>%
  group_by(task_id) %>%
  mutate(r = rank(mcr, ties.method = 'min')) %>%
  ungroup() %>%
  select(-mcr)


df %>%
  ggplot(aes(x = r))+
  geom_bar()+
  facet_grid(.~Algorithm)+
  xlab('Rank')+
  ylab('Count')+
  theme_minimal()
ggsave(
  file.path("figures", "Ranks.eps"),
  width = 10,
  height = 8,
  units = "cm"
)


resultData %>%
  group_by(task_id, fold) %>%
  mutate(r = rank(mcr)) %>%
  ungroup() %>%
  ggplot(aes(x = r, fill = Algorithm))+
  geom_bar()+
  facet_grid(Algorithm ~ .)


relPerfDF = resultData %>%
  group_by(task_id, alg_id, Algorithm) %>%
  summarize(mcr =mean(mcr)) %>%
  ungroup() %>%
  group_by(task_id) %>%
  mutate(relMcr = (mcr[alg_id ==1] - mcr)/mcr[alg_id == 1]) %>%
  ungroup() %>%
  inner_join(tasksMetaFeats %>% mutate(id = as.numeric(id)), join_by(task_id == id)) %>%
  select(-mcr) %>%
  filter(alg_id != 1)

relPerfDF = resultData %>%
  group_by(task_id, alg_id, Algorithm) %>%
  summarize(sd = sd(mcr), mcr =mean(mcr)) %>%
  ungroup() %>%
  group_by(task_id) %>%
  mutate(relMcr = (mcr[alg_id ==1] - mcr)/sd[alg_id == 1]) %>%
  ungroup() %>%
  select(-mcr) %>%
  filter(alg_id != 1)


relPerfDF %>%
  ggplot(aes(x = relMcr, y = Algorithm))+
  geom_boxplot()+
  geom_vline(xintercept = 0, linetype = 2)+
  theme_minimal()+
  ylab('')+
  xlab('Relative Performance')
ggsave(
  file.path("figures", "relativePerformance.eps"),
  width = 10,
  height = 8,
  units = "cm"
)

