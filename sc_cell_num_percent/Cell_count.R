library(dplyr)

for(i in c("Sample", "Treatment","Species")) {
  for(j in c("Celltype")) {
    x <- celltype_summ(seuratdata = seuratdata, type1 = j, type2 = i)
    write.table(x, file = paste0("./Output/Main_Celltype/RPCA/2.6_umap_", j, "_", i, "_summ.xls"),
                quote = F, row.names = F, sep = "\t")
    
    x <- celltype_summ(seuratdata = seuratdata, type1 = j, type2 = i, precent = TRUE)
    write.table(x, file = paste0("./Output/Main_Celltype/RPCA/2.6_umap_", j, "_", i, "_summ_precent.xls"),
                quote = F, row.names = F, sep = "\t")
    
    x2 <- x[-length(rownames(x)),]
    x2 <- reshape2::melt(x2, id.vars = "Cell_type")
    x2$value <- sub("%", "", x2$value) %>% as.numeric()
    x2$Cell_type <- factor(x2$Cell_type, levels = levels(seuratdata@meta.data[[j]]))
    
    p1 <- ggplot(x2, aes(x = Cell_type, y = as.numeric(value))) +
      geom_bar(aes(fill = variable, color = "black"), position = "stack", stat = "identity", width = 0.75) +
      theme_classic() + labs(x = "", y = "", fill = "Sample") + guides(colour = "none") +
      scale_color_manual(values = c("black")) + scale_fill_tableau("Tableau 20") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7), legend.title = element_blank())
    # ggsave(p1, file = paste0("./Output/Main_Celltype/RPCA/2.6_umap_", j, "_", i, "_summ_precent1.png"), width = 8, height = 5)
    ggsave(p1, file = paste0("./Output/Main_Celltype/RPCA/2.6_umap_", j, "_", i, "_summ_precent1.pdf"), width = 8, height = 6)
    
    p1 <- ggplot(x2, aes(x = Cell_type, y = as.numeric(value))) +
      geom_bar(aes(fill = variable, color = "black"), position = "fill", stat = "identity", width = 0.75) +
      theme_classic() + labs(x = "", y = "", fill = "Sample") + guides(colour = "none") +
      scale_color_manual(values = c("black")) + scale_fill_tableau("Tableau 20") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7), legend.title = element_blank())
    # ggsave(p1, file = paste0("./Output/Main_Celltype/RPCA/2.6_umap_", j, "_", i, "_summ_precent2.png"), width = 8, height = 5)
    ggsave(p1, file = paste0("./Output/Main_Celltype/RPCA/2.6_umap_", j, "_", i, "_summ_precent2.pdf"), width = 8, height = 6)
  }
}