rna<-fread("rna_tissue_consensus.tsv")

brain <- c("amygdala","basal ganglia","cerebellum","cerebral cortex","hippocampal formation","hypothalamus",
           "midbrain","pituitary gland","pons","thalamus","white matter")

wuguan <- c("retina","salivary gland","tongue")
  
sex <- c("cervix","endometrium","epididymis","fallopian tube","ovary","placenta","seminal vesicle","prostate","testis","vagina")

rna %<>% 
  dplyr::mutate(Tissue2 = case_when(Tissue %in% brain ~ "Brain",
                                   Tissue %in% wuguan ~ "wuguan",
                                   Tissue %in% sex ~ "sex",
                                   TRUE ~ "other"))
rna %>% 
  ggplot(aes(x = Tissue2,y=nTPM))+
  geom_boxplot()+
  theme_test()+
  theme(axis.text.x = element_text(angle = 45,hjust= 1),
        axis.text = element_text(colour = "black"))+
  xlab("")+ylab("")
ggsave("4tissuttype_boxtplot.pdf",width = 4,height = 3,dpi=300)