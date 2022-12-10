IDmapbitr<-bitr(anova.res_specifc$path,fromType = "SYMBOL",
                toType = "ENTREZID",OrgDb = org.Hs.eg.db, drop = F)
IDmapbitr %<>% na.omit() %>% unique() %>% dplyr::filter(SYMBOL!="")

k=try(enrichKEGG(IDmapbitr$ENTREZID,
                 pvalueCutoff = 1,
                 qvalueCutoff = 1,
                 organism = "hsa"))
k<-try(setReadable(k,OrgDb = org.Hs.eg.db,keyType = "ENTREZID"))

p.specific <-barplot(k,showCategory = 5)+
  scale_fill_gradient(low = "#D61F20",high = "#3178AD")+
  ylab("")+xlab("")+
  theme_test()+
  theme(axis.text = element_text(color="black"))

ggsave("specific_kegg_top5.png",width = 5,height = 3,dpi=300)

go<-as.data.frame(g.specific)
go$Description %<>% stringr::str_wrap(width = 60)
go_10<-go %>% arrange(pvalue) %>% group_by(ONTOLOGY) %>% dplyr::filter(row_number()<=5) %>% arrange(desc(pvalue))
go_10$Description<-factor(go_10$Description,levels=go_10$Description)

go_10.bp<-go_10[go_10$ONTOLOGY=="BP",]
go_10.cc<-go_10[go_10$ONTOLOGY=="CC",]
go_10.mf<-go_10[go_10$ONTOLOGY=="MF",]

p.bp<-go_10.bp %>% 
  ggplot(aes(x=c(-log10(pvalue)),y=Description)) +
  geom_bar(fill="red",stat = "identity",width = 0.3)+
  ylab("") +
  xlab("-Log10(P-value)") +
  labs(title = "Go-Analysis_BP")+
  theme_bw()+
  scale_x_continuous(expand=c(0,0.01),position="top")+
  scale_y_discrete(expand=c(0.05,0.05))+
  theme(plot.margin=unit(c(1,0.5,0.5,0.5),'cm'),
        axis.text.x=element_text(colour='black', size=10),
        axis.text.y=element_text(colour='black', size=10),
        axis.ticks.y = element_blank()) 

p.cc<-go_10.cc %>% 
  ggplot(aes(x=c(-log10(pvalue)),y=Description)) +
  geom_bar(fill="red",stat = "identity",width = 0.3)+
  ylab("") +
  xlab("-Log10(P-value)") +
  labs(title = "Go-Analysis_CC")+
  theme_bw()+
  scale_x_continuous(expand=c(0,0.01),position="top")+
  scale_y_discrete(expand=c(0.05,0.05))+
  theme(plot.margin=unit(c(1,0.5,0.5,0.5),'cm'),
        axis.text.x=element_text(colour='black', size=10),
        axis.text.y=element_text(colour='black', size=10),
        axis.ticks.y = element_blank()) 

p.mf<-go_10.mf %>% 
  ggplot(aes(x=c(-log10(pvalue)),y=Description)) +
  geom_bar(fill="red",stat = "identity",width = 0.3)+
  ylab("") +
  xlab("-Log10(P-value)") +
  labs(title = "Go-Analysis_MF")+
  theme_bw()+
  scale_x_continuous(expand=c(0,0.01),position="top")+
  scale_y_discrete(expand=c(0.05,0.05))+
  theme(plot.margin=unit(c(1,0.5,0.5,0.5),'cm'),
        axis.text.x=element_text(colour='black', size=10),
        axis.text.y=element_text(colour='black', size=10),
        axis.ticks.y = element_blank()) 

p.bp+p.cc+p.mf


ggsave("specific_go_top5.png",width = 14,height = 3,dpi=300)