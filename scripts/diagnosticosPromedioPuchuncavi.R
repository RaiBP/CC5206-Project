ggplot(enPuchuncavi[order(enPuchuncavi$Rel, decreasing = T),][1:30,], aes(x=reorder(Diag, Rel), y=Rel)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Porcentajes relativos de diagnósticos, Puchuncavi promedio 2002 a 2017") + 
  xlab("Diagnóstico") + ylab("Porcentaje relativo sobre promedio nacional")
