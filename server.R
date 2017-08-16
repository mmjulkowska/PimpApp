function(input,output){
  
 MyPlot1 <-function(){
    temp <- subset(pimp, pimp$Accession == input$genotype)
    pheno <- temp[,c("Treatment",input$Trait)]
    pheno[,1] <- as.factor(pheno[,1])
    colnames(pheno)[2] <- "PoI"
    ggplot(data = pheno, aes(x = Treatment, y = PoI, fill = Treatment)) + geom_boxplot() + labs(title = paste("The ", input$Trait, " of ", input$genotype), xlab="NaCl (mM)", ylab=input$Trait)
    # TO DO POINT # TO DO POINT # TO DO POINT # TO DO POINT # TO DO POINT 
    # still need to beautify the plot - title and the axis are horrible!
    # TO DO POINT # TO DO POINT # TO DO POINT # TO DO POINT # TO DO POINT 
  }
  
  output$plot1 <- renderPlot({
    MyPlot1()
  })
  
  MD1 <- function(){
    temp <- subset(pimp, pimp$Accession == input$genotype)
    MD1_a <- temp[,c("Accession","Treatment",input$Trait)]
    MD1_a
  }
  
  output$dwnldSub1 <- downloadHandler(
    filename=function(){
      paste(input$Trait, " of ",input$genotype,".csv", sep="")},
    content <- function(file){
      write.csv(MD1(), file)}
  )
  
  myPlotHisto <- function(){
    histo <- pimp[,c("Treatment",input$Trait)]
    histo[,1] <- as.factor(histo[,1])
    colnames(histo)[2] <- "Le_trait"
    a = min(histo$Le_trait, na.rm=TRUE)
    b = max(histo$Le_trait, na.rm=TRUE)
    var = input$Trait
    plot <- ggplot(histo, aes(Le_trait, fill = Treatment)) + geom_density(alpha=0.2) + xlim(a,b) + ggtitle(paste(input$Trait, " of Natural Diversity Panel")) + xlab(input$Trait) + ylab("% of the accessions")
    plot
  }
  
  output$histo1 <- renderPlot({
    myPlotHisto()
  })
  
  MA1 <- function(){
    temp1 <- subset(pimp, pimp$Accession == input$gen1)
    temp2 <- subset(pimp, pimp$Accession == input$gen2)
    temp3 <- subset(pimp, pimp$Accession == input$gen3)
    temp4 <- subset(pimp, pimp$Accession == input$gen4)
    temp5 <- subset(pimp, pimp$Accession == input$gen5)
    temp6 <- subset(pimp, pimp$Accession == input$gen6)
    temp7 <- subset(pimp, pimp$Accession == input$gen7)
    temp8 <- subset(pimp, pimp$Accession == input$gen8)
    temp14 <- rbind(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8)
    MyAcc1 <- temp14[,c("Accession","Treatment", input$tr2)]
    MyAcc1
  }
  
  output$dataAcc <- downloadHandler(
    filename=function(){
      paste(input$Trait, " of ",input$gen1,", ", input$gen2,", ", input$gen3,", ", input$gen4,", ", input$gen5,", ", input$gen6,", ", input$gen7,", ", input$gen8,".csv", sep="")},
    content <- function(file){
      write.csv(MA1(), file)}
  )
  
  output$plot2a <- renderPlot({
    temp1 <- subset(pimp, pimp$Accession == input$gen1)
    temp2 <- subset(pimp, pimp$Accession == input$gen2)
    temp3 <- subset(pimp, pimp$Accession == input$gen3)
    temp4 <- subset(pimp, pimp$Accession == input$gen4)
    temp5 <- subset(pimp, pimp$Accession == input$gen5)
    temp6 <- subset(pimp, pimp$Accession == input$gen6)
    temp7 <- subset(pimp, pimp$Accession == input$gen7)
    temp8 <- subset(pimp, pimp$Accession == input$gen8)
    temp14 <- rbind(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8)
    pheno2 <- temp14[,c("Accession","Treatment", input$tr2)]
    pheno2[,1] <- as.factor(pheno2[,1])
    pheno2[,2] <- as.factor(pheno2[,2])
    names(pheno2)[3] <- "pheno"
    pheno_0 <- subset(pheno2, pheno2$Treatment == "C")
    
    # Doing ANOVA and Post-Hoc analysis at 0mM NaCl
    # ok let's try something more simple like boxplot 
    #boxplot(pheno_0[,3] ~ pheno_0[,1], main="0 mM NaCl", xlab="", col=c("blue"))
    amod <- aov(pheno ~ Accession, data = pheno_0)
    tuk <- glht(amod, linfct = mcp(Accession = "Tukey"))
    tuk.cld <- cld(tuk)   
    old.par <- par( mai=c(1,1,1.25,1))
    plot(tuk.cld, las=1, col="green4", ylab=input$tr2)
    
  })
  
  output$plot2b <- renderPlot({
    temp1 <- subset(pimp, pimp$Accession == input$gen1)
    temp2 <- subset(pimp, pimp$Accession == input$gen2)
    temp3 <- subset(pimp, pimp$Accession == input$gen3)
    temp4 <- subset(pimp, pimp$Accession == input$gen4)
    temp5 <- subset(pimp, pimp$Accession == input$gen5)
    temp6 <- subset(pimp, pimp$Accession == input$gen6)
    temp7 <- subset(pimp, pimp$Accession == input$gen7)
    temp8 <- subset(pimp, pimp$Accession == input$gen8)
    temp14 <- rbind(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8)
    pheno2 <- temp14[,c("Accession","Treatment", input$tr2)]
    pheno2[,1] <- as.factor(pheno2[,1])
    pheno2[,2] <- as.factor(pheno2[,2])
    names(pheno2)[3] <- "pheno"
    pheno_S <- subset(pheno2, pheno2$Treatment == "S")
    
    # Doing ANOVA and Post-Hoc analysis at 0mM NaCl
    # ok let's try something more simple like boxplot 
    #boxplot(pheno_0[,3] ~ pheno_0[,1], main="0 mM NaCl", xlab="", col=c("blue"))
    amod <- aov(pheno ~ Accession, data = pheno_S)
    tuk <- glht(amod, linfct = mcp(Accession = "Tukey"))
    tuk.cld <- cld(tuk)   
    old.par <- par( mai=c(1,1,1.25,1))
    plot(tuk.cld, las=1, col="tomato4", ylab=input$tr2)
  })
  
  output$plot3 <- renderPlotly({
    pheno3 <- PIMPavg_all[,c("Accession",input$Trait3, input$Trait4)]
    pheno3 <- na.omit(pheno3)
    
    p <- plot_ly(pheno3, x= ~pheno3[,2], y=~pheno3[,3], type = 'scatter', mode = 'markers', text = ~pheno3[,1]) %>% 
      layout(xaxis = list(title = paste(input$Trait3)), yaxis = list(title = paste(input$Trait4)))
    #layout(title = "awesome correlation", xaxis = list(title = input$Trait3, zeroline = T), yaxis=list(title = input$Trait4, zeroline = T))
  })
  
  output$corr <- renderText({
    pheno3 <- PIMPavg_all[,c("Accession",input$Trait3, input$Trait4)]
    pheno3 <- na.omit(pheno3)
    correl <- cor(pheno3[,2], pheno3[,3])
    correl
  })
  
  output$corpval <- renderText({
    pheno3 <- PIMPavg_all[,c("Accession",input$Trait3, input$Trait4)]
    pheno3 <- na.omit(pheno3)
    pval <- cor.test(pheno3[,2], pheno3[,3])$p.val
    pval[1]
  })
  
  # Cluster tree of ALL accessions based on three selected traits
  output$ClusterTree <- renderPlot({
    # make a temporary subset based on Clust1, Clust2 and Clust3 (les traits)
    clust_temp <- PIMPavg_all[,c("Accession", input$Clust1, input$Clust2, input$Clust3)]
    clust_temp <- na.omit(clust_temp)
    PIMP_matrix <- clust_temp[,2:4]
    row.names(PIMP_matrix) <- clust_temp$Accession
    PIMP_matrix = as.matrix(PIMP_matrix)
    PIMP_t_matrix = t(PIMP_matrix)
    PIMP_t_cor = cor(PIMP_t_matrix,method=c("pearson"))
    PIMP_t_dist = dist(PIMP_t_cor)
    PIMP_t_clust = hclust(PIMP_t_dist, method="ward.D2")
    Clufa <- plot(as.dendrogram(PIMP_t_clust), horiz=T)
    Clufa
  })
  # 
  
  output$HotHeatMap <- renderPlot({
    clust_temp <- PIMPavg_all[,c("Accession", input$Clust1, input$Clust2, input$Clust3)]
    clust_temp <- na.omit(clust_temp)
    PIMP_matrix <- clust_temp[,2:4]
    row.names(PIMP_matrix) <- clust_temp$Accession
    PIMP_matrix = as.matrix(PIMP_matrix)
    PIMP_t_matrix = t(PIMP_matrix)
    PIMP_t_cor = cor(PIMP_t_matrix,method=c("pearson"))
    PIMP_t_dist = dist(PIMP_t_cor)
    PIMP_t_clust = hclust(PIMP_t_dist, method="ward.D2")
    heatmap.2(PIMP_t_matrix, Colv=as.dendrogram(PIMP_t_clust), col=blue2red(100),scale=c("row"),density.info="none",trace="none", cexRow=0.7)
  })
  
  output$HotANOVA <- renderPlot({
    clust_temp <- PIMPavg_all[,c("Accession", input$Clust1, input$Clust2, input$Clust3)]
    clust_temp <- na.omit(clust_temp)
    PIMP_matrix <- clust_temp[,2:4]
    row.names(PIMP_matrix) <- clust_temp$Accession
    PIMP_matrix = as.matrix(PIMP_matrix)
    PIMP_t_matrix = t(PIMP_matrix)
    PIMP_t_cor = cor(PIMP_t_matrix,method=c("pearson"))
    PIMP_t_dist = dist(PIMP_t_cor)
    PIMP_t_clust = hclust(PIMP_t_dist, method="ward.D2")
    # cut_tree at $tree_cut value (but first make it numeric)
    cluster <- as.data.frame(cutree(PIMP_t_clust,h=as.numeric(input$tree_cut)))
    names(cluster)[1] <- "cluster"
    # To fuse the cluster with the initial data - since we have a chance of non-equal number of columns, we have to transform the entire data set into a matrix as well:
    PIMP_matrix_all <- PIMPavg_all[,2:ncol(PIMPavg_all)]
    row.names(PIMP_matrix_all) <- PIMPavg_all[,"Accession"]
    # and mering two matrixes by row.names - this is the only working solution I found
    new <- merge(cluster, PIMP_matrix_all, by="row.names")
    names(new)[1] <- "Accession"
    
    # then make subset of the file based on the trait chosen in here:
    to_test <- new[,c("Accession","cluster",input$Clust_test)]
    names(to_test)[3] <- "phenotype"
    to_test[,2] <- as.factor(to_test[,2])
    # then paste here ANOVA script with cluster as testing variable
    amod <- aov(phenotype ~ cluster, data = to_test)
    tuk <- glht(amod, linfct = mcp(cluster = "Tukey"))
    tuk.cld <- cld(tuk)   
    old.par <- par( mai=c(1,1,1.25,1))
    plot(tuk.cld, las=1, col="tomato4", ylab=input$Clust_test)
  })
  
  CLU1 <- function(){
    clust_temp <- PIMPavg_all[,c("Accession", input$Clust1, input$Clust2, input$Clust3)]
    clust_temp <- na.omit(clust_temp)
    PIMP_matrix <- clust_temp[,2:4]
    row.names(PIMP_matrix) <- clust_temp$Accession
    PIMP_matrix = as.matrix(PIMP_matrix)
    PIMP_t_matrix = t(PIMP_matrix)
    PIMP_t_cor = cor(PIMP_t_matrix,method=c("pearson"))
    PIMP_t_dist = dist(PIMP_t_cor)
    PIMP_t_clust = hclust(PIMP_t_dist, method="ward.D2")
    # cut_tree at $tree_cut value (but first make it numeric)
    cluster <- as.data.frame(cutree(PIMP_t_clust,h=as.numeric(input$tree_cut)))
    names(cluster)[1] <- "cluster"
    # To fuse the cluster with the initial data - since we have a chance of non-equal number of columns, we have to transform the entire data set into a matrix as well:
    PIMP_matrix_all <- PIMPavg_all[,2:ncol(PIMPavg_all)]
    row.names(PIMP_matrix_all) <- PIMPavg_all[,"Accession"]
    # and mering two matrixes by row.names - this is the only working solution I found
    new <- merge(cluster, PIMP_matrix_all, by="row.names")
    names(new)[1] <- "Accession"
    new
  }
  
  
  output$dwnldClust <- downloadHandler(
    filename=function(){
      paste("Accession clustering based on ",input$Clust1, ", ",input$Clust2," and ",input$Clust3,".csv", sep="")},
    content <- function(file){
      write.csv(CLU1(), file)}
  )
  # end of script here
}