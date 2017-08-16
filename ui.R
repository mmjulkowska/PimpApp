fluidPage(
  theme = shinytheme("united"),
  navbarPage(">> PIMP App <<", 
  
tabPanel("Background information", icon=icon("info"),
h2("About the PimpApp"),
"Currently the App is designed to help digg through the data and identify the most interesting accessions - data is coming from The Plant Accelerator"
# end of Tab1
),
             
# start tabPanel # 2

tabPanel("Explore the Phenotypes of Individual Accessions", icon=icon("leaf"),
sidebarPanel(
  fluidRow(
  h3("Examine one accession at the time"),
  helpText("In the study of diversity panels, the phenotypes of individual accessions are often overlooked. In the tabs below you can select the accession and phenotype of interest to be explored.", br(), br(),
    "The boxplot (upper panel) represent the variation within the selected accession based on few replicates, while the histogram (lower panel), represents the variation observed for the entire diversity panel across different conditions studied" ),
  selectInput("genotype", label=("Genotype of interest"), choices=unique(pimp$Accession), selected="M256"),
  selectInput("Trait", label=("Chose a phenotype to display"), choices=list("Na per Dry Weight" = "NapDW", "Na [mM]" = "Na_mM", "K per Dry Weight" = "KpDW", "K [mM] " = "K_mM", "RATIO Na per K" = "NapK"),selected="NapDW"),
  downloadButton("dwnldSub1", label = "Download Data")
    )),

mainPanel(
  plotOutput("plot1"),
  plotOutput("histo1"))
# end tabPanel # 1
  ),

# start tabPanel # 3

tabPanel("Compare Individual Accessions", icon=icon("balance-scale"),
sidebarPanel(
  fluidRow(
  h3("Mix and Match"),
  helpText("In this tab you can select up to eight genotypes to compare in different conditions, which are presented in individual sub-tabs.", br(), br(), 
           "The boxplots represent the selected phenotype of eight chosen accessions, and the letters above the graphs represent the significant groups as calculated using Tukey pair-wise comparison with p-value < 0.05.", br(), br(),
  strong("DISCLAIMER:"), "As the results presented in here are based only on very few biological replicates, the significant differences between the accessions should be interpreted with caution."),
    column(8, offset = 1,
    selectInput("tr2", label=("Chose a phenotype to display"), choices=list("Na per Dry Weight" = "NapDW", "Na [mM]" = "Na_mM", "K per Dry Weight" = "KpDW", "K [mM] " = "K_mM", "RATIO Na per K" = "NapK"),selected="NapDW"),
    column(4, offset =1, 
      selectInput("gen1", label=("Genotype 1"), choices=unique(pimp$Accession), selected="M017"),
      selectInput("gen2", label=("Genotype 2"), choices=unique(pimp$Accession), selected="M217"),
      selectInput("gen3", label=("Genotype 3"), choices=unique(pimp$Accession), selected="M282"),
      selectInput("gen4", label=("Genotype 4"), choices=unique(pimp$Accession), selected="M234")), 
    column(4, offset =1,
      selectInput("gen5", label=("Genotype 5"), choices=unique(pimp$Accession), selected="M130"), 
      selectInput("gen6", label=("Genotype 6"), choices=unique(pimp$Accession), selected="M001"), 
      selectInput("gen7", label=("Genotype 7"), choices=unique(pimp$Accession), selected="M046"),
      selectInput("gen8", label=("Genotype 8"), choices=unique(pimp$Accession), selected="M050")), 
    downloadButton("dataAcc", label="Download data")
    ))),
mainPanel(
  navbarPage("",
  tabPanel("Control", plotOutput("plot2a")), 
  tabPanel("Salt Stress", plotOutput("plot2b"))))
# end tabPanel # 3
  ),

# start tabPanel # 4
tabPanel("Explore the Correlations between Individual Traits", icon=icon("line-chart"),
  sidebarPanel(
  fluidRow(
  h3("How does it correlate?"),
    helpText("In this tab you can explore the correlations between individual traits. The values representing individual accessions are based on the average calculated per accession per condition. The Pearson correlation coefficient (r2) and p-value are plotted above the graphs", br(),
    "We hope that those correlation graphs will be helpful to select accessions showing extreme phenotypes, that can be used in, for example for bulked segregant analysis or in depth individual study of the accession."),
      selectInput("Trait3", label=("Trait 1"), choices=list("Na per Dry Weight Control" = "NapDW.m_0", "Na per Dry Weight Salt Stress" = "NapDW.m_75", "Na [mM] Control" = "Na_mM.m_0", "Na [mM] Salt Stress" = "Na_mM.m_75", "K per Dry Weight Control" = "KpDW.m_0", "K per Dry Weight Salt Stress" = "KpDW.m_75", "K [mM] Control" = "K_mM.m_0", "K [mM] Salt Stress" = "K_mM.m_75", "RATIO Na per K Control" = "NapK.m_0", "RATIO Na per K Salt Stress" = "NapK.m_75"),selected="NapDW.m_75"),
      selectInput("Trait4", label=("Trait 2"), choices=list("Na per Dry Weight Control" = "NapDW.m_0", "Na per Dry Weight Salt Stress" = "NapDW.m_75", "Na [mM] Control" = "Na_mM.m_0", "Na [mM] Salt Stress" = "Na_mM.m_75", "K per Dry Weight Control" = "KpDW.m_0", "K per Dry Weight Salt Stress" = "KpDW.m_75", "K [mM] Control" = "K_mM.m_0", "K [mM] Salt Stress" = "K_mM.m_75", "RATIO Na per K Control" = "NapK.m_0", "RATIO Na per K Salt Stress" = "NapK.m_75"),selected="NapDW.m_0")
#, downloadButton("downloadPlot3", label = "Download Plot")
    )),
mainPanel(helpText("The Pearson Correlation coefficient (r2) is ", textOutput("corr"),br(),"The p-value is ",textOutput("corpval")),
  plotlyOutput("plot3", height = 600, width = 600))
# end tabPanel # 4
  ),


# start tabPanel # 5
tabPanel("Cluster Analysis", icon=icon("magic"),
  sidebarPanel(
  fluidRow(
    h3("So you think you can cluster?"),
    helpText("Cluster analysis can be performed for any set of chosen traits. Therefore, in this tab we integrated the dataset so that you can perform your own cluster analysis on the traits of your interest.", br(), br()),
    navbarPage("Let's cluster",
    tabPanel("Step 1",
      helpText("Please select three phenotypes that you would like to take into consideration for the cluster analysis.", br(),
      "You will see the heat-map of the chosen phenotypes appear in the first tab. The value of the phenotypes is normalized with z-Fisher transformation, so the color scale is normalized for each phenotype"),
          selectInput("Clust1", label="Trait 1", choices=list("Na per Dry Weight Control" = "NapDW.m_0", "Na per Dry Weight Salt Stress" = "NapDW.m_75", "Na [mM] Control" = "Na_mM.m_0", "Na [mM] Salt Stress" = "Na_mM.m_75", "K per Dry Weight Control" = "KpDW.m_0", "K per Dry Weight Salt Stress" = "KpDW.m_75", "K [mM] Control" = "K_mM.m_0", "K [mM] Salt Stress" = "K_mM.m_75", "RATIO Na per K Control" = "NapK.m_0", "RATIO Na per K Salt Stress" = "NapK.m_75"),selected="NapDW.m_75"),
          selectInput("Clust2", label="Trait 2", choices=list("Na per Dry Weight Control" = "NapDW.m_0", "Na per Dry Weight Salt Stress" = "NapDW.m_75", "Na [mM] Control" = "Na_mM.m_0", "Na [mM] Salt Stress" = "Na_mM.m_75", "K per Dry Weight Control" = "KpDW.m_0", "K per Dry Weight Salt Stress" = "KpDW.m_75", "K [mM] Control" = "K_mM.m_0", "K [mM] Salt Stress" = "K_mM.m_75", "RATIO Na per K Control" = "NapK.m_0", "RATIO Na per K Salt Stress" = "NapK.m_75"),selected="KpDW.m_75"),
          selectInput("Clust3", label="Trait 3", choices=list("Na per Dry Weight Control" = "NapDW.m_0", "Na per Dry Weight Salt Stress" = "NapDW.m_75", "Na [mM] Control" = "Na_mM.m_0", "Na [mM] Salt Stress" = "Na_mM.m_75", "K per Dry Weight Control" = "KpDW.m_0", "K per Dry Weight Salt Stress" = "KpDW.m_75", "K [mM] Control" = "K_mM.m_0", "K [mM] Salt Stress" = "K_mM.m_75", "RATIO Na per K Control" = "NapK.m_0", "RATIO Na per K Salt Stress" = "NapK.m_75"),selected="NapDW.m_0")),
    tabPanel("Step 2",
      helpText("Have a look at the dendrogram by clicking on the tab >> Accession Dendrogram << and determine the number of clusters that you would like to make by choosing a value on the x-axis representing the Ward linkage between the accessions", br(), br(),
      strong("ALERT:"), "Please note that if you chose too many clusters this analysis will not be informative and the subsequent post-hoc analysis will not be possible. We advise for the optimal number of clusters between three to ten.", br(), br(),
      "Below please enter the value at which you wish to cut the dendrogram - make sure to enter ONLY numerical values. IF you wish to enter values < 1, please use dot - as in 0.01."),
      textInput("tree_cut", label="Cut tree at", value="0.3")),
    tabPanel("Step 3",
      helpText("Compare the clusters by clicking on the tab >> Cluster Validation << to examine significant differences between your clusters.", br(),br(),
      strong("DISCLAIMER:"), "Note that the differences between the clusters may be due to the unequal distribution of the accessions between the samples. Therefore, the results presented in here should be interpreted with caution.", br(),br(),
      "Below please select the trait for which you wish to validate your cluster analysis"),
      selectInput("Clust_test", label="chose a trait to test", choices=list("Na per Dry Weight Control" = "NapDW.m_0", "Na per Dry Weight Salt Stress" = "NapDW.m_75", "Na [mM] Control" = "Na_mM.m_0", "Na [mM] Salt Stress" = "Na_mM.m_75", "K per Dry Weight Control" = "KpDW.m_0", "K per Dry Weight Salt Stress" = "KpDW.m_75", "K [mM] Control" = "K_mM.m_0", "K [mM] Salt Stress" = "K_mM.m_75", "RATIO Na per K Control" = "NapK.m_0", "RATIO Na per K Salt Stress" = "NapK.m_75"),selected="NapDW.m_75")),
    tabPanel("Step 4",
      helpText("Download the data to examine which accessions belong to which cluster"),
      downloadButton("dwnldClust", label = "Download Cluster data"))
  ))),
  
  mainPanel(
    navbarPage(title="",
      tabPanel("Clustering Heat Map",
        helpText("This graph represents the clustering of the accessions based on three selected traits. The colors represent the phenotype value which has been normalized per trait with Z-Fisher transformation. Please note that the data per accession is based on the average trait value collected from few replicas only. "),
        plotOutput("HotHeatMap")),
      tabPanel("Accession Dendrogram",
        helpText("The below graph represents a dendrogram of the accessions and how they are clustered based on the selected traits in", strong("Step1"),". The accessions are clustered using Ward linkage method and the x-axis is represents the distance between the individual accessions."),
        plotOutput("ClusterTree")),
      tabPanel("Cluster Validation",
        helpText("The boxplot represents the phenotype value chosen in ", strong("Step 3 "), " per cluster. The average values per accessions were pulled depending on the cluter cut-off value determined in ", strong("Step 2"),". The letters above the graph represent the significant groups as calculated using Tukey pair-wise comparison with p-value < 0.05."),
        plotOutput("HotANOVA"))
                        ))
             # end of tabPanel#5
  )))