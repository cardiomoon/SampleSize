
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(plotly)
library(ggiraph)


source("numericInput4.R")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Test 1 Mean", tabName = "test1mean", icon = icon("dashboard")),
        menuItem("Compare 2 Means", tabName = "compare2mean", icon = icon("dashboard")),
        menuItem("Compare k Means", tabName = "comparekmean", icon = icon("dashboard")),
        menuItem("Test 1 Proportion", tabName = "test1prop", icon = icon("dashboard")),
        menuItem("Compare 2 Proportions", tabName = "compare2prop", icon = icon("dashboard")),
        menuItem("Compare Paired Proportions", tabName = "comparePairedprop", icon = icon("dashboard")),
        menuItem("Compare k Proportions", tabName = "comparekprop", icon = icon("dashboard")),
        menuItem("Test Time-To-Event Data", tabName = "survival", icon = icon("dashboard")),
        menuItem("Test Odds Ratio", tabName = "oddsratio", icon = icon("dashboard")),
        menuItem("About...", tabName = "about", icon = icon("dashboard"))
        
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "test1mean",
                radioButtons("t1mean","Select",
                             choices=c("1-Sample 2-Sided Equalty"=1,"1-Sample 1-Sided"=2,
                                       "1-Sample Non-Inferiority or Superiority"=3,"1-Sample Equivalence"=4),
                             selected=1),
                fluidRow(
                    box(title="Enter data",status="primary",
                        solidHeader=TRUE,
                        
                        numericInput4("mu","True mean, μ",value=2,step=1),
                        
                        numericInput4("mu0","Null hypothesis mean, μ0",value=1.5,step=1),
                        
                        numericInput4("sd0","Standard Deviation, σ",value=1,step=1),
                       
                        conditionalPanel(condition="input.t1mean=='3' | input.t1mean=='4'",
                                         numericInput4("delta","Non-inferiority or Superiority Margin, δ",value=10,step=1)
                                         
                        )
                    ),
                    box(title="power",status="primary",solidHeader=TRUE,
                        numericInput4("power1","Power, 1−β",value=0.8,step=0.1),
                        numericInput4("alpha1","Type I error rate, α",value=0.05,step=0.01),
                        numericInput4("FUloss1","Follow up loss rate",value=0.15,step=0.01),
                        numericInput4("compliance1","Compliance",value=0.95,step=0.01)
                    )
                ),
                fluidRow(
                    valueBoxOutput("sampleSize1"),
                    valueBoxOutput("sampleSize12",width=6)
                ),
                box(width=12,
                    plotlyOutput("plot2")),
                box(title="Used Equation",width=12,status="primary",solidHeader=TRUE,
                      htmlOutput("equation1")
                ),
                box(title="Reference",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("ref1")
                )
                
        ),
        tabItem(tabName = "compare2mean",
                
                radioButtons("ttest","Select",
                             choices=c("2-Sample 2-Sided Equalty"=1,"2-Sample 1-Sided"=2,
                                       "2-Sample Non-Inferiority or Superiority"=3,"2-Sample Equivalence"=4),
                             selected=1),
               fluidRow(
                box(title="Enter data",status="primary",
                    solidHeader=TRUE,
                  
                    numericInput4("mean1","Group 'A' mean, μA",value=5,step=1),
                 
                    numericInput4("mean2","Group 'B' mean, μB",value=10,step=1),
                   
                    conditionalPanel(condition="input.ttest!='2'",
                                     numericInput4("sd","Standard Deviation, σ",value=10,step=1)
                    ),
                    conditionalPanel(condition="input.ttest=='2'",
                                     numericInput4("sd1","Standard Deviation, σA",value=10,step=1),
                                     numericInput4("sd2","Standard Deviation, σB",value=10,step=1)
                    ),
                    conditionalPanel(condition="input.ttest=='3' | input.ttest=='4'",
                                     numericInput4("margin","Non-inferiority or Superiority Margin, δ",value=10,step=1)
                                     
                    ),
                    numericInput4("k","Sampling Ratio, κ=nA/nB",value=1,step=1)
                ),
                box(title="power",status="primary",solidHeader=TRUE,
                    numericInput4("power","Power, 1−β",value=0.8,step=0.1),
                    numericInput4("alpha","Type I error rate, α",value=0.05,step=0.01),
                    numericInput4("FUloss","Follow up loss rate",value=0.15,step=0.01),
                    numericInput4("compliance","Compliance",value=0.95,step=0.01)
                    )
                ),
                fluidRow(
                    valueBoxOutput("sampleSize2",width=3),
                    valueBoxOutput("sampleSize",width=3),
                    valueBoxOutput("sampleSize3",width=6)
                    
                ),
                box(width=12,
                    plotlyOutput("plot1")),
                box(title="Used Equation",width=12,status="primary",solidHeader=TRUE,
                   htmlOutput("equation2")
               ),
               box(title="Reference",width=12,status="primary",solidHeader=TRUE,
                   htmlOutput("ref2")
               )
        ),
        tabItem(tabName = "comparekmean",
                
                radioButtons("anova","Select",
                             choices=c("1-Way ANOVA Pairwise, 2-Sided Equalty"=1,
                                       "1-Way ANOVA Pairwise, 1-Sided Equalty"=2),
                             selected=1),
                fluidRow(
                    box(title="Enter data",status="primary",
                        solidHeader=TRUE,
                        
                        numericInput4("mean31","Group 'A' mean, μA",value=5,step=1),
                        
                        numericInput4("mean32","Group 'B' mean, μB",value=10,step=1),
                        
                        conditionalPanel(condition="input.anova=='1'",
                                         numericInput4("sd3","Standard Deviation, σ",value=10,step=1)
                        ),
                        conditionalPanel(condition="input.anova=='2'",
                                         numericInput4("sd31","Standard Deviation, σA",value=10,step=1),
                                         numericInput4("sd32","Standard Deviation, σB",value=10,step=1),
                                         numericInput4("kappa3","Sampling Ratio, κ=nA/nB",value=1,step=1)
                        ),
                        numericInput4("tau","Number of Pairwise Comparisons, τ",value=1,step=1)
                    ),
                    box(title="power",status="primary",solidHeader=TRUE,
                        numericInput4("power3","Power, 1−β",value=0.8,step=0.1),
                        numericInput4("alpha3","Type I error rate, α",value=0.05,step=0.01),
                        numericInput4("FUloss3","Follow up loss rate",value=0.15,step=0.01),
                        numericInput4("compliance3","Compliance",value=0.95,step=0.01)
                    )
                   
                ),
                fluidRow(
                    valueBoxOutput("sampleSize31",width=3),
                    valueBoxOutput("sampleSize32",width=6)
                    
                ),
                box(width=12,
                    
                    plotlyOutput("plot3")
                    ),
                box(title="Used Equation",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("equation3")
                ),
                box(title="Reference",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("ref3")
                )
        ),
        tabItem(tabName = "test1prop",
                
                radioButtons("t1p","Select",
                             choices=c("1-Sample, 2-Sided Equalty"=1,
                                       "1-Sample 1-Sided"=2,
                                       "1-Sample Non-Inferiority or Superiority"=3,
                                       "1-Sample Equivalence"=4),
                             selected=1),
                fluidRow(
                    box(title="Enter data",status="primary",
                        solidHeader=TRUE,
                        
                        numericInput4("prop","True Proportion, p",value=0.5,step=0.01),
                        
                        numericInput4("prop0","Null Hypothesis Proportion, p0",value=0.3,step=0.01),
                        
                        
                        conditionalPanel(condition="input.t1p=='3' | input.t1p=='4'",
                                         numericInput4("delta4","Non-inferiority or Superiority Margin, δ",value=10,step=1)
                                         
                        )
                    ),
                    box(title="power",status="primary",solidHeader=TRUE,
                        numericInput4("power4","Power, 1−β",value=0.8,step=0.1),
                        numericInput4("alpha4","Type I error rate, α",value=0.05,step=0.01),
                        numericInput4("FUloss4","Follow up loss rate",value=0.15,step=0.01),
                        numericInput4("compliance4","Compliance",value=0.95,step=0.01)
                    )
                ),
                fluidRow(
                    valueBoxOutput("sampleSize41",width=3),
                    valueBoxOutput("sampleSize42",width=6)
                    
                ),
                box(width=12,
                    plotlyOutput("plot4")),
                box(title="Used Equation",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("equation4")
                ),
                box(title="Reference",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("ref4")
                )
        ),
        tabItem(tabName = "compare2prop",
                
                radioButtons("c2p","Select",
                             choices=c("2-Sample 2-Sided Equalty"=1,"2-Sample 1-Sided"=2,
                                       "2-Sample Non-Inferiority or Superiority"=3,
                                       "2-Sample Equivalence"=4),
                             selected=1),
                fluidRow(
                    box(title="Enter data",status="primary",
                        solidHeader=TRUE,
                        
                        numericInput4("pA","Group 'A' Proportion, pA",value=0.85,step=0.01),
                        
                        numericInput4("pB","Group 'B' Proportion, pB",value=0.65,step=0.01),
                        
                        conditionalPanel(condition="input.c2p=='3' | input.c2p=='4'",
                                         numericInput4("delta5","Non-inferiority or Superiority Margin, δ",value=-0.10,step=0.01)
                                         
                        ),
                        numericInput4("k5","Sampling Ratio, κ=nA/nB",value=1,step=1)
                    ),
                    box(title="power",status="primary",solidHeader=TRUE,
                        numericInput4("power5","Power, 1−β",value=0.8,step=0.1),
                        numericInput4("alpha5","Type I error rate, α",value=0.05,step=0.01),
                        numericInput4("FUloss5","Follow up loss rate",value=0.15,step=0.01),
                        numericInput4("compliance5","Compliance",value=0.95,step=0.01)
                    )
                ),
                fluidRow(
                    valueBoxOutput("sampleSize51",width=3),
                    valueBoxOutput("sampleSize52",width=3),
                    valueBoxOutput("sampleSize53",width=6)
                    
                ),
                box(width=12,
                    plotlyOutput("plot5")),
                box(title="Used Equation",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("equation5")
                ),
                box(title="Reference",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("ref5")
                )
        ),
        tabItem(tabName = "comparePairedprop",

                radioButtons("cpp","Select",
                             choices=c("McNemar's Z-test, 2-Sided Equalty"=1,
                                       "McNemar's Z-test, 1-Sided"=2),
                                      # ,"2-Sample Non-Inferiority or Superiority"=3,
                                      #  "2-Sample Equivalence"=4),
                             selected=1),
           
                box(title="Enter numbers or proportions",status="primary",
                        solidHeader=TRUE,width=7,
                    fluidRow(
                            column(5,
                                   h4("Group "),
                                   hr(),
                                   HTML("<b>Group 'A', success</b>"),
                                   hr(),
                                   HTML("<b>Group 'A', failure</b>")
                                   ),
                            column(7,
                                   h4("Group 'B'"),
                                numericInput3("n11","success",value=96,step=1,width=80),

                                numericInput3("n10","failure",value=24,step=1,width=80),

                                br(),
                                numericInput3("n01","",value=45,step=1,width=80),

                                numericInput3("n00","",value=85,step=1,width=80)
                            )

                        )
                    ),
                    box(title="power",status="primary",solidHeader=TRUE,width=5,
                        numericInput4("power6","Power, 1−β",value=0.8,step=0.1,labelwidth=150),
                        numericInput4("alpha6","Type I error rate, α",value=0.05,step=0.01,labelwidth=150),
                        numericInput4("FUloss6","Follow up loss rate",value=0.15,step=0.01,labelwidth=150),
                        numericInput4("compliance6","Compliance",value=0.95,step=0.01,labelwidth=150)
                    ),
               
                fluidRow(
                    valueBoxOutput("sampleSize61",width=3),
                    valueBoxOutput("sampleSize63",width=6)

                ),
                box(width=12,
                    plotlyOutput("plot6")),
                box(title="Used Equation",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("equation6")
                ),
                box(title="Reference",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("ref6")
                )
        ),
        tabItem(tabName = "comparekprop",
                
                h3("1-Way ANOVA Pairwise"),
                box(title="Enter data",status="primary",
                    solidHeader=TRUE,
                   
                    numericInput4("pA7","Group 'A' Proportion, pA",value=0.2,step=0.01),
                    numericInput4("pB7","Group 'B' Proportion, pB",value=0.4,step=0.01),
                    numericInput4("tau7","Number of Pairwise Comparisons, τ",value=2,step=1)
                ),
                box(title="power",status="primary",solidHeader=TRUE,
                    numericInput4("power7","Power, 1−β",value=0.8,step=0.1),
                    numericInput4("alpha7","Type I error rate, α",value=0.05,step=0.01),
                    numericInput4("FUloss7","Follow up loss rate",value=0.15,step=0.01),
                    numericInput4("compliance7","Compliance",value=0.95,step=0.01)
                ),
                
                fluidRow(
                    valueBoxOutput("sampleSize71",width=3),
                    valueBoxOutput("sampleSize73",width=6)
                    
                ),
                box(width=12,
                    plotlyOutput("plot7")),
                box(title="Used Equation",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("equation7")
                ),
                box(title="Reference",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("ref7")
                )
        ),
        tabItem(tabName = "survival",
                radioButtons("coxph","Select",
                             choices=c("Cox PH, 2-Sided Equalty"=1,
                                       "Cox PH, 1-Sided, Non-Inferiority or Superiority"=2,
                                       "Cox PH, equivalence"=3),
                             selected=1),
                
                box(title="Enter data",status="primary",
                    solidHeader=TRUE,width=7,
                    
                    numericInput4("hr","Hazard Ratio, θ",value=2,step=1,labelwidth=260),
                    conditionalPanel(condition="input.coxph!='3'",
                    numericInput4("hr0","Null-Hypothesis Hazard Ratio, θ0",value=1,step=1,labelwidth=260)
                    ),
                    numericInput4("pE","Overall Probability of Event, pE",value=0.8,step=0.01,labelwidth=260),
                    numericInput4("pA8","Proportion of Sample in Group 'A', pA",value=0.5,step=1,labelwidth=260),
                    conditionalPanel(condition="input.coxph=='3'",
                    numericInput4("delta8","Non-inferiority or Superiority Margin, δ",value=0.5,step=1,labelwidth=260)
                    )
                ),
                box(title="power",status="primary",solidHeader=TRUE,width=5,
                    numericInput4("power8","Power, 1−β",value=0.8,step=0.1,labelwidth=150),
                    numericInput4("alpha8","Type I error rate, α",value=0.05,step=0.01,labelwidth=150),
                    numericInput4("FUloss8","Follow up loss rate",value=0.15,step=0.01,labelwidth=150),
                    numericInput4("compliance8","Compliance",value=0.95,step=0.01,labelwidth=150)
                ),
                
                fluidRow(
                    valueBoxOutput("sampleSize81",width=3),
                    valueBoxOutput("sampleSize83",width=6)
                    
                ),
                box(width=12,
                    plotlyOutput("plot8")),
                box(title="Used Equation",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("equation8")
                ),
                box(title="Reference",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("ref8")
                )
                
        ),
        tabItem(tabName = "oddsratio",
                radioButtons("oddratio","Select",
                             choices=c("Equalty"=1,
                                       "Non-Inferiority or Superiority"=2,
                                       "Equivalence"=3),
                             selected=1),
                
                box(title="Enter data",status="primary",
                    solidHeader=TRUE,width=7,
                    
                    numericInput4("pA9","Group 'A' Proportion, pA",value=0.40,step=0.01,labelwidth=260),
                    numericInput4("pB9","Group 'B' Proportion, pB",value=0.25,step=0.01,labelwidth=260),
                    conditionalPanel(condition="input.oddratio!='1'",
                        numericInput4("delta9","Non-inferiority or Superiority Margin, δ",value=0.5,step=0.1,labelwidth=260)
                    ),
                    numericInput4("k9","Sampling Ratio, κ=nA/nB",value=1,step=1,labelwidth=260)
                    
                ),
                box(title="power",status="primary",solidHeader=TRUE,width=5,
                    numericInput4("power9","Power, 1−β",value=0.8,step=0.1,labelwidth=150),
                    numericInput4("alpha9","Type I error rate, α",value=0.05,step=0.01,labelwidth=150),
                    numericInput4("FUloss9","Follow up loss rate",value=0.15,step=0.01,labelwidth=150),
                    numericInput4("compliance9","Compliance",value=0.95,step=0.01,labelwidth=150)
                ),
                
                fluidRow(
                    valueBoxOutput("sampleSize91",width=3),
                    valueBoxOutput("sampleSize92",width=3),
                    valueBoxOutput("sampleSize93",width=6)
                    
                ),
                box(width=12,
                    plotlyOutput("plot9")),
                box(title="Used Equation",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("equation9")
                ),
                box(title="Reference",width=12,status="primary",solidHeader=TRUE,
                    htmlOutput("ref9")
                )
        ),
        tabItem(tabName = "about",
                box(width=12,
                    uiOutput("about")
                    )
        )
    )
)

# Put them together into a dashboardPage
dashboardPage(
    dashboardHeader(title = "Sample Size"),
    sidebar,
    body
)