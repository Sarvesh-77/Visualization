library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggcorrplot)

mydata<-read_csv("survey_lung_cancer.csv")

display1 = append(mydata[1:3],mydata[8:10])
display = data.frame(display1, mydata[16])
df=mydata
head(display)
op <- as.data.frame(t(as.data.frame(t(mydata))[16,]))
opt = as.data.frame(t(op))
for (i in (1:length(opt))){
  if (opt[i] == 'YES'){
    opt[i] = 2
  }
  else {
    opt[i] = 1
  }
}
mydata[16] = as.data.frame(t(opt))
output = as.data.frame(t(mydata[,16]))
count = 0
total = 0
for (i in t(mydata[16])){
  if (i == 2){
    count = count + 1
  }
  total = total + 1
}
pos = count
vals = c()
truths = as.data.frame(t(mydata[16]))
for (i in mydata[,3:15]){
  count = 0
  for (j in (1:length(t(i)))){
    if (truths[j] == 2){
      count = count + t(i)[j] - 1
    }
  }
  vals = append(vals, count/pos*100)
}
symptoms = c(colnames(mydata)[3:15])
plot1 = data.frame(symptoms, vals)
ages = as.data.frame(t(mydata[2]))
pos_ages = c()
for (i in (1:length(truths))){
  if (truths[i] == 2){
    pos_ages = append(pos_ages, t(mydata[2])[i]) 
  }
}
ages = data.frame(pos_ages)
vals = c()
truths = as.data.frame(t(mydata[16]))
for (i in mydata[,3:15]){
  count = 0
  for (j in (1:length(t(i)))){
    if (truths[j] == 2){
      count = count + t(i)[j] - 1
    }
  }
  vals = append(vals, count/pos*100)
}
symptoms = c(colnames(df)[3:15])

ui <- dashboardPage(
  dashboardHeader(title="Lung Cancer Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Univariate Analysis", tabName = "singlevarmenu"),
      menuItem("Multivariate Analysis", tabName = "multivarmenu"),
      menuItem("Data", tabName = "data"),
      menuItem("Conclusion", tabName = "conc")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("intro",
        fluidRow(h1("Introduction"),align='center'),
        h3("This projects aims to discuss the various characteristics of lung cancer patients. We observe different causes and symptoms of patients and analyze them using different graphs using ggplot and various other tools in R. We make use of different types of plots such as histograms, pie charts, bar graphs etc. for visualization."),
        br(),
        br(),
        tags$div(tags$img(src='lung-cancer-tumor-8col.png',height=350, width=650),align='center')
      ),
  
      tabItem("singlevarmenu",
              tabsetPanel(
                tabPanel("Categorical Variables",
                         selectInput("catvar", "Categorical Variable", colnames(mydata)[c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]),
                         plotOutput("piechart"),
                         h4("Frequency Table"),
                         fluidRow(tableOutput("frequency")),
                         align = 'center'
                         ),
                tabPanel("Numeric Variables",
                         selectInput("numvar", "Numeric Variable", colnames(mydata)[c(2)]),
                         h4("Summary Statistics"),
                         tableOutput("stats"),
                         h4("Plots"),
                         fluidRow(plotOutput("histogram")),
                         align = 'center'
                )
              )),
      tabItem("multivarmenu",
              tabsetPanel(
                tabPanel("Analysis of Causes",
                         h3("Analyse 2 types of causes of Lung Cancer against each other"),
                         selectInput("catvar1",  "Variable 1", colnames(mydata)[c(3,6,7,11)]),
                         selectInput("catvar2", "Variable 2", colnames(mydata)[c(3,6,7,11)], selected="PEER_PRESSURE"),
                         fluidRow(plotOutput("barchart1")),
                         align = 'center'
                         ),
                tabPanel("Analysis of Symptoms",
                         h3("Analyse 2 types of symptoms of Lung Cancer against each other"),
                         selectInput("catvar3",  "Variable 1", colnames(mydata)[c(4,5,8,9,10,12,13,14,15)]),
                         selectInput("catvar4", "Variable 2", colnames(mydata)[c(4,5,8,9,10,12,13,14,15)], selected="ANXIETY"),
                         fluidRow(plotOutput("barchart2")),
                         align = 'center'
                         ),
                tabPanel("Correlation of different features",
                         h3("Correlation plot of different characteristics"),
                         br(),
                         fluidRow(plotOutput("corrplot")),
                         align = 'center'
                )
              )),
      tabItem("data",
              tabsetPanel(
                tabPanel("Head of Data",
                         tableOutput("tablehead")
                ),
                tabPanel("Select Data",
                         fluidRow(tags$b(h3('Select Features', align='center'))),
                         fluidRow(column(3,checkboxGroupInput("c1", "", colnames(mydata)[1:4])),
                         column(3,checkboxGroupInput("c2", "", colnames(mydata)[5:8])),
                         column(3,checkboxGroupInput("c3", "", colnames(mydata)[9:12])),
                         column(3,checkboxGroupInput("c4", "", colnames(mydata)[13:16]))),
                         textInput("selrows", "Select Rows (vector expression)", "1:10"),
                         tableOutput("tableselect")
                )
              )),
      tabItem("conc",
              fluidRow(h1("Summary and Conclusion"), align = 'center'),
              br(),
              h3("We analyzed the different features regarding lung cancer patients and made a couple of observations:"),
              br(),
              h3("1) Fatigue is observed in most lung cancer patients."),
              h3("2) Lung cancer is seen more in older people than young ones."),
              h3("3) The features tabulated are not highly correlated to each other."),
              h3("4) Lung cancer is seen slightly more in men than in women."),
              br(),
              h3("Lung cancer is among the leading causes of death among the different cancers and hence, analyzing different features of lung cancer patients, especially with the help of various graphs, helps us get an idea about the different causes and symptoms of the disease.")
              
        
      )
    )
  )
)

server <- function(input,output){
  
  rv = reactiveValues()
  
  output$piechart = renderPlot({

    var = input$catvar
    df2 = mydata
    gender_counts = c(0,0)
    for (i in t(df2[var])){
      if (i == 'F'){
        i = 1
      }
      else if(i == 'M'){
        i = 2
      }
      gender_counts[i] = gender_counts[i] + 1
    }
    if (var == 'GENDER'){
      genders = c('Female (F)', 'Male (M)')
      title = 'Percentage of Lung Cancer affected patients by GENDER'
    }
    else{
      genders = c('NO (1)', 'YES (2)')
      title = paste('Percentage of Lung Cancer affected patients associated with',var,sep = ' ')
    }
    gen = data.frame(genders, gender_counts)
    ggplot(gen, aes(x = "", y = gender_counts, fill = genders)) +
      geom_col() +
      geom_text(aes(label = round(gender_counts/sum(gender_counts)*100, 2)),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y")+ ggtitle(title) +
      theme_void() + theme(plot.title = element_text(hjust = 0.5, vjust = -3)) + guides(fill=guide_legend(title=var))
    })
  
  output$frequency = renderTable({
    var = input$catvar
    t = table(mydata[,var])
  }, colnames=FALSE)
  
  output$stats = renderTable({
    var = input$numvar
    s = summary(mydata[,var])
    df = data.frame(Min=s[[1]], Median=s[[3]], Mean=s[[4]], Max=s[[6]])
    df
  })
  
  output$histogram = renderPlot({
    plot2 = ggplot(ages, aes(pos_ages)) + geom_histogram(binwidth = 5, colour = 'orange', fill = 'red') + xlab('Age groups') + ylab('Count of lung cancer patients') + stat_bin(aes(y=..count.., label=..count..), geom="text",binwidth=5, vjust = -0.4) + ggtitle('Occurrence of lung cancer in different age groups (Total 270 patients)') + theme(plot.title = element_text(hjust = 0.5))
    plot2
  }, width = 800)
  
  
  output$barchart1 = renderPlot({
    varc1 = input$catvar1
    varc2 = input$catvar2
    for (i in 1:16){
      if (colnames(mydata)[i] == varc1){
        c1 = i
      }
      if(colnames(mydata)[i] == varc2){
        c2 = i
      }
    }
    symptoms = c(varc1, varc2)
    vals = vals[c(c1-2,c2-2)]
    plot1 = data.frame(symptoms, vals)
    
    p = ggplot(plot1, aes(x = symptoms, y = vals)) + geom_text(aes(label=round(vals,1)), color="black", vjust = -0.5) + geom_bar(stat = 'identity', width = 0.5, color = 'darkblue', fill = 'darkblue') + theme(axis.text.x=element_text(size = 12,hjust=1,vjust=1), plot.title = element_text(hjust = 0.5)) + xlab('Causes of lung cancer') + ylab('Percentage') + ggtitle('Lung cancer patients with different causes')
    print(p)
  }, width = 800)
  
  output$barchart2 = renderPlot({
    varc1 = input$catvar3
    varc2 = input$catvar4
    for (i in 1:16){
      if (colnames(mydata)[i] == varc1){
        c1 = i
      }
      if(colnames(mydata)[i] == varc2){
        c2 = i
      }
    }
    symptoms = c(varc1, varc2)
    vals = vals[c(c1-2,c2-2)]
    plot1 = data.frame(symptoms, vals)
    
    p = ggplot(plot1, aes(x = symptoms, y = vals)) + geom_text(aes(label=round(vals,1)), color="black", vjust = -0.5) + geom_bar(stat = 'identity', width = 0.5, color = 'darkgreen', fill = 'darkgreen') + theme(axis.text.x=element_text(size = 12,hjust=1,vjust=1), plot.title = element_text(hjust = 0.5)) + xlab('Symptoms of patients') + ylab('Percentage') + ggtitle('Lung cancer patients with different symptoms')
    print(p)
  }, width = 800)
  
  output$corrplot = renderPlot({
    df = mydata
    df1 = subset(df, LUNG_CANCER == 2)
    df2 = df1[1:15]
    temp = c()
    for (i in (1:length(t(df2[,1])))){
      if (df2[i,1] == 'M'){
        temp = append(temp, 1)
      }
      else{
        temp = append(temp, 2)
      }
    }
    df2[,1] = temp
    corr = cor(df2)
    ggcorrplot(corr) + theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(size=8, angle=45, vjust=0.98, hjust=1, margin=margin(-3,0,0,0)), axis.text.y=element_text(size=8, hjust = 1, margin=margin(0,-3,0,0)), panel.grid.major=element_blank()) + ggtitle('Correlation between different features of patients')
  }, height = 500, width = 600)
  
  output$tablehead = renderTable({
    mydata[1:10,]
  })
  
  output$tableselect = renderTable({
    vars = c(input$c1, input$c2, input$c3, input$c4)
    dat = cbind(data.frame(S.no=1:nrow(mydata)), mydata[,vars])
    rows = eval(parse(text=input$selrows))
    dat[rows,]
  })
  
}


## main()
shinyApp(ui,server)
