library(plyr)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinythemes)
library(tools)
library(readxl)
library(haven)
library(shinycssloaders)
library(rmarkdown)
library(boot)
library(jtools)

# We want more variables in the dropdown so we create a list beforehand

l1 = c("Model 1","Model 4", "Model 6", "Model 8")

ui <- navbarPage(title = withTags(a(href = 'https://www.ashwinmalshe.com/', img(src="ccubes.png",height = 30, width = 108))),                 
        #shinythemes::themeSelector(),
        theme = shinytheme(theme = "yeti"),
        tabPanel("Mediation App",
          sidebarPanel(
            style = "overflow-y:scroll; max-height: 600px",
            helpText("Acceptable Formats: Excel (.xlsx, .xls), CSV (.csv), SPSS (.sav), Stata (.dta), or SAS (.sas7bdat)"),
            fileInput("mydata","Upload Data:"),
            helpText("Select PROCESS Model."),
            selectInput("modelno","PROCESS Model:", l1, selected = "Model 4", selectize = TRUE),
            uiOutput("xui"),
            uiOutput("yui"),
            conditionalPanel(
              condition = "input.modelno == 'Model 1'",
              uiOutput("modContui"),
              uiOutput("covaryui")
            ),
            conditionalPanel(
              condition = "input.modelno != 'Model 1'",
              uiOutput("med1ui"),
              uiOutput("covarm1ui")),
            conditionalPanel(
              condition = "input.modelno == 'Model 6'",
              uiOutput("med2ui"),
              uiOutput("covarm2ui")
            ),
            conditionalPanel(
              condition = "input.modelno == 'Model 8'",
              uiOutput("mod1ui")
            ),
            #uiOutput("covaryui"),
            helpText("If necessary, change the bootstrap parameters."),
            numericInput("rseed","Random Number Seed:", value = 123456),
            numericInput("bsample", "Number of Boostrap Samples:", value = 1000),
            numericInput("ci", "Confidence Level:", value = 0.95),
            uiOutput("actionui"),
            uiOutput("downloadui")
            ),
            mainPanel(
              tabsetPanel(id = "tabs",
                tabPanel("Setup",
                         tableOutput("t1"),
                         verbatimTextOutput("filename"),
                         verbatimTextOutput("rownum"),
                         verbatimTextOutput("modelno"),
                         verbatimTextOutput("xvar"),
                         verbatimTextOutput("yvar"),
                         verbatimTextOutput("med1"),
                         verbatimTextOutput("med2"),
                         verbatimTextOutput("mod1"),
                         verbatimTextOutput("rseed"),
                         verbatimTextOutput("bsample"),
                         verbatimTextOutput("ci"),
                         value = "Setup"
                ),
                tabPanel("Results",
                          #withSpinner(tableOutput("t2")),
                         withSpinner(tableOutput("t2")),
                         tableOutput("t21"),
                         tableOutput("t3"),
                         tableOutput("t4"),
                         textOutput("text1"),
                         tags$head(tags$style("#text1{color: red;
                                                font-size: 14px;
                                                font-style: italic;
                                                }"
                                              )
                                    ),
                         value = "results"
                         ),
                tabPanel("Plot",
                         plotOutput("p1"))
              )
            )
        ),
        tabPanel("Instructions",
                 fluidRow(
                   column(3),
                   column(6,
                          style = "font-size: 11pt",
                          includeMarkdown("Instructions.md")
                   )
                 )
        ),
        tabPanel("Example",
                 fluidRow(
                   column(3),
                   column(6,
                          style = "font-size: 11pt",
                          includeMarkdown("Example.md")
                   )
                 )
        ),
        tabPanel("About",
                 fluidRow(
                   column(3),
                   column(6,
                          style = "font-size: 11pt",
                          includeMarkdown("Mediate Shiny Dashboard.md")
                   )
                 )
        )
)

`%then%` <- shiny:::`%OR%`

server <- function(input, output, session){
  observeEvent(input$mediate,{
    updateTabsetPanel(session, "tabs", selected = "results")
    # session$sendCustomMessage(type = 'testmessage',
    #                           message = "Thanks")
  })
  dt = reactive({
    if(file_ext(input$mydata$name) %in% c("xls","xlsx")) {readxl::read_excel(input$mydata$datapath)}
    else{
      if(file_ext(input$mydata$name) == "csv") {read.csv(input$mydata$datapath, header = TRUE)}
      else{
        if(file_ext(input$mydata$name) == "dta") {read_dta(input$mydata$datapath)}
        else{
          if(file_ext(input$mydata$name) == "sas7bdat") {read_sas(input$mydata$datapath)}
          else{
            if(file_ext(input$mydata$name) == "sav") {read_sav(input$mydata$datapath)}
            else{NULL}
          }
        }
      }
    }
  })

  output$t1 = renderTable({
    if (is.null(input$mydata)){return(NULL)} else{head(dt())}
  }, caption=paste("First 6 observations"),
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL))       

  output$xui = renderUI({
    if(is.null(input$mydata)){NULL}
    else selectInput("xvar", "X Variable:", c("",colnames(dt())))
  })

  output$yui = renderUI({
    if(is.null(input$xvar)){NULL}
    else if(input$xvar == ""){NULL}
    else selectInput("yvar", "Y Variable:",
                     c("",colnames(dt())[!colnames(dt()) %in% c(input$xvar)]), selectize = TRUE)
  })
  
  output$med1ui = renderUI({
    if(is.null(input$yvar)){NULL} 
    else if(input$yvar == ""){NULL}
    else{
      selectInput("med1", "Mediator 1:",
                  c("",colnames(dt())[!colnames(dt()) %in% c(input$xvar, input$yvar)]), selectize = TRUE)
    }
  })
  output$covarm1ui = renderUI({
    if(is.null(input$med1)){NULL} 
    else if(input$med1 == ""){NULL}
    else selectInput("covarm1", "Optional Covariates for Mediator 1",
                     c(Choose = '',colnames(dt())[!colnames(dt()) %in% c(input$xvar,input$yvar,input$med1)]),
                     selectize = TRUE, multiple = TRUE)
  }) 
  
  output$med2ui = renderUI({
    if(is.null(input$med1)){NULL}
    else if(input$med1 == ""){NULL}
    else{
      selectInput("med2", "2nd Mediator:",
                  c("",colnames(dt())[!colnames(dt()) %in% c(input$xvar, input$yvar, input$med1, input$covarm1)]))
    }
  })
  
  output$covarm2ui = renderUI({
    if(is.null(input$med2)){NULL} 
    else if(input$med2 == ""){NULL}
    else selectInput("covarm2", "Optional Covariates for Mediator 2",
                     c(Choose = '',colnames(dt())[!colnames(dt()) %in% c(input$xvar,input$yvar,input$med1,input$med2, input$covarm1)]),
                     selectize = TRUE, multiple = TRUE)
  }) 
  output$mod1ui = renderUI({
    if(is.null(input$med1)){NULL}
    else if(input$med1 == ""){NULL}
    else{list(
      helpText("Type the moderating variable name. It must be a binary variable (i.e., with 2 levels only)"),
      selectInput("mod1", "Moderator:",
                  c("",colnames(dt())[!colnames(dt()) %in% c(input$xvar, input$yvar, input$med1, input$covarm1)]))
    )}
  })

  output$modContui = renderUI({
    if(is.null(input$yvar)){NULL}
    else if(input$yvar == ""){NULL}
    else{list(
      helpText("Select moderating variable. It must be a CONTINUOUS variable"),
      selectInput("modCont", "Continuous Moderator:",
                  c("",colnames(dt())[!colnames(dt()) %in% c(input$xvar, input$yvar)]))
    )}
  })  
  output$covaryui = renderUI({
    if(is.null(input$modCont)){NULL} 
    else if(input$modCont == ""){NULL}
    else selectInput("covary", "Optional Covariates for Y variable",
                     c(Choose = '',colnames(dt())[!colnames(dt()) %in% c(input$xvar,input$yvar,input$modCont)]),
                     selectize = TRUE, multiple = TRUE)
  })   
  output$actionui <- renderUI({
    if (is.null(input$yvar)) return()
    else{
      if(input$modelno == "Model 1"){
        list(helpText("Press the button to run the analysis."),
             actionButton("mediate","Run Moderation"))
      }
      else{
      list(helpText("Press the button to run the analysis."),
      actionButton("mediate","Run Mediation"))
      }
    }
  })
  
  output$downloadui <- renderUI({
    if (is.null(input$yvar)) return()
    else{
      list(helpText("Download report in PDF or Word format."),
      radioButtons('format', 'Report format', c('PDF', 'Word'),inline = TRUE),
      downloadButton(outputId = "report",label = "Download Report"))
    }
  })
    
        filename = reactive(paste("File Name:", input$mydata$name))
        output$filename = renderText(filename())
        modelno = reactive(paste("PROCESS Model:", input$modelno))
        output$modelno = renderText(modelno())
        xvar = reactive(paste("X Variable:", input$xvar))
        output$xvar = renderText(xvar())
        yvar = reactive(paste("Y variable:", input$yvar))
        output$yvar = renderText(yvar())
        med1 = reactive(paste("Mediator:", input$med1))
        output$med1 = renderText(med1())
        med2 = reactive({if(input$modelno == "Model 6"){paste("2nd Mediator:", input$med2)} else{NULL}})
        output$med2 = renderText(med2())
        mod1 = reactive({if(input$modelno == "Model 8"){paste("Moderator:", input$mod1)} else{NULL}})
        output$mod1 = renderText(mod1())
        rseed = reactive(paste("Random Number Generator Seed:", input$rseed))
        output$rseed = renderText(rseed())
        bsample = reactive(paste("Number of Bootstrap Samples:", input$bsample))
        output$bsample = renderText(bsample())
        ci = reactive(paste("Confidence Level:", input$ci))
        output$ci = renderText(ci())

#        bootAlert = eventReactive(input$bsample, {if(input$bsample > 2000) return(1)})
 
        observeEvent(input$bsample, {
          if (input$bsample > 2000) showModal(
            modalDialog(title = "A Lot of Bootstrap Samples!",
                        "You have more than 2,000 samples. This may take a while so please be patient.",
                        easyClose = TRUE))
        })               
        bs4 = function(data, i){
          d = data[i,]
          if(length(input$covarm1) > 0){
          c1 = lm(as.formula(paste(input$med1, "~", input$xvar, "+", paste(input$covarm1, collapse = "+"))),
                  data = d)$coefficients
          c2 = lm(as.formula(paste(input$yvar,"~", input$med1,"+", input$xvar, "+",paste(input$covarm1, collapse = "+"))),
                  data = d)$coefficients
          }
          else{
            c1 = lm(as.formula(paste(input$med1, "~", input$xvar)),
                    data = d)$coefficients
            c2 = lm(as.formula(paste(input$yvar,"~", input$med1,"+", input$xvar)),
                    data = d)$coefficients
          }
          c3 = c1[2]*c2[2]; names(c3) = "Indirect Effect"
          c4 = c3 + c2[3]; names(c4) = "Total Effect"
          return(c(c1,c2,c3,c4))
        }
        
        
        bs6 = function(data, i) {
          d = data[i,]
          if(length(input$covarm1) > 0){
            c1 = lm(as.formula(paste(input$med1, "~", input$xvar, "+", paste(input$covarm1, collapse = "+"))),
                    data = d)$coefficients
            if(length(input$covarm2 > 0)){
              c11 = lm(as.formula(paste(input$med2, "~",input$med1, "+", input$xvar, "+",
                      paste(input$covarm1, collapse = "+"),"+" ,paste(input$covarm2, collapse = "+"))), data = d)$coefficients
              c2 = lm(as.formula(paste(input$yvar, "~", input$med1,"+" ,input$med2,"+", input$xvar, "+",
                  paste(input$covarm1, collapse = "+"),"+" , paste(input$covarm2, collapse = "+"))), data = d)$coefficients
            }
            else{
              c11 = lm(as.formula(paste(input$med2, "~",input$med1, "+", input$xvar, "+", paste(input$covarm1, collapse = "+"))),
                       data = d)$coefficients
              c2 = lm(as.formula(paste(input$yvar, "~", input$med1,"+" ,input$med2,"+", input$xvar, "+", paste(input$covarm1, collapse = "+"))),
                      data = d)$coefficients
            }
          }
          else{
            c1 = lm(as.formula(paste(input$med1, "~", input$xvar)), data = d)$coefficients
            c11 = lm(as.formula(paste(input$med2, "~", input$med1,"+" ,input$xvar)), data = d)$coefficients
            c2 = lm(as.formula(paste(input$yvar, "~", input$med1,"+" ,input$med2,"+",input$xvar)), data = d)$coefficients
          }
          
          c3 = c1[2]*c2[2]; names(c3) = "Indirect Effect 1"
          c31 = c1[2]*c11[2]*c2[3]; names(c31) = "Indirect Effect 2"
          c4 = c3 + c31 + c2[4]; names(c4) = "Total Effect"
          return(c(c1,c11,c2,c3,c31,c4))
        }
        
        
        bs8 = function(data, i) {
          d = data[i,]
          d[, paste0(input$xvar,"X",input$mod1)] = d[,input$xvar]*d[,input$mod1]
          if (length(input$covarm1) >0){
            c1 = lm(as.formula(paste0(input$med1, " ~ ", input$xvar," + ",input$mod1, " + ",tail(colnames(d),1),
                                     "+", paste(input$covarm1, collapse = "+"))), data = d)$coefficients
            c2 = lm(as.formula(paste0(input$yvar, " ~ ",input$med1," + ", input$xvar," + ",input$mod1," + ",tail(colnames(d),1),
                                     "+", paste(input$covarm1, collapse = "+"))), data = d)$coefficients
          }
          else{
            c1 = lm(as.formula(paste0(input$med1, " ~ ", input$xvar," + ",input$mod1, " + ",input$xvar,":",input$mod1)), data = d)$coefficients
            c2 = lm(as.formula(paste0(input$yvar, " ~ ",input$med1," + ", input$xvar," + ",input$mod1," + ",input$xvar,":",input$mod1)), data = d)$coefficients
          }
          c3 = c1[2]*c2[2]; names(c3) = "Indirect Effect: Moderator = 0"
          c31 = (c1[2]+c1[4])*c2[2]; names(c31) = "Indirect Effect: Moderator = 1"
          c4 = c3 + c2[3]; names(c4) = "Total Effect: Moderator = 0"
          c41 = c31 + c2[3] + c2[5]; names(c41) = "Total Effect: Moderator = 1"
          return(c(c1,c2,c3,c31,c4,c41))
        }
        

t2 = eventReactive(input$mediate,{
                  validate(
                    need(!is.null(input$mydata), "Please upload a data set") %then%
                    need(input$xvar != "", "Please select a X variable") %then%
                    need(input$yvar != "", "Please select a Y variable") %then%
                    need(input$med1 != "", "Please select a Mediating Variable")
                    )
      dt1 = if(file_ext(input$mydata$name) %in% c("xls","xlsx")) {read_excel(input$mydata$datapath)}
            else if(file_ext(input$mydata$name) == "csv") {read.csv(input$mydata$datapath, header = TRUE)}
            else if(file_ext(input$mydata$name) == "dta") {read_dta(input$mydata$datapath)}
            else if(file_ext(input$mydata$name) == "sas7bdat") {read_sas(input$mydata$datapath)}
            else if(file_ext(input$mydata$name) == "sav") {read_sav(input$mydata$datapath)}
            else {NULL}
              
  
            if(input$modelno == "Model 4"){
                  dt2 = dt1[,c(input$xvar, input$yvar, input$med1, input$covarm1)]
                  set.seed(input$rseed)
                  bs1 = boot::boot(data = dt2,
                                   statistic = bs4,
                                    R = input$bsample)
                  bootse = apply(bs1$t,2,sd)
                  est = data.frame(Coefficient = names(bs1$t0),
                                   Estimate = bs1$t0,
                                   Boot_SE = bootse,
                                   t_stat = bs1$t0/bootse)
                  bca = matrix(NA, nrow = nrow(est), ncol = 2)
                  
                  for (j in 1:nrow(est)) {
                    bca[j,] = boot::boot.ci(bs1, conf = input$ci, type = "bca", index = j)$bca[,4:5]
                  }
                  
                  est = cbind(est,as.data.frame(bca))
                  CI = 100*input$ci
                  names(est)[5:6] = c(paste0("Lower ",CI,"% ","CI"), paste0("Upper ",CI,"% ","CI"))
                  rownames(est) = NULL
                  
                  cm1 = length(input$covarm1)
                  
                  est1 = est[c(1:(2 + cm1)),]
                  est1$p_value = 2*(1-pt(abs(est1$t_stat),nrow(dt2)-(2+cm1)))
                  est1 = cbind(Parameter = c("","a1",rep("",cm1)), est1[c(1:4,7,5,6)])
                  # Table for Regression 2
                  est2 = est[c((3+cm1):(5+2*cm1)),]
                  est2$p_value = 2*(1-pt(abs(est2$t_stat), nrow(dt2) - (3+cm1)))
                  est2 = cbind(Parameter = c("","b1","c",rep("",cm1)), est2[c(1:4,7,5,6)])
                  # Table for Mediation
                  est3 = tail(est,2)
                  colnames(est3)[1] = "Effect"
                  est3 = cbind(Parameter = c("a1*b1","c+a1*b1"), est3)
                  
                  est1 = est1 %>% mutate_if(is.numeric, round, digits=3) 
                  est2 = est2 %>% mutate_if(is.numeric, round, digits=3)
                  est3 = est3 %>% mutate_if(is.numeric, round, digits=3) 
                  
                  est4 = est3
                  
                  
                  ##### Graph
                  d = data.frame(x1 = c(0,2,4), x2 = c(1,3,5), 
                                 y1 = c(2,4,2), y2 = c(3,5,3),
                                 label = c(input$xvar,input$med1,input$yvar))
                  
                  # This data set makes arows
                  d1 = data.frame(x1 = c(0.5,1,3), x2 = c(2,4,4.5),
                                  y1 = c(3,2.5,4.5), y2 = c(4.5,2.5,3),
                                  xadj = c(0,0,0), yadj = c(0.2,-0.5,0.3),
                                  arrlabel = c(
                                    paste("a1 = ",est1[2,3]),
                                    paste("c = ", est2[3,3], "\n",
                                          "Ind. effect (a1*b1) = ",est3[1,3], "\n",
                                          "Tot. effect (c+a1*b1) = ",est3[2,3]),
                                    paste("b1 = ", est2[2,3])),
                                  angle = c(45,0,-45))
                  
                  g1 = ggplot(data = d) + 
                    geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
                              color="black", alpha=0.5, size = 0.2, fill = "white") +
                    geom_text(aes(x=(x1+x2)/2, y = (y1+y2)/2, label=label), size=4) +
                    geom_text(aes(x = (x1+x2)/2 - xadj, y = (y1+y2)/2 + yadj,
                                  label = arrlabel, angle = angle),
                              data = d1,family = 'serif', fontface = 'italic') +
                    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = d1,
                                 size = 0.2,
                                 arrow = arrow(angle = 15, ends = "last", type = "closed",
                                               length = unit(0.15, "inches"))) +
                    theme(axis.title = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          panel.background = element_blank()) +
                    scale_y_continuous(limits = c(0,6)) + coord_fixed(ratio = 1)
                  
                  
                
                  return(list(est1 = est1, est2 = est2, est3 = est3, est4 = est4, g1 = g1))
            }
      
              else if(input$modelno == "Model 6"){
                  dt2 = dt1[,c(input$xvar, input$yvar, input$med1, input$med2, input$covarm1, input$covarm2)]
                  set.seed(input$rseed)
                  bs1 = boot::boot(data = dt2,
                                   statistic = bs6,
                                   R = input$bsample)
                  bootse = apply(bs1$t,2,sd)
                  cm1 = length(input$covarm1)
                  cm2 = length(input$covarm2)
                  
                  est = data.frame(Coefficient = names(bs1$t0),
                                   Parameter = c("","a1",rep("",cm1),
                                                 "","d21","a2",rep("",cm1+cm2),
                                                 "","b1","b2","c",rep("",cm1+cm2),
                                                 "a1*b1","a1*d21*b2", "c+(a1*b1)+(a1*d21*b2)"),
                                   Estimate = bs1$t0,
                                   `Boot SE` = bootse,
                                  t_stat  = bs1$t0/bootse)
                  bca = matrix(NA, nrow = nrow(est), ncol = 2)
                  
                  for (j in 1:nrow(est)) {
                    bca[j,] = boot::boot.ci(bs1, conf = input$ci, type = "bca", index = j)$bca[,4:5]
                  }
                  
                  est = cbind(est,as.data.frame(bca))
                  CI = 100*input$ci
                  names(est)[6:7] = c(paste0("Lower ",CI,"% ","CI"), paste0("Upper ",CI,"% ","CI"))
                  rownames(est) = NULL

                  est1 = est[c(1:(2+cm1)),]
                  est1$p_value = 2*(1-pt(abs(est1$t_stat),nrow(dt2)-2-cm1))
                  est1 = est1[c(1:5,8,6,7)]
                  
                  est11 = est[c((3+cm1):(5+2*cm1+cm2)),]
                  est11$p_value = 2*(1-pt(abs(est11$t_stat),nrow(dt2)-3-cm1-cm2))
                  est11 = est11[c(1:5,8,6,7)]
                  
                  est2 = est[c((6+2*cm1+cm2):(9+3*cm1+2*cm2)),]
                  est2$p_value = 2*(1-pt(abs(est2$t_stat),nrow(dt2)-4-cm1-cm2))
                  est2 = est2[c(1:5,8,6,7)]
                  
                  est3 = tail(est,3)
                  colnames(est3)[1] = "Effect"
                  est1 = est1 %>% mutate_if(is.numeric, round, digits=3)
                  est11 = est11 %>% mutate_if(is.numeric, round, digits=3)
                  est2 = est2 %>% mutate_if(is.numeric, round, digits=3)
                  est3 = est3 %>% mutate_if(is.numeric, round, digits=3) 
                  
                  est4 = est3
                  est4$Parameter = as.character(est3$Parameter)
                  est4[2,2] = "a1\\*d21\\*b2" 
                  est4[3,2] = "c + (a1*b1) + (a1\\*d21\\*b2)"
                  
                  ##### Graph
                  d = data.frame(x1 = c(0,2,4.5,6.5), x2 = c(1,3,5.5,7.5), 
                                 y1 = c(2,4,4,2), y2 = c(3,5,5,3),
                                 label = c(input$xvar,input$med1,input$med2,input$yvar))
                  
                  # This data set makes arows
                  
                  d1 = data.frame(x1 = c(.5,1,1,3,3,5.5), x2 = c(2,4.5,6.5,4.5,6.5,7),
                                  y1 = c(3,2.75,2.5,4.5,4.25,4.5), y2 = c(4.5,4.25,2.5,4.5,2.75,3),
                                  xadj = c(0.5,1.1,2.5,3.1,4,5.6), yadj = c(3.2,3,2,4.7,3.5,4.6),
                                  arrlabel = c(
                                    paste("a1 = ",est1[2,3]),
                                    paste("a2 = ", est11[3,3]),
                                    paste("c = ", est2[4,3], "\n",
                                          "Ind. effect 1 (a1*b1) = ",est3[1,3], "\n",
                                          "Ind. effect 2 (a1*d21*b2) = ",est3[2,3], "\n",
                                          "Tot. effect (c+(a1*b1)+(a1*d21*b2)) = ",est3[3,3]),
                                    paste("d21 = ", est11[2,3]),
                                    paste("b1 = ", est2[2,3]),
                                    paste("b2 = ", est2[3,3])),
                                  angle = c(45,18.4,0,0,-18.4,-45))
                  
                  g1 = ggplot(data = d) + 
                    geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
                              color="black", alpha=0.5, size = 0.2, fill = "white") +
                    geom_text(aes(x=(x1+x2)/2, y = (y1+y2)/2, label=label), size=4) +
                    # geom_text(aes(x = (x1+x2)/2 - xadj, y = (y1+y2)/2 + yadj,
                    geom_text(aes(x = xadj, y = yadj,                
                                  label = arrlabel, angle = angle), hjust =0,data = d1, family = 'serif', fontface = 'italic') +
                    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = d1,
                                 size = 0.2,
                                 arrow = arrow(angle = 15, ends = "last", type = "closed",
                                               length = unit(0.15, "inches"))) +
                    theme(axis.title = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          panel.background = element_blank()) +
                    scale_y_continuous(limits = c(0,6)) + coord_fixed(ratio = 1)

              return(list(est1 = est1, est11 = est11, est2 = est2, est3 = est3, est4 = est4, g1 = g1))
              } 
      
              else if(input$modelno == "Model 8"){
                  dt2 = dt1[,c(input$xvar, input$yvar, input$med1, input$mod1, input$covarm1)]
                  modlevel = plyr::count(dt2[,input$mod1])[1]
                  validate(
                    need(nrow(modlevel) == 2, "Moderator must be a binary variable with only two levels")
                  )
                  change1 = 0; change2 = 0
                  if(nrow(modlevel) == 2){
                    if(modlevel[1,1] != 0) {
                      dt2[,input$mod1][dt2[,input$mod1] == modlevel[1,1]] = 0
                      change1 = 1
                    }
                    if(modlevel[2,1] != 1) {
                      dt2[,input$mod1][dt2[,input$mod1] == modlevel[2,1]] = 1
                      change2 = 1
                    }
                  }
                  
                  
                  set.seed(input$rseed)
                  bs1 = boot::boot(data = dt2, statistic = bs8, R = input$bsample)
                  
                  bootse = apply(bs1$t,2,sd)
                  
                  cm1 = length(input$covarm1)
                  est = data.frame(Coefficient = names(bs1$t0),
                                   Parameter = c("","a1","a2","a3",rep("",cm1),
                                                 "","b1","c1","c2","c3",rep("",cm1),
                                                 "a1*b1","(a1+a3)*b1", "c1+(a1*b1)", "c1+c3+(a1+a3)*b1"),
                                   Estimate = bs1$t0,
                                   Boot_SE = bootse,
                                   t_stat  = bs1$t0/bootse)
                  bca = matrix(NA, nrow = nrow(est), ncol = 2)
                  
                  for (j in 1:nrow(est)) {
                    bca[j,] = boot::boot.ci(bs1, conf = input$ci, type = "bca", index = j)$bca[,4:5]
                  }
                  
                  est = cbind(est,as.data.frame(bca))
                  CI = 100*input$ci
                  names(est)[6:7] = c(paste0("Lower ",CI,"% ","CI"), paste0("Upper ",CI,"% ","CI"))
                  rownames(est) = NULL
                  
                  est1 = est[c(1:(4+cm1)),]
                  est1$p_value = 2*(1-pt(abs(est1$t_stat),nrow(dt2)-4-cm1))
                  est1 = est1[c(1:5,8,6,7)]
                  est1$Coefficient = as.character(est1$Coefficient)
                  est1[4,1] = paste0(input$xvar,":",input$mod1)
                  
                  
                  est2 = est[c((5+cm1):(9+2*cm1)),]
                  est2$p_value = 2*(1-pt(abs(est2$t_stat),nrow(dt2)-5-cm1))
                  est2 = est2[c(1:5,8,6,7)]
                  est2$Coefficient = as.character(est2$Coefficient)
                  est2[5,1] = paste0(input$xvar,":",input$mod1)
                                    
                  est3 = tail(est,4)
                  colnames(est3)[1] = "Effect"
                  
                  est1 = est1 %>% mutate_if(is.numeric, round, digits=3) 
                  est2 = est2 %>% mutate_if(is.numeric, round, digits=3)
                  est3 = est3 %>% mutate_if(is.numeric, round, digits=3) 
                  
                  est4 = est3
                  d = data.frame(x1 = c(1,0,3.5,6), x2 = c(2,1,4.5,7), 
                                 y1 = c(2,4,4,2), y2 = c(3,5,5,3),
                                 label = c(input$xvar,input$mod1,input$med1,input$yvar))
                  
                  d1 = data.frame(x1 = c(1.5,2,1,1,4.5), x2 = c(3.5,6,2.5,3.5,6.5),
                                  y1 = c(3,2.5,4.25,4.75,4.5), y2 = c(4.5,2.5,3.75,4.75,3),
                                  arrlabel = c(
                                    paste("a1 = ",est1[2,3]),
                                    paste("c1 = ", est2[3,3], "\n",
                                          "Ind Effect @ 0 (a1*b1)",est3[1,3], "\n",
                                          "Ind Effect @ 1 ((a1+a3)*b1) = ",est3[2,3], "\n",
                                          "Tot Effect @ 0 (c1+(a1*b1)) = ",est3[3,3],"\n",
                                          "Tot Effect @ 1 (c1+c3+(a1+a3)*b1) = ",est3[4,3]),
                                    paste("a3 = ",est1[4,3]),
                                    paste("a2 = ",est1[3,3]),
                                    paste("b1 = ", est2[2,3])),
                                  angle = c(40,0,-20,0,-35),
                                  xadj = c(2.2,2.5,1.2,1.2,4.7),
                                  yadj = c(3.1,1.75,4.4,4.9,4.5))
                  
                  g1 = ggplot(data = d) + 
                    geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
                              color="black", alpha=0.5, size = 0.2, fill = "white") +
                    geom_text(aes(x=(x1+x2)/2, y = (y1+y2)/2, label=label), size=4) +
                    geom_text(aes(x = xadj, y = yadj, label = arrlabel, angle = angle),
                              hjust =0,data = d1, family = 'serif', fontface = 'italic') +
                    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = d1,
                                 size = 0.2,
                                 arrow = arrow(angle = 15, ends = "last", type = "closed",
                                               length = unit(0.15, "inches"))) +
                    theme(axis.title = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          panel.background = element_blank()) +
                    scale_y_continuous(limits = c(0,6)) +
                    scale_x_continuous(limits = c(0,10)) + coord_fixed(ratio = 1)
                  
                  
                  
                  
                  return(list(est1 = est1, est2 = est2, est3 = est3, est4 = est4, g1 = g1,
                              modlevel = modlevel, change1 = change1, change2 = change2))
                }
  })

        output$p1 = renderPlot(t2()$g1)
        output$t2 = renderTable({
          if (is.null(t2()$est1)){return(NULL)} else{t2()$est1}
        }, caption=paste("Mediator #1 Equation"),
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL), digits = 3)

        output$t21 = renderTable({
          if (is.null(t2()[["est11"]])){return(NULL)} else{t2()[["est11"]]}
        }, caption=paste("Mediator #2 Equation"),
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL), digits = 3)


        output$t3 = renderTable({
          if (is.null(t2()[["est2"]])){return(NULL)} else{t2()[["est2"]]}
        }, caption=paste("Dependent Variable Equation"),
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL), digits = 3)

        output$t4 = renderTable({
          if (is.null(t2()[["est3"]])){return(NULL)} else{t2()[["est3"]]}
        }, caption=paste("Indirect and Total Effects"),
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL), digits = 3)

        text1 = reactive({
          if(is.null(t2()[["change1"]])) return(NULL)
          else if(t2()[["change1"]] == 0 & t2()[["change2"]] == 0) return(NULL)
          else if(t2()[["change1"]] == 1 & t2()[["change2"]] == 1) {paste(
            "The levels of moderator have been recoded from",
                                      t2()[["modlevel"]][1,1], "to 0 and from",
                                      t2()[["modlevel"]][2,1], "to 1")}
          else if(t2()[["change1"]] == 1 & t2()[["change2"]] == 0){paste(
            "The level of moderator has been recoded from",
            t2()[["modlevel"]][1,1], "to 0.")}
          else if(t2()[["change1"]] == 0 & t2()[["change2"]] == 1) {paste(
            "The level of moderator has been recoded from",
            t2()[["modlevel"]][2,1], "to 1.")}
        })
        output$text1 = renderText(text1())
         
        ##############################################################
        ## Output a word document
        
        output$report <-  downloadHandler(
          ## Making the filename for the report here. Using two different
          ## extensions for the file
          filename = function(){
              paste0(input$mydata$name,"_",input$modelno,"_",Sys.Date(),".",
                     switch(input$format, PDF = 'pdf', Word = 'docx'))
          },
          
          content = function(file) {
            ## Copy the report file to a temporary directory before processing it, in
            ## case we don't have write permissions to the current working dir (which
            ## can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(
              date = Sys.Date(),
              filename = filename(),
              modelno = modelno(),
              xvar = xvar(),
              yvar = yvar(),
              med1 = med1(),
              med2 = med2(),
              mod1 = mod1(),
              rseed = rseed(),
              bsample = bsample(),
              ci = ci(),
              table1 = t2()$est1,
              table2 = t2()$est11,
              table3 = t2()$est2,
              table4 = t2()$est4,
              text1 = text1(),
              plot1 = t2()$g1
            )
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            library(rmarkdown)
            rmarkdown::render(tempReport,
                              switch(input$format,
                                     PDF = pdf_document(), Word = word_document()),
                              output_file = file, params = params,
                              envir = new.env(parent = globalenv())
            )
          }
        )
        
}

shinyApp(ui = ui, server = server)
