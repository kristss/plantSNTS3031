## install.packages("shiny")
library(shiny)
library(rlang)
library(tidyverse)
library(DT)

windowsFonts(Lucida=windowsFont("Lucida Sans"))
par(bty = 'o', family = 'Lucida') # change the plot font

#standard data
P_dim = 8.0
COP_dim = 5.0
T_lim0 = c(-5,0,5)
C_SH_lim0 = c(0.78,1.00,1.10)
C_DHW_lim0 = c(0.43,0.58,0.73)
P_SH_lim0 = c(0.92,1.00,1.15)
P_DHW_lim0 = c(0.82,0.91,1.00)
T.1 = -5
T.2 = 0
T.3 = 5
COP_SH.1 = 0.78
COP_SH.2 = 1.00
COP_SH.3 = 1.10
COP_DHW.1 = 0.43
COP_DHW.2 = 0.58
COP_DHW.3 = 0.73
P_SH.1 = 0.92
P_SH.2 = 1.00 
P_SH.3 = 1.15
P_DHW.1 = 0.82
P_DHW.2 = 0.91
P_DHW.3 = 1.00
Ts_mean = 0
Ts_amp = 2
phi = 0
dhw <- c(0,0,0,0,0,0.96,6.87,13.74,6.87,0.96,0.96,0.96,0.96,0.96,0.96,0.96,0.96,13.74,13.74,1.37,1.37,1.37,0.96,0)
BRA = 176
# I.4 Simplified calculation of accumulator volume and heat loss
# I.13
Vol_heat = 0.4
# For housing and apartments with local distribution
a = 0.37
b = 0.19
dt=1
K = 0


# Define the UI
ui <- fluidPage(
    tags$style(HTML(
      "* {font-family: 'Lucida Sans Unicode', 'Lucida Grande', sans-serif !important;}"
    )),
  titlePanel("Modell for væske/vann varmepumpe etter SN/TS 3031"),
  fluidRow(column(
    4,
    tags$p("Kristian Skeie 2019-03"),
    tags$hr(),
    tags$i(style="color:grey",
      "Modellen for varmepumpesystemer er hentet fra tillegg K i SN/TS 3031:2016, der fem ulike varmepumper er beskrevet:
      væske/vann-, luft/vann-, luft/luft- og avtrekksvarmepumper samt avkastvarmepumper i kompaktaggregater."
    )
  ),
  column(8,
         tags$div(
           tags$p(
             "Modellen er basert på beregning av den største
              varmekapasiteten og varmefaktoren COP, ved ulike driftspunkter.
              Samtidig behov for varmtvann og oppvarming er også behandlet.
              Ved dellastsytelse beregnes varmefaktoren med en forenklet inverter- eller av/på styring."
           ),
           tags$p(
             "I dette eksempelet antas det at varmepumpen dekker grunnlast, at varmtvann gis prioritet,
             og at spisslast har tilstrekkelig effekt til å dekke det resterende varmebehovet, 
             dvs. at spisslast alltid er stor nok til at settpunkt i tank overholdes."
           ),
           tags$p(
             "Ytelsen til varmepumpen, avgitt varmeeffekt P og varmefaktor COP,
              beskreves ved ulike driftspunkter (dvs. ulike avgivelsestemperaturer
              og kildetemperaturer)."
             ),
           tags$p(
             "Veiledende verdier for væske/vann varmepumpe er vist under (fra tabell K.10). Det tas utgangspunkt i at avgitt effekt ved fullast er opgitt 
             ved middels kildetemperatur T.2 (her 0 grader) og laveste avgivelsestemperatur (her 35 grader). Driftsmodus er gitt ved 35 og 55 grader temperaturavgivelse.
             Det antas at oppvarmingsmodus skjer langs øvre kurve (her 35 grader ) og varmtvannsmodus langs nedre kurve (her 55 grader)."
           )
         ))),
  sidebarLayout(
    sidebarPanel(
      numericInput("COP_dim", "COP ved dimensjonerende T.2:", COP_dim, step = 0.1),
      fluidRow(
        column(4, numericInput("T.1", "T.1:", T.1)),
        column(4, numericInput("T.2", "T.2:", T.2)),
        column(4, numericInput("T.3", "T.3:", T.3))
      ),
      fluidRow(
        column(4, numericInput("COP_SH.1", "SH.1:", COP_SH.1, step = 0.1)),
        column(4, "SH.2 (1.0)"),
        column(4, numericInput("COP_SH.3", "SH.3:",  COP_SH.3, step = 0.1))
      ),
      fluidRow(
        column(4, numericInput("COP_DHW.1", "DHW.1:", COP_DHW.1, step = 0.1)),
        column(4, numericInput("COP_DHW.2", "DHW.2:",  COP_DHW.2, step = 0.1)),
        column(4, numericInput("COP_DHW.3", "DHW.3:",  COP_DHW.3, step = 0.1))
      )
    ),
    mainPanel(plotOutput('plot'),tags$p(
      "Valg av laveste kildetemperatur T.1 er viktig da denne modellen 
       vil sette avgitt temperatur til null under denne temperaturen."
    ))
  ),
  sidebarLayout(
    sidebarPanel(
    numericInput("P_dim", "P ved dimensjonerende T.2:", P_dim, step = 0.1),
    fluidRow(
      column(4, numericInput("P_SH.1", "SH.1:", P_SH.1, step = 0.1)),
      column(4, "SH.2 (1.0)"),
      column(4, numericInput("P_SH.3", "SH.3:",  P_SH.3, step = 0.1))
    ),
    fluidRow(
      column(4, numericInput("P_DHW.1", "DHW.1:", P_DHW.1, step = 0.1)),
      column(4, numericInput("P_DHW.2", "DHW.2:",  P_DHW.2, step = 0.1)),
      column(4, numericInput("P_DHW.3", "DHW.3:",  P_DHW.3, step = 0.1))
    )
  ),
  mainPanel(plotOutput('plot2'),tags$p(
    "Varmekildens temperaturvariasjon over året bør beregnes i egen programvare.
    Tabellverdiene i SN/TS 3031 er gitt for energibrønner, sjøtemperatur e.l.
    som funksjon av årsmiddel utetemperaturen. Det anbefales å bruke de dårligste verdiene i disse
    tabellene for å lage konservative anslag i tidligfase."))),
  sidebarLayout(sidebarPanel(
    numericInput("Ts_mean", "T snitt over året:", Ts_mean, step = 0.1),
    numericInput("Ts_amp", "T amplitude over året:", Ts_amp, step = 0.1),
    numericInput("phi", "Faseforksyvning:", phi, step = 0.1)
    ),
  mainPanel(plotOutput('plot3'),tags$p(
    "Lastprofiler til romoppvarming kan lages i SIMIEN, eksporteres som timesverdier.csv og importeres her. Senere vil jeg 
    bruke de nye ISO 52000 standardene til enkle energiberegninger. Varmtvannsprofiler lages med tabellverdier til venstre."))
),
sidebarLayout(sidebarPanel(
    numericInput("BRA", "Gulvareal til tappevannsberegning:", BRA, step = 0.1),
    selectInput("dhw_profile", label = "Velg tappevannsprofil:", 
                choices = list("Småhus (SN/TS 3031)" = 1), 
                selected = 1),
    hr(),
    # Input: Select a file ----
    fileInput("file1", "Last in timeverdier fra SIMIEN:",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
  ),
  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output: Data file ----
    #tableOutput("contents")
    DT::dataTableOutput("contents"),tags$p(
      "Applikasjonen kaller på et script skrevet i C++ (Rcpp lib i R), som beregner timesverdier med ytelse for varmepumpe og spisslast (her Q_EL altså elektrisitet).")
  )),
sidebarLayout(sidebarPanel(
  numericInput("a", "a param", a, step = 0.1),
  numericInput("b", "b param", b, step = 0.1),
  numericInput("Vol_heat", "Vol_heat per BRA [liter/m2]", Vol_heat, step = 0.1)
),
# Main panel for displaying outputs ----
mainPanel(
  
  # Output: Data file ----
  #tableOutput("contents")
  DT::dataTableOutput("results")
  
))
)

# Define the server code
server <- function(input, output) {
  output$plot <- renderPlot({
    plot(T_lim0[2],input$COP_dim,col="grey",ylim=c(0,8),xlim=c(-10,10),ylab="COP",xlab="Kildetemperatur [°C]",cex=3,family='Lucida')
    abline(v=input$T.2,h=COP_SH.2*input$COP_dim,lty=5,col="lightgrey")
    points(input$T.2,COP_SH.2*input$COP_dim,col="red",cex=3)
    
    lines(c(-15,T_lim0,35),c(0.78,C_SH_lim0,1.10)*input$COP_dim, col="grey")
    points(T_lim0,C_SH_lim0*input$COP_dim, pch=19, col="grey")

    lines(c(-15,input$T.1,input$T.2,input$T.3,35),c(input$COP_SH.1, input$COP_SH.1, COP_SH.2, input$COP_SH.3, input$COP_SH.3)*input$COP_dim)
    points(c(-15,input$T.1,input$T.2,input$T.3,35),c(input$COP_SH.1, input$COP_SH.1, COP_SH.2, input$COP_SH.3, input$COP_SH.3)*input$COP_dim, pch=19)
    
    lines(c(-15,T_lim0,35),c(0.43,C_DHW_lim0,0.73)*input$COP_dim, col="grey")
    points(c(-15,T_lim0,35),c(0.43,C_DHW_lim0,0.73)*input$COP_dim,pch=19, col="grey")

    lines(c(-15,input$T.1,input$T.2,input$T.3,35),c(input$COP_DHW.1, input$COP_DHW.1, input$COP_DHW.2, input$COP_DHW.3, input$COP_DHW.3)*input$COP_dim)
    points(c(-15,input$T.1,input$T.2,input$T.3,35),c(input$COP_DHW.1, input$COP_DHW.1, input$COP_DHW.2, input$COP_DHW.3, input$COP_DHW.3)*input$COP_dim, pch=19)
    
    title(main=paste("Varmefaktor COP:",input$COP_dim,"@",input$T.2,"°C"),line=-1)
  })
  output$plot2 <- renderPlot({
    plot(T_lim0[2],input$P_dim,col="grey",ylim=c(0,input$P_dim*1.4),xlim=c(-10,10),ylab="P effekt [kW]",xlab="Kildetemperatur [°C]",cex=3)
    abline(v=input$T.2,h=input$P_dim,lty=5,col="lightgrey")
    points(input$T.2,input$P_dim,col="red",cex=3)
    
    lines(c(T_lim0[1],T_lim0,35),c(0,P_SH_lim0,P_SH_lim0[3])*input$P_dim, col="grey")
    points(c(T_lim0),c(P_SH_lim0)*input$P_dim, pch=19, col="grey")
    
    lines(c(input$T.1,input$T.1,input$T.2,input$T.3,35),c(0, input$P_SH.1, P_SH.2, input$P_SH.3, input$P_SH.3)*input$P_dim)
    points(c(input$T.1,input$T.2,input$T.3),c(input$P_SH.1, P_SH.2, input$P_SH.3)*input$P_dim, pch=19)
    
    lines(c(T_lim0[1],T_lim0,35),c(0,P_DHW_lim0,0.73)*input$P_dim, col="grey")
    points(T_lim0,P_DHW_lim0*input$P_dim,pch=19, col="grey")
    
    lines(c(input$T.1,input$T.1,input$T.2,input$T.3,35),c(0, input$P_DHW.1, input$P_DHW.2, input$P_DHW.3, input$P_DHW.3)*input$P_dim)
    points(c(input$T.1,input$T.2,input$T.3),c(input$P_DHW.1, input$P_DHW.2, input$P_DHW.3)*input$P_dim, pch=19)
    
    title(main=paste("Avgitt effekt",input$P_dim,"kW @",input$T.2,"°C"),line=-1)
  })  
  output$plot3 <- renderPlot({
    plot(1:12,input$Ts_mean-input$Ts_amp*cos((2*pi)/12*(1:12-input$phi-1)),ylim=c(-10,10),ylab="Kildetemperatur [°C]",xlab="Måned",type="l")
    abline(v=1:12,h=T_lim0,lty=5,col="lightgrey")
    title(main=paste("Varmekildens temperatur (fjell, jord, vann, sjø)"),line=-1)
    })
  output$contents <- DT::renderDataTable({
    # renderTable({})
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read_delim(input$file1$datapath,"\t", escape_double = FALSE, locale = locale(decimal_mark = ",",encoding = "WINDOWS-1252"), trim_ws = TRUE,skip = 9)
        df$'Varmtvann [W]' <- dhw[df$Time+1]*input$BRA
             },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    #DT::datatable(df[, input$show_vars, drop = FALSE])
    DT::datatable(df, options = list(searching = FALSE))
    #df
    #df[c(1:5,(dim(timeverdier)[1]-4):dim(timeverdier)[1]),c(1:13,16,19)]
    
  })
  output$results <- DT::renderDataTable({
    req(input$file1)
    df2 <- rcpp_SN3031(T_e=df$'Utetemp. [°C]',L_DHW=df$'Varmtvann [W]',L_SH=df$'Romoppv.[W]',Month=df$Måned,
                       T_s=c(input$T.1,input$T.2,input$T.3),
                       P35=c(input$P_SH.1,input$P_SH.2,input$P_SH.3),
                       C35=c(input$COP_SH.1,input$COP_SH.2,input$COP_SH.3),
                       P55=c(input$P_DHW.1,input$P_DHW.2,input$P_DHW.3),
                       C55=c(input$COP_DHW.1,input$COP_DHW.2,input$COP_DHW.3),
                       P_HP_12=input$P_dim*1000, 
                       COP_HP_12=input$COP_dim, 
                       T_sh_set=35,
                       T_dhw_set=55, 
                       H_s= a + b * (Vol_heat*BRA)^0.4, 
                       K=exp(-(dt/(4180 * 988 * ((Vol_heat*BRA)/1000) / (3600 * (a + b * (Vol_heat*BRA)^0.4))))),
                       T_a=20,
                       dt=1,
                       Tavg_s=input$Ts_mean,
                       Tamp_s=input$Ts_amp,
                       phi=input$phi)
    DT::datatable(df2, options = list(searching = FALSE))
  
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)

df3 <- read_delim("timeverdier.txt","\t", escape_double = FALSE, locale = locale(decimal_mark = ",",encoding = "WINDOWS-1252"), trim_ws = TRUE,skip = 9)
df3$'Varmtvann [W]' <- dhw[df3$Time+1]*176


