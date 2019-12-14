rm(list=ls())


#bibliotecas para execução do app
if(!require(shiny))install.packages("shiny");require(shiny)
if(!require(shinydashboard))install.packages("shinydashboard");require(shinydashboard)
if(!require(ggplot2))install.packages("ggplot2");require(ggplot2)
if(!require(plotly))install.packages("plotly");require(plotly)
if(!require(leaflet))install.packages("leaflet");require(leaflet)
if(!require(DT))install.packages("DT");require(DT)
#versões dos app
#R version 3.4.2 
#RStudio version 1.1.453

#versões dos pacotes
#leaflet versão 2.0.2
#plotly versão 4.8.0
#ggplot2 versão 3.1.0
#shinydashboard versão 0.7.1
#shiny versão 1.2.0 


#banco de dados para o app
load("D:/dados.RData")

setwd("D:/")
dados$ips_padronizada=((dados$`IPS - 2017`))

#- mean(dados$`IPS - 2017`))/sd(dados$`IPS - 2017`))
dados$taxa=dados$`TC - 2017`
dados$cor=0
dados[(dados$ips_padronizada>=mean(dados$`IPS - 2017`) & dados$taxa <=80),"cor"]=1
dados[(dados$ips_padronizada>=mean(dados$`IPS - 2017`) & dados$taxa >=80),"cor"]=2
dados[(dados$ips_padronizada<=mean(dados$`IPS - 2017`) & dados$taxa <=80),"cor"]=3
dados[(dados$ips_padronizada<=mean(dados$`IPS - 2017`) & dados$taxa >=80),"cor"]=4
dados$cor=as.factor(dados$cor)
data_red=data.frame(dados$log,dados$lat, PLACE=paste("Red_place_",seq(1,218)))
names(data_red)=c("LONG","LAT","PLACE")


#inicio do app
shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = "Tela de Diagnóstico!!", titleWidth=300),
    
    
    dashboardSidebar(
      sidebarMenu(menuItem("Tela de Diagnóstico",  icon = icon("accusoft"),    tabName = "PaineldeAvaliação"),
                  menuItem("Tabela",               icon = icon("table"),       tabName = "Tabela"),
                  menuItem("Mapa",                 icon = icon("map-marked-alt"),       tabName = "Mapa"),
                  menuItem("Informações",          icon = icon("info-circle"), tabName = "Informações"))),
    
    
    dashboardBody( 
      tabItems(
        tabItem(tabName = "PaineldeAvaliação",
                fluidRow(box(width = 12, title = "Tela de Diagnóstico",solidHeader = TRUE, status = "primary",plotlyOutput('plot1')))),
        tabItem(tabName = "Tabela",
                fluidRow(box(width = 12, title = "Raw Data", solidHeader = TRUE, status = "primary",DT::dataTableOutput("tabela")))),
        tabItem(tabName = "Mapa",
                fluidPage(br(),
                          leafletOutput("plot2", height="600px"),
                          br())),
        tabItem(tabName = "Informações",
                includeHTML("about.html")))
    )),
  server = function(input, output) {
    
    #criando legenda
    
    
    output$plot1 <-  renderPlotly({
      content1 <- paste0("Unidade:",dados$unidade,
                         "<br>Meta 1:",dados$`meta 1 - 2018`,"%",
                         "<br>Meta 2:",dados$`meta 2- 2018`,"%",
                         "<br>IPS:",round(dados$ips_padronizada,2),
                         "<br>Taxa de Congestinamento:",round(dados$taxa,2),"%")
      p=ggplot(dados, aes(taxa,ips_padronizada,col=cor,text=(content1)))+
        geom_hline(aes(yintercept=0))+                  #adiciona a linha horizontal
        geom_vline(aes(xintercept=0))+                  #adiciona a linha vertical
        geom_vline(aes(xintercept=80))+   
        geom_hline(aes(yintercept=mean(dados$`IPS - 2017`)))+#adiciona a linha vertical
        geom_point()+                                   #coloca os pontos
        theme_classic() +                               #tema do gráfico
        scale_fill_brewer(palette = "Accent")+          #paleta de cores
        xlab("Taxa de Congestionamento")+   #titulo do eixo x
        ylab("IPS")+
        theme(legend.position="none")         #retira nome do titulo da legenda
        #scale_x_continuous(breaks=c(-5:5))+            #valores que aparecem no eixo x
        #scale_y_continuous(breaks=c(-5:5))+            #valores que aparecem no eixo y
        #xlim(-5,5)+                                    #valores iniciais e finais do eixo x
        #ylim(-5,5)                                     #valores iniciais e finais do eixo y
        ggplotly(p,tooltip = c("text"))})
    
    
    output$plot2 <- renderLeaflet({
      
      #criando legenda
      content <- paste0("<strong>Unidade: </strong>",dados$unidade,"<br><strong>Meta 1 :  </strong>",round(dados$`meta 1 - 2018`,2),"<br><strong>Meta 2 :  </strong>",round(dados$`meta 2- 2018`,2))
      leaflet() %>% 
        setView(lng=-36.2922847, lat=-5.7999146, zoom=7 ) %>%
        addProviderTiles("Esri.WorldImagery", group="tema 2") %>%
        addProviderTiles("Stamen.Toner", group = "tema 3")%>%
        addTiles(options = providerTileOptions(noWrap = TRUE), group="tema 1")%>%
        addCircleMarkers(data=data_red, lng=~LONG , lat=~LAT,popup = content,  radius=8 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Unidades Judiciárias") %>%
        addLayersControl(overlayGroups = c("Unidades Judiciárias") , baseGroups = c("tema 1","tema 2","tema 3"), options = layersControlOptions(collapsed = FALSE))
    })
    
    output$tabela <- DT::renderDataTable({DT::datatable(dados[,c(2:6,9)],rownames = FALSE,extensions = 'Buttons',options = list(searchHighlight = TRUE,order = list(list(2, 'desc')),dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))}, server = FALSE)

  }
)
