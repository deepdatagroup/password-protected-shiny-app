library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(shinyjs)
library(shinyauthr)
library(tidytext)
library(tidyverse)
library(xml2)
library(rtweet)
library(tm)
library(Hmisc)
library(tidyr)
library(caret)
library(dplyr)
library(RCurl)
library(plotly)
library(readxl)
library(reshape2)
library(glue)
library(stringr)
library(rvest)
library(wordcloud2)
library(DT)
library(flexdashboard)


###thme for ggplot graphs
mytheme <-  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.y=element_blank(),
    axis.text.y=element_text(size=10),
    #text=element_text(size=) , 
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
)


mytheme2 <-  theme(
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.y=element_blank(),
    axis.text.y=element_text(size=10),
    #text=element_text(size=) , 
    #axis.ticks.y=element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    
    panel.grid.major = element_blank(),
    
)






###########PáGINA SUPERSALUD
pag2=xml2::read_html("http://www.supersalud.gov.co/es-co/delegadas/supervision-institucional/direccion-delegada-supervision-institucional/estadisticas-de-las-eapb/indicadores-de-afiliaciones")
links=pag2 %>% html_nodes("a") %>% html_attr("href") 
links=links[which(links %>% str_detect(".xls")==TRUE)]

temp.file <- paste(tempfile(),".xlsx",sep = "")

tryCatch({
    download.file(first(links), temp.file, mode = "wb",method = "wget")
}, warning = function(w) {
    download.file(first(links), temp.file, mode = "wb")
}, error = function(e) {
    download.file(first(links), temp.file, mode = "wb")
})

############################################################FUNCIONES IMPORTANTES##################


createLink <- function(val){
    return(sprintf('<a href="%s" ">Link</a>',val))
}

##################################################################################################


user_base <- data_frame(
    user = c("jhon","user"), ##add more users here
    password = c("pass","pass"), 
    password_hash = sapply(c("pass","pass"), sodium::password_store), 
    permissions = c("admin", "admin"),
    name = c("Jhon Parra", "Invitado")
)



#logoini=tags$a(href="http://coomeva.com.co/",tags$img(src="logocoom.png", height="50" ,width="170"))


ui <- dashboardPage(skin="black",
                    
                    dashboardHeader(title = "MARKET SHARE MEDICINA PREPAGADA",titleWidth = 190,
                                    tags$li(class = "dropdown", style = "padding: 0px;",
                                            tags$a(icon("file-code"), 
                                                   href = "https://coomeva.com.co/",
                                                   title = "TEST")),
                                    tags$li(class = "dropdown", style = "padding: 0px;",
                                            shinyauthr::logoutUI("logout")),
                                    tags$li(class = "dropdown", 
                                            tags$a(icon("github"), 
                                                   href = "https://github.com/paulc91/shinyauthr",
                                                   title = "Login powered by Paulc91"))
                    ),
                    
                    dashboardSidebar(collapsed = FALSE, 
                                     div(textOutput("welcome"), style = "padding: 20px"),
                                     div(sidebarMenu(id = "barra",
                                                     
                                                     
                                                     
                                                     #div(style="display:inline-block;text-align: center; align: center",actionButton(inputId = "gobutton",label = "Ver /Actualizar Noticias ",icon = icon('refresh')))
                                                     
                                                     menuItem("Medicina Prepagada",icon=icon("stethoscope"),tabName = "medprep")
                                                     
            
                                                     
                                                     
                                                     
                                     )
                                     
                                     
                                     
                                     
                                     
                                     )),
                    
                    
                    dashboardBody(
                        shinyjs::useShinyjs(),
                        tags$head(tags$style(".table{margin: 0 auto;}"),
                                  tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                                              type="text/javascript"),
                                  includeScript("returnClick.js")
                        ),
                        
                        tags$style(HTML("


                .box.box-solid.box-primary>.box-header {
                background:Green;
                
                }
                
                .box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}

                ")),
                        
                        shinyauthr::loginUI("login"),
                        
                        
                       tabItem(tabName = "medprep",
                                
                                
                                
                                
                                #textOutput("tit1",inline = T),
                                uiOutput("mpuiout")
                                
                                
                                
                                
                        ),   
                        
                      
                        
                        
                        
                        #tabItem(tabName = "tarifa",
                        #   textOutput("tarifahd1")         
                        #        ),   
                        
                        
                        
                        
                        HTML('<div data-iframe-height></div>')
                    )
)

server <- function(input, output, session){
    
    # observe({
    #  print(input$barra)
    #})
    
    
    credentials <- callModule(shinyauthr::login, "login", 
                              data = user_base,
                              user_col = user,
                              pwd_col = password_hash,
                              sodium_hashed = TRUE,
                              log_out = reactive(logout_init()))
    
    logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
    
    
    user_info <- reactive({credentials()$info})
    
    
    data_salud<-reactive({
        req(credentials()$user_auth)
        
        
        tmp <- read_excel(temp.file,col_names = T)
        
        prep=aggregate(tmp$`Cantidad de Afiliados`,by=list(tmp$`Razon Social entidad`),FUN=sum) %>% mutate(prop=round(100*x/sum(x),2)) %>% arrange_at("prop","desc")
        colnames(prep)=c("Entidad","Afiliados", "Share")
        
        return(prep)
        
    })
    
    data_cem<-reactive({
        req(credentials()$user_auth)
        
        
        tmp2 <- read_excel(temp.file,col_names = T,sheet = 2,skip=5)
        
        prep2=tmp2 %>% filter(!is.na(ENTIDAD)) 
        prep2=aggregate(list(afil=tmp2$AFILIADOS,ambu=tmp2$AMBULANCIAS),by=list(tmp2$ENTIDAD),FUN=sum) %>% mutate(p1=round(100*afil/sum(afil),2),p2=round(100*ambu/sum(ambu),2)) %>% arrange_at("p1","desc")
        colnames(prep2)=c("Entidad","Afiliados", "N_ambul","share_afiliados","share_ambul")
        
        return(prep2)
        
    })
    
    
    
    output$mpout1<-renderPlot({
        
        req(credentials()$user_auth)
        if(input$barra=="medprep"){
            data_salud() %>% arrange(-Share) %>% top_n(n=10) %>% ggplot(aes(x=reorder(Entidad,Share),y=Share,fill=Entidad))+labs(y="Market Share (%)",x="Entidad")+geom_bar(position = "identity",stat="sum",show.legend = T)+geom_text(inherit.aes = T,aes(label=paste(Share,"%")),nudge_y = 2)+guides(size=FALSE,fill=FALSE,color=FALSE)+
                coord_flip()+mytheme
            
        }
        else{}
        
    })
    
    
    
    output$mpout2<-renderDataTable({
        
        req(credentials()$user_auth)
        data_salud() 
        
        
    }, rownames=FALSE,escape=FALSE,options=list(pageLength = 7,lengthChange=FALSE,searchHighlight = TRUE,info = FALSE))
    
    
    
    output$mpout3<-renderPlot({
        
        req(credentials()$user_auth)
        if(input$barra=="medprep"){
            data_cem() %>% arrange(-share_afiliados) %>% top_n(n=10) %>% ggplot(aes(x=reorder(Entidad,share_afiliados),y=share_afiliados,fill=Entidad))+labs(y="Market Share (%)",x="Entidad")+geom_bar(position = "identity",stat="sum",show.legend = T)+geom_text(inherit.aes = T,aes(label=paste(share_afiliados,"%")),nudge_y = 3)+
                guides(size=FALSE,fill=FALSE,color=FALSE)+scale_fill_brewer(palette = "Blues")+coord_flip()+mytheme
            
        }
        else{}
        
    })
    
    
    output$mpout4<-renderDataTable({
        
        req(credentials()$user_auth)
        aux=data_cem()[,1:3]
        colnames(aux)[3]="No. Ambulancias"
        aux
        
        
    } ,rownames=FALSE,escape=FALSE,options=list(lengthChange=FALSE,searchHighlight = TRUE,info = FALSE))
    
    
    output$mpuiout<-renderUI({
        
        if(input$barra=="medprep"){
            req(credentials()$user_auth)
            fluidRow(
                
                column(width = 6,offset = 0,
                       widgetUserBox(title = "Market Share MP",width = 12, color = "blue",src="heart.png",type=2,plotOutput("mpout1"),
                                     dataTableOutput("mpout2"),tags$h6("Datos obtenidos de Supersalud, MP se refiere a poblacion afiliada a Medicina Integral y salud oral"))),
                column(width = 6,
                       widgetUserBox(title = "Market Share CEM",width = 12, color = "blue",src="ambulance.png",type=2,plotOutput("mpout3"),
                                     dataTableOutput("mpout4")))
            )
            
            
            
        } else{}
        
    })
    
    
    
    output$welcome <- renderText({
        req(credentials()$user_auth)
        glue("Bienvenido {user_info()$name} !")
    })
    
    
    
    
    
}

shinyApp(ui, server)