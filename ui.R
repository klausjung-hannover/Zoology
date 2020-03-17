library(shinythemes)
library (markdown)
library (shinyBS)


#fluidPage(
  #shinythemes::themeSelector(),




  shinyUI(
     navbarPage(h3("Oxygen transporter proteins"),
       theme=shinytheme("flatly"),
       tabPanel(h5(style="text-align:center", "Oxygen-hemoglobin dissociation curve"),      
        sidebarLayout(
          sidebarPanel(width=4,
            
            h4 ("Oxygen transport proteins:", bsButton("sauerstofftransporterhelp", label = "", icon = icon("question"),
                                                    size = "extra-small")),
            bsPopover(id = "sauerstofftransporterhelp", title = "",
                     content = paste0("For more information please click on the tab", a("oxygen transporter proteins ") ),
                     placement = "right", 
                     trigger = "hover", 
                     options = list(container = "body")
            
          ),
            
          
            
           div(style="display: inline-block;horizontal-align:top; width: 70px;",checkboxInput("graphhemo", "Hemoglobin", value=FALSE)),
           
           div(style="display: inline-block;horizontal-align:top; width:50px;",HTML("<br>")),
           div(style="display: inline-block;horizontal-align:top; width: 70px;",checkboxInput("graphmyoglobin", "Myoglobin", FALSE)) ,
           tags$br(),
           tags$br(),
           tags$br(),
           
           #h4 ("Acidity"),
           
           # radioButtons("pH", label = h5("pH-Wert"),
           #                    choices = c("increase", "decrease"),
           #                    selected = NA, inline= TRUE),
           # 
           selectInput ("pH", h4("Blood pH:"), c("", "decrease"="Abnahme", "increase"="Zunahme"), selected=FALSE, multiple = FALSE, selectize= FALSE, width=140),
           
           tags$br(),
            h4("How does the oxygen dissocation curve change?"),
           selectInput ("askaf", h5("The oxygen affinity..."), c("", "increases"="steigt","decreases"= "sinkt", "remains the same"="bleibt gleich"), selected=FALSE, multiple = FALSE, selectize= FALSE, width = 140), 
           #checkboxInput("askgraph", "Was sagt dieser Kurvenverlauf aus?", FALSE),
           
           tags$br(),
           tags$br(),
           tags$br(),
           tags$img(src="DigiStep.png")
           
          ),
        
          mainPanel( width=6 ,
                      plotOutput ("lines"),
                      tags$br(),
                      tags$br(),
                      textOutput ("sauerstoffbindungskurve"),
                      tags$head(tags$style("#sauerstoffbindungskurve{color: solid silver;
                          font-size: 18px;
                                           font-style: plain;
                                           
                                           }"
                       )),
                      textOutput("korrekt"),
                      tags$head(tags$style("#korrekt{color: green;
                                           font-size: 22px;
                                           font-style: plain;
                                           #text-decoration:underline;
                                           }")),
                      textOutput("falsch"),
                      tags$head(tags$style("#falsch{color: red;
                                           font-size: 22px;
                                           font-style: plain;
                                           #text-decoration:underline;
                                           }")),
                       textOutput ("nutzen1"),
                       
                        tags$head(tags$style("#nutzen1{color: solid silver;
                          font-size: 18px;
                                            font-style: plain;
                                            
                                             }"
                       )),

                     
                       tags$br(),
                       tags$br(),
                       textOutput ("hinweis"),
                       tags$head(tags$style("#hinweis{color: solid silver;
                                             font-size: 17px;
                                            font-style: plain;
                                            }")),
                      #actionLink("textbohreffekt", "Bohreffekt."),
                      #bsPopover(id="bohreffekt" , title =  "was", content = "hiertext")
                      
                      textOutput ("textbohreffekt"),
                      tags$head(tags$style("#textbohreffekt{color: blue;
                                             font-size: 17px;
                                           font-style: plain; text-decoration:underline
                                           }")),
                      bsPopover(id="textbohreffekt" , title =  "Definition", content = "The Bohr effect refers to the decreased oxygen affinity of hemoglobin in response to decreased blood pH.", placement = "left"),
                    
                                            
                     
                      tags$br(),
                      tags$br()
                     )
                    )
                  ),
              
  
              navbarMenu(h5("Oxygen Transporter"),
                  tabPanel (h4("Hemoglobin"),      
                    #sidebarLayout(
                    #  sidebarPanel(),
                    mainPanel (width=4,
                    h1 ("Hemoglobin"),
                    p("Iron-containing protein in the red blood cells (erythrocytes) of vertebrates."),
                   
                    h3 ("Structure"),
                   
                    p("Hemoglobin is a", span("tetramer.", style="col:green"), "Consisting of following subunits:"),
                    strong("Four globin chains:"), p("2 \u03B1- und 2 \u03B2 globin chanis, binding each one a molecule of heaem."),
                   
                    strong("Heme"), p("contains in its centre an iron-atom, which binds reversible oxgen from the blood. It is responsable for the red color of the blood."),
                    tags$img(src="hemoglobin.jpg"),
                    #plotOutput("bild"),
                    br(),
                    br(),
                    h3("Function"),
                    p ("Transports oxygen from the lung to the tissue. One hemoglobin molecule can bind up to four oxygen molecules reversibly. Low oxygen parcial pressure in blood leads to a slow saturation of hemoglobin. "), 
	                  strong ("The binding of oxygen to one of the subunits leads to the so called"), # Veraenderung der Haemoglobstruktur. sodass weitere O2-Molekuele leichter an die anderen Untereinheiten binden."),
                    #strong ("Dieser Effekt wird auch als"), 
                    actionLink("popup", "allosteric effect,"),#  style= "color:blue", textdecoration= "underline"),
                    bsPopover(id="popup",title="Allostery/ Positive cooperativity:", content= "Binding of the ligand at one site results in a conformation change of the protein increasing this way the facilitated binding of another ligand,"),
                    tags$head(tags$style("#popup{color: blue;
                                             font-size: 15px;
                                           font-style: plain; text-decoration:underline
                                           }")), 
                    strong ("which explains the sigmoid oxygen dissocation curve of the hemoglobin."),
                   
                    
                   
                    br(),
                    br(),
                    br(),
                      
                    
                    em (h4 ("Interesting out of evolutionary persepctive:")),
                    p ("Heme of hemoglobin is structural similar to chlorphyll. The main difference is found in the centre. Chlorophyll is built around of magnesium, instead of iron."),
                    br(),
                    br(),
                    br(),
                    br(),
                    br()
                    )
                  ),
                  
                  tabPanel(h4("Myoglobin"),

                       mainPanel(width=4, 
                       h1 ("Myoglobin"),
                       br(),
                       p("Oxygen-binding protein in heart and skeletal muscles."),
                      
                       h3 ("Structure"),
                       p( "Monomeric protein with", strong ("one"), "heme prosthetic group."),
                       tags$img (src="Myoglobin.png"),
                       h3 ("Function"),
                       p ("Recieves oxygen from red blood cells and transports it within the muscle cells. Unlike hemoglobin, which function is to transport oxygen, myoglobin serves as an oxygen storage. Its oxygen affinitiy is much higher than of hemoglobin."), 
                       strong ("The oxygen binding curve for myoglobin is hyperbolic, due to the monomeric structure of myoglobin and therefore the absence of positive cooperativity."),
                       
                       
                       
                       br(),
                       br(),
                     
                       br(),
                       br(),
                       br(),
                       br(),
                       
                       br(),
                       br(),
                       br()
                  
                  
                  )
                ),
                
                tabPanel(h4("Sources"),
                         
                         h3 ("Bibliography"),
                         br(),
                         "Engelhardt, W., Breves, G., Diener , M., & Gaebel, G. (2015). ", em("Physiologie der Haustiere"), 
                         "(5. ed.). Stuttgart: Georg Thieme Verlag",
                         
                         br(),
                         br(),
                         #h3 ("Bilderverzeichnis"),
                         br(),
                         ""
                         
                                   
                )
                
                
                
              )#,
       
              #tabPanel (tags$img(src="DigiStep.png"))
     )
  )
           
  
                
               
                     
                           
                             
                
      


  
