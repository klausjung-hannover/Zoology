library(jpeg)
library(stringi)
library (shiny)





function(input, output,session) {
	
	lblack = rgb(100, 100, 100, alpha=90, maxColorValue=255)
	lred = rgb(255, 100, 100, alpha=90, maxColorValue=255)

	pH7.6 <-read.table ("Tabellen\\pH_7_6.txt",header=T,sep="\t")
	pH7.4 <-read.table ("Tabellen\\pH_7_4.txt",header=T,sep="\t")
  pH7.2 <-read.table("Tabellen\\pH_7_2.txt",header=T,sep="\t")
  myoglobin <-read.table ("Tabellen\\myoglobin.txt",header=T,sep="\t")
  
 
  observeEvent (input$pH, {
    updateSelectInput ( session, "askaf", selected = NA
      )
           
  })          

  observeEvent (input$graphhemo, {
    updateSelectInput ( session, "askaf", selected = NA
    )
    
  })     
  
  
  observeEvent (input$graphhemo, {
    updateSelectInput ( session, "pH", selected = NA
    )
    
  })  
  
  observeEvent (input$graphmyoglobin, {
    updateSelectInput ( session, "pH", selected = NA
    )
    
  })  
  
  
  observeEvent (input$graphmyoglobin, {
    updateSelectInput ( session, "askaf", selected = NA
    )
    
  }) 
  
  
  #########SAUERSTOFFBINDUNGSKURVEN
  
	output$lines <- renderPlot({
	  #par(pin= c(7,4))
	  #par()
	  plot(1,1, type="n", xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="pO2 (mmHg)")
	 
	 
	
	  if (input$graphhemo=="TRUE")  {
	  plot(1,1, type="n", xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="pO2 (mmHg)")
	  text(x=100,y=95, "pH=7,4", cex=1, col="black")
	  lines (spline(pH7.4), col="black", lwd=2, lty=1)
	  output$sauerstoffbindungskurve <- renderText ({"The oxygen hemoglobin dissociation curve describes the relationship between the oxygen saturation (SO2 =amount of O2 bound to hemoglobin) and the prevaling oxygen parcial pressure (pO2) in the blood."
	   })
	  }
	  

	  
	  #### Keine Auswahl graph, ph und askaf ###
	  
	  if ((input$graphhemo=="FALSE") & (input$pH =="") & (input$askaf == "")) {
	    
	    
	    plot(1,1, type="n", xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="pO2 (mmHg)")
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$korrekt <- renderText ({""})
	    output$falsch <-renderText ({""})
	    output$nutzen1 <- renderText ({""})
	    output$hinweis <- renderText ({""})
	    output$textbohreffekt <- renderText ({""})
	    
	  }
	  
	  
	  
	 
	 
	  ####pH 7,6 #######
	  
	  #####observeEvent bei Aenderung pH askaf ausblenden
	  if (input$graphhemo=="TRUE" & input$pH =="Zunahme" & input$askaf == "") {


	    text(x=10,y=60, "pH=7,6", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.6), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    text(x=10,y=60, "pH=7,6", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$korrekt <- renderText ({""})
	    output$falsch <-renderText ({""})
	    output$nutzen1 <- renderText ({""})
	    output$hinweis <- renderText ({""})
	    output$textbohreffekt <- renderText ({""})

	  }
	  
	  
	  
	  if (input$graphhemo=="TRUE" & input$pH =="Zunahme") {
	    
	    text(x=10,y=60, "pH=7,6", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.6), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    text(x=10,y=60, "pH=7,6", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	  
	    
	  }
	  

	  if (input$graphhemo=="TRUE" & input$pH =="Zunahme" & input$askaf=="steigt"){
	    text(x=10,y=60, "pH=7,6", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.6), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    text(x=10,y=60, "pH=7,6", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")

	    arrows (x0=28, y0=60, x1=22, y1=60, lty=1, length = 0.25, angle=30, lwd=2, col="red")
	    text(35, 58, paste("shift to the left"), cex=1.5, col="red", adj = c(0,0))
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$korrekt <- renderText ({"Correct!"})
	    output$falsch <-renderText ({""})
	    output$nutzen1 <- renderText ({"A high blood pH leads to an increased affinity of the hemoglobin for oxygen. The oxygen dissociation curve is shifted to the left, resulting in less oxygen delivery to the tissue."})

	    output$hinweis <- renderText ({"The influence of pH on hemoglobin oxygen affinity is called:"})
	    output$textbohreffekt <- renderText ({"Bohr effect"})

	    
	    
	    
	    }
	  

	  if (input$graphhemo=="TRUE" & input$pH =="Zunahme" & input$askaf=="sinkt"){
		  text(x=10,y=60, "pH=7,6", cex=1, col="red")
		  text(x=100,y=95, "pH=7,4", cex=1, col="black")
		  lines (spline(pH7.6), col="red", lwd=2, lty=1)
		  lines (spline(pH7.4), col="black", lwd=2, lty=2)
		  output$sauerstoffbindungskurve <- renderText({""}) 
		  output$falsch <- renderText ({"Incorrect"})
		  output$korrekt <- renderText ({""})
		  output$nutzen1<- renderText ({"Carbon dioxide is secreted by the lung, leading this way to an increased blood pH. Hemoglobin will be loaded within the lungs capillars with oxygen."})
		   
		  output$hinweis <- renderText ({""})
		  output$textbohreffekt <-renderText({""})
	  }
	  
	  
	  if (input$graphhemo=="TRUE" & input$pH =="Zunahme" & input$askaf=="bleibt gleich"){
	    text(x=10,y=60, "pH=7,6", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.6), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$falsch <- renderText ({"Incorrect"})
	    output$korrekt <- renderText ({""})
	    output$nutzen1<- renderText ({"Carbon dioxide is secreted by the lung, leading this way to an increased blood pH. Hemoglobin will be loaded within the lungs capillars with oxygen."})
	    
	    output$hinweis <- renderText ({""})
	    output$textbohreffekt <-renderText({""})
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  ####pH 7,4 #########
    
	  # if ((input$pH=="2") & (input$graphhemo=="TRUE")& (input$askgraph=="TRUE")){
	  #    text(x=100,y=95, "pH=7,4", cex=1, col="red")
	  #    lines (spline(pH7.4), col="red", lwd=2, lty=1)
	  #    output$nutzen1<- renderText ({"Sigmoide Sauerstoffbindungskurve"})
	  #    output$nutzen2 <-renderText ({"Die Kurve stellt die Beziehung zwischen dem Anteil an mit Sauerstoff beladenen Haemoglobin (=SO2) und dem Sauerstoffpartialdruck (pO2), d.h. die im Blut geloeste Menge an O2 dar.
	  #      Der Verlauf ist sigmoid: Dies deutet auf eine kooperative Bindung des Sauerstoff an Haemoglobin hin."})
	  #    output$hinweis <- renderText ({""})
	  #    
	  #  }
	  #   
	  # 
	  # if ((input$pH=="2")& (input$graphhemo=="TRUE")& input$askgraph=="FALSE") {
	  #   text(x=100,y=95, "pH=7,4", cex=1, col="red")
	  #   lines (spline(pH7.4), col="red", lwd=2, lty=1)
	  #   output$nutzen1<- renderText ({""})
	  #   output$nutzen2 <- renderText ({""})
	  #   output$hinweis <- renderText ({""})
	  #   }
	  # 
	 
	  
	  ### pH 7,2########
	  
	  
	  if ((input$pH=="Abnahme") & (input$graphhemo=="TRUE")){
	    text(x=80,y=80, "pH=7,2", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.2), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)

	  }

	  if (input$graphhemo=="TRUE" & input$pH =="Abnahme" & input$askaf == "") {
	    text(x=80,y=80, "pH=7,2", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.2), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText({""})
	    output$nutzen1<- renderText ({""})
	    output$hinweis <- renderText ({""})
	    output$hinweis <- renderText ({""})
	    output$textbohreffekt <- renderText ({""})
	    
	  } 


	  if ((input$pH=="Abnahme") & (input$graphhemo=="TRUE")& (input$askaf=="sinkt")){

	    text(x=80,y=80, "pH=7,2", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.2), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    text (40,53, paste("shift to the right"), cex=1.5, col="red", adj = c(0,0))
	    arrows (x0=28, y0=55, x1=36, y1=55, lty=1, length = 0.25, angle=30, lwd=2, col="red")  
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText({"Correct!"})
	    output$nutzen1<- renderText ({"Low blood pH decreases the affinity of hemoglobin for oxygen. The oxygen dissocation curve is shifted to the right. Oxygen is delivered to the tissue."})

	    output$hinweis <- renderText ({""})
	    output$hinweis <- renderText ({"The influence of pH on hemoglobin oxygen affinitiy is called:"})
	    output$textbohreffekt <- renderText ({"Bohr effect"})

	    }




	  if ((input$pH=="Abnahme") & (input$graphhemo=="TRUE")& (input$askaf=="steigt")){

	    text(x=80,y=80, "pH=7,2", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.2), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$korrekt<- renderText ({""})
	    output$falsch <-renderText ({"Incorrect"})
	    output$nutzen1<- renderText ({"Metabollically active tissue produces carbon dioxide, leading to a low blood pH. Hemoglobins affinity for oxygen decreases in order to carry oxygen to the tissue."})
	    output$hinweis <- renderText ({""})
	    output$textbohreffekt <-renderText({""})


	    }


	  if ((input$pH=="Abnahme") & (input$graphhemo=="TRUE")& (input$askaf=="bleibt gleich")){

	    text(x=80,y=80, "pH=7,2", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.2), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$korrekt<- renderText ({""})
	    output$falsch <-renderText ({"Incorrect"})
	    output$nutzen1<- renderText ({"Metabollically active tissue produces carbon dioxide, leading to a low blood pH. Hemoglobins affinity for oxygen decreases, in consequence oxygen is carried to the tissue."})

	    output$hinweis <- renderText ({""})
	    output$textbohreffekt <-renderText({""})
	    }


	 
	  ####MYOGLOBIN ALLEINE ######
	  
	  
	  
	   if(input$graphmyoglobin=="TRUE")  {
	     lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	     #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	     output$sauerstoffbindungskurve <- renderText({"The oxygen dissocation curve of myoglobin is hyperbolic. This indicates that myoglobin is saturated at much lower concentrations of 02 compared with hemoglobin."})  
	       
	     output$nutzen1<- renderText ({""})
	     output$nutzen2 <- renderText ({""})
	     output$hinweis <- renderText ({""})
	   }
	  
	  
	  if((input$graphmyoglobin=="TRUE") & (input$askaf =="") & (input$pH ==""))  {
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$nutzen1<- renderText ({""})
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText ({""})
	    output$hinweis <- renderText ({""})
	  }
	  
	  
	  
	  
	  
	  if((input$graphmyoglobin=="TRUE") & (input$askaf =="") & (input$pH =="Abnahme"))  {
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$nutzen1<- renderText ({""})
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText ({""})
	    output$hinweis <- renderText ({""})
	  }
	  
	  if((input$graphmyoglobin=="TRUE") & (input$askaf =="") & (input$pH =="Zunahme"))  {
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$nutzen1<- renderText ({""})
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText ({""})
	    output$hinweis <- renderText ({""})
	  }
	  
	  
	  
	  
	  
	  
	  if((input$graphmyoglobin=="TRUE")& (input$askaf=="steigt"))  {
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$korrekt <-renderText ({""})
	    output$falsch <-renderText ({"Incorrect"})
	    output$nutzen1<- renderText ({"Myoglobin is made up of a single polypeptide and can only bind one oxygen-molecule.Oxygen affinity does not change through wide ranges of blood pH."})
	    output$nutzen2 <- renderText ({""})
	    output$hinweis <- renderText ({""})
	  }
	  
	  
	  if((input$graphmyoglobin=="TRUE")& (input$askaf=="sinkt"))  {
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$korrekt <-renderText ({""})
	    output$falsch <-renderText ({"Incorrect"})
	    output$nutzen1<- renderText ({"Myoglobin is made up of a single polypeptide and can only bind one oxygen-molecule.Oxygen affinity does not change through wide ranges of blood pH."})
	    output$nutzen2 <- renderText ({""})
	    output$hinweis <- renderText ({""})
	  }
	  
 	       
 	  if((input$graphmyoglobin=="TRUE")& (input$askaf=="bleibt gleich")){
 	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
 	    output$sauerstoffbindungskurve <- renderText({""}) 
 	    output$korrekt <-renderText({"Correct!"})
 	    output$falsch <-renderText ({""})
 	    output$nutzen1 <- renderText ({"Unlike hemoglobin mygoglobin can only bind one oxygen molecule. It has a higher affinitiy for oxygen than hemoglobin. It is not influenced by the blood pH."})
      output$hinweis <- renderText ({"Myoglobin is not influenced by the Bohr effect and does not bind O2 cooperativly."})
	
 	  }  
 	      
	
	  
	  
	  
	  
	  #####MYOGLOBIN UND HAEMOGLOBIN ZUSAMMEN
	  
	  
	  if((input$graphmyoglobin=="TRUE")& (input$graphhemo=="TRUE"))  {
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    legend (80, 30, c ("Hemoglobin", "Myoglobin"), lty = c(1,1), lwd= c(2.5, 2.5), col=c( "black", "blue"), box.col = "darkgrey", cex=1.2)
	    output$sauerstoffbindungskurve <- renderText({"The differences in the shape of the oxygen dissocation curve for hemoglobin and myoglobin is explained by the different structure of both proteins. Hemoglobin is a tetramer, capable of binding cooperativly four oxygen molecules. Myglobin on the other hand is monomer and can only bind one oxygen molecule."}) 
	    output$nutzen1<- renderText ({""})
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText({""})
	    output$hinweis <- renderText ({""})
	  }
	  
	  
	  if((input$graphmyoglobin=="TRUE")& (input$graphhemo=="TRUE") & (input$pH=="Abnahme"))  {
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    legend (80, 30, c ("Hemoglobin", "Hemoglobin", "Myoglobin"), lty = c(1,1,1), lwd= c(2.5, 2.5, 2.5), col=c("red", "black", "blue"), box.col = "darkgrey", cex=1.2)
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$nutzen1<- renderText ({""})
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText({""})
	    output$hinweis <- renderText ({""})
	  }
	  
	  if((input$graphmyoglobin=="TRUE")& (input$graphhemo=="TRUE") & (input$pH=="Zunahme"))  {
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    legend (80, 30, c ("Hemoglobin", "Hemoglobin", "Myoglobin"), lty = c(1,1,1), lwd= c(2.5, 2.5, 2.5), col=c("red", "black", "blue"), box.col = "darkgrey", cex=1.2)
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$nutzen1<- renderText ({""})
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText({""})
	    output$hinweis <- renderText ({""})
	  }
	  
	  
	  
	  if((input$graphmyoglobin=="TRUE")& (input$graphhemo=="TRUE") & (input$pH =="Abnahme") & (input$askaf== "sinkt"))  {
	    
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    legend (80, 30, c ("Hemoglobin", "Hemoglobin", "Myoglobin"), lty = c(1,1,1), lwd= c(2.5, 2.5, 2.5), col=c("red", "black", "blue"), box.col = "darkgrey", cex=1.2)

	  text(x=80,y=80, "pH=7,2", cex=1, col="red")
	  text(x=100,y=95, "pH=7,4", cex=1, col="black")
	  lines (spline(pH7.2), col="red", lwd=2, lty=1)
	  lines (spline(pH7.4), col="black", lwd=2, lty=2)
	  lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	  text (40,53, paste("shift to the right"), cex=1.5, col="red", adj = c(0,0))
	  arrows (x0=28, y0=55, x1=36, y1=55, lty=1, length = 0.25, angle=30, lwd=2, col="red")  
	  output$sauerstoffbindungskurve <- renderText({""}) 
	  output$falsch <- renderText ({""})
	  output$korrekt <- renderText({""})
	  output$nutzen1<- renderText ({"Oxygen affinity of hemoglobin depends an the pH-level of the blood. Myoglobin binds oxygen independently from the pH-level."})
	
	  output$hinweis <- renderText ({"Decreasing blood pH leads to lower affinity of hemoglobin for oxygen."})
	  
	  output$textbohreffekt <- renderText ({""})
	
	  }
	  if((input$graphmyoglobin=="TRUE")& (input$graphhemo=="TRUE") & (input$pH =="Abnahme") & (input$askaf== "steigt"))  {
	    
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    legend (80, 30, c ("Hemoglobin", "Hemoglobin", "Myoglobin"), lty = c(1,1,1), lwd= c(2.5, 2.5, 2.5), col=c("red", "black", "blue"), box.col = "darkgrey", cex=1.2)
	    
	    text(x=80,y=80, "pH=7,2", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.2), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    text (40,53, paste("shift to the right"), cex=1.5, col="red", adj = c(0,0))
	    arrows (x0=28, y0=55, x1=36, y1=55, lty=1, length = 0.25, angle=30, lwd=2, col="red")  
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText({""})
	    output$nutzen1<- renderText ({"Oxygen affinity of hemoglobin depends an the pH-level of the blood. Myoglobin binds oxygen independently from the pH-level."})
	    
	    output$hinweis <- renderText ({"Decreasing blood pH leads to lower affinity of hemoglobin for oxygen"})
	    output$hinweis <- renderText ({""})
	    output$textbohreffekt <- renderText ({""})
	    
	  }
	  
	  
	  
	  if((input$graphmyoglobin=="TRUE")& (input$graphhemo=="TRUE") & (input$pH =="Abnahme") & (input$askaf== "bleibt gleich"))  {
	    
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    legend (80, 30, c ("Hemoglobin", "Hemoglobin", "Myoglobin"), lty = c(1,1,1), lwd= c(2.5, 2.5, 2.5), col=c("red", "black", "blue"), box.col = "darkgrey", cex=1.2)
	    
	    text(x=80,y=80, "pH=7,2", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.2), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    text (40,53, paste("shift to the right"), cex=1.5, col="red", adj = c(0,0))
	    arrows (x0=28, y0=55, x1=36, y1=55, lty=1, length = 0.25, angle=30, lwd=2, col="red")  
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText({""})
	    output$nutzen1<- renderText ({"Oxygen affinity of hemoglobin depends an the pH-level of the blood. Myoglobin binds oxygen independently from the pH-level."})
	    
	    output$hinweis <- renderText ({"Decreasing blood pH leads to lower affinity of hemoglobin for oxygen. For myoglobin the affinity remains the same!"})
	   
	    output$textbohreffekt <- renderText ({""})
	    
	  }
	  
	  
	  
	  
	  
	  
	  
	  if((input$graphmyoglobin=="TRUE")& (input$graphhemo=="TRUE") & (input$pH =="Zunahme") & (input$askaf== "steigt"))  {
	    
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    legend (80, 30, c ("Hemoglobin", "Hemoglobin", "Myoglobin"), lty = c(1,1,1), lwd= c(2.5, 2.5, 2.5), col=c("red", "black", "blue"), box.col = "darkgrey", cex=1.2)
	    
	    text(x=10,y=60, "pH=7,6", cex=1, col="red")
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.6), col="red", lwd=2, lty=1)
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    arrows (x0=28, y0=60, x1=22, y1=60, lty=1, length = 0.25, angle=30, lwd=2, col="red")
	    text(35, 58, paste("shift to the left"), cex=1.5, col="red", adj = c(0,0))
	    
	    
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText({""})
	    output$nutzen1<- renderText ({"Oxygen affinity of hemoglobin depends an the pH-level of the blood. Myoglobin binds oxygen independently from the pH-level."})
	    output$hinweis <- renderText ({"Increasing blood pH leads to a higher oxygen affinity of hemoglobin."})
	    output$textbohreffekt <- renderText ({""})
	    
	  }
	  
	  if((input$graphmyoglobin=="TRUE")& (input$graphhemo=="TRUE") & (input$pH =="Zunahme") & (input$askaf== "sinkt"))  {
	    
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    legend (80, 30, c ("Hemoglobin", "Hemoglobin", "Myoglobin"), lty = c(1,1,1), lwd= c(2.5, 2.5, 2.5), col=c("red", "black", "blue"), box.col = "darkgrey", cex=1.2)
	    text(x=10,y=60, "pH=7,6", cex=1, col="red")
	    lines (spline(pH7.6), col="red", lwd=2, lty=1)
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    arrows (x0=28, y0=60, x1=22, y1=60, lty=1, length = 0.25, angle=30, lwd=2, col="red")
	    text(35, 58, paste("Shift to the left"), cex=1.5, col="red", adj = c(0,0))
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText({""})
	    output$nutzen1<- renderText ({"Oxygen affinity of hemoglobin depends an the pH-level of the blood. Myoglobin binds oxygen independently from the pH-level."})
	    
	    output$hinweis <- renderText ({"Increasing blood pH leads to a higher oxygen affinity of hemoglobin."})
	    output$textbohreffekt <- renderText ({""})
	    
	  }
	  
	  if((input$graphmyoglobin=="TRUE")& (input$graphhemo=="TRUE") & (input$pH =="Zunahme") & (input$askaf== "bleibt gleich"))  {
	    
	    #text (x=20, y=98, "Myoglobin", cex=1, col= "blue")
	    legend (80, 30, c ("Hemoglobin", "Hemoglobin", "Myoglobin"), lty = c(1,1,1), lwd= c(2.5, 2.5, 2.5), col=c("red", "black", "blue"), box.col = "darkgrey", cex=1.2)
	    
	    lines (spline(pH7.6), col="red", lwd=2, lty=1)
	    text(x=100,y=95, "pH=7,4", cex=1, col="black")
	    text(x=10,y=60, "pH=7,6", cex=1, col="red")
	    lines (spline(pH7.4), col="black", lwd=2, lty=2)
	    arrows (x0=28, y0=60, x1=22, y1=60, lty=1, length = 0.25, angle=30, lwd=2, col="red")
	    text(35, 58, paste("shift ti the left"), cex=1.5, col="red", adj = c(0,0))
	    lines (spline(myoglobin), col="blue", lwd=2, lty=1, xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="PO2 (mmHg)")
	    
	    output$sauerstoffbindungskurve <- renderText({""}) 
	    output$falsch <- renderText ({""})
	    output$korrekt <- renderText({""})
	    output$nutzen1<- renderText ({"Oxygen affinity of hemoglobin depends an the pH-level of the blood. Myoglobin binds oxygen independently from the pH-level."})
	    
	    output$hinweis <- renderText ({"Decreasing blood pH leads to lower affinity of hemoglobin for oxygen. For myoglobin the affinity remains the same!"})
	    output$textbohreffekt <- renderText ({""})
	    
	  }
	  
	  
	  
	  
	})
	  
	 
	  ##### TEXT UNTER DEM GRAPH###################################
	
	    #observeEvent (

	    #input$askgraph, {
	     # if (input$graphhemo=="TRUE" & input$pH=="7,4")

	      #{output$nutzen1<- renderText({"Sigmoide Sauerstoffbindungskurve"})
	      #output$nutzen2 <- renderText ({"Die Kurve stellt die Beziehung zwischen dem Anteil an mit Sauerstoff beladenen Haemoglobin (=SO2) und dem Sauerstoffpartialdruck (pO2), d.h. die im Blut geloeste Menge an O2 dar.
	      #Der Verlauf ist sigmoid: Dies deutet auf eine kooperative Bindung des Sauerstoff an Haemoglobin hin."
	     #   })
	      #print(input$askgraph)
	      #output$lines <- renderPlot({
	       # plot(1, 1, type="n", xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="pO2 (mmHg)")
	        # {text(x=100,y=95, "pH=7,4", cex=1, col="red")
	         # lines (spline(pH7.4), col="red", lwd=2, lty=1)
	          #}})


	      #}})
	  # #
	  # 
	  # # 
	  # # 
	  #observeEvent (


	    #input$askgraph, {
	      # if ((input$askgraph==TRUE) & (input$graphhemo==TRUE) & (input$pH=="7,2")){
	      #   output$nutzen1<- renderText ({"Bei niedrigem pH sinkt die Sauerstoffaffinitaet des Haemoglobin."})
	      #   output$nutzen2 <- renderText ({"Das stoffwechselaktiven Gewebe produziert viel Kohlendioxid, wodurch der pH-Wert herabgesetzt wird.
	      #                   Die Sauerstoffabgabe an das Gewebe wird erleichtert."})
	      #
	      #   output$lines <- renderPlot({
	      #
	      #     plot(1, 1, type="n", xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="pO2 (mmHg)")
	      #     text(x=80,y=80, "pH=7,2", cex=1, col="red")
	      #     text(x=100,y=95, "pH=7,4", cex=1, col="black")
	      #     lines (spline(pH7.2), col="red", lwd=2, lty=1)
	      #     lines (spline(pH7.4), col="red", lwd=2, lty=2)
	      #     text (40,53, paste("Rechtsverschiebung"), cex=1.5, col="red", adj = c(0,0))
	      #     arrows (x0=28, y0=55, x1=36, y1=55, lty=1, length = 0.25, angle=30, lwd=2, col="red")  })
}
	        #}})
	  #
	  #
	  # observeEvent (
	  # 
	  #   input$askgraph, {
	  #     if (input$graphhemo=="TRUE" & input$pH=="7,6"){
	  #       output$nutzen1<- renderText ({"Bei hohem pH steigt die Sauerstoffaffinitaet des Haemoglobin."})
	  #       output $nutzen2 <- renderText ({"In der Lunge wird CO2 abgegeben, dadurch steigt der pH-Wert. Das Haemoglobin wird mit Sauerstoff beladen."})
	  #       output$lines <- renderPlot({
	  #         plot(1, 1, type="n", xlim=c(0, 120), ylim=c(0, 100), ylab="SO2 (%)", xlab="pO2 (mmHg)")
	  #         text(x=10,y=60, "pH=7,6", cex=1, col="red")
	  #         text(x=100,y=95, "pH=7,4", cex=1, col="black")
	  #         lines (spline(pH7.6), col="red", lwd=2, lty=1)
	  #         lines (spline(pH7.4), col="black", lwd=2, lty=2)
	  #         arrows (x0=28, y0=60, x1=22, y1=60, lty=1, length = 0.25, angle=30, lwd=2, col="red")
	  #         text(35, 58, paste("Linksverschiebung"), cex=1.5, col="red", adj = c(0,0))
	  # 
	  #         })
	  # 
	  #   }})
	  # 
	  # 
	  # 
	  # 
	  # observeEvent (
	  #   
	  #   input$askmyoglobingraph, {
	  #     if (input$graphmyoglobin=="TRUE")
	  #       
	  #     {output$nutzen3<- renderText({"Die Sauerstoffbindungskurve des Myoglobin ist hyberbol"})
	  #     output$nutzen4 <- renderText ({"Myoglobin kann nur ein Sauerstoffmolkuel binden. Diesen bindet er mit einer hoeheren Affinitaet als Haemoglobin."})
	  #     }}) 
	  #  
	  #  
	  # 
	  #   
	  #   
	  
	 
	  # if ((input$askgraph=="TRUE") & (input$pH=="7,4"))  {
	  #   
	  #   output$nutzen1 <- renderText ({"Sauerstoffbindungskurve"})
	  #   output$nutzen2 <- renderText ({"
	  #     Die Kurve stellt die Beziehung zwischen dem Anteil an mit Sauerstoff beladenen Haemoglobin (=SO2) und dem Sauerstoffpartialdruck (pO2), d.h. die im Blut geloeste Menge an O2 dar.
	  #     Der Verlauf ist sigmoid: Dies deutet auf eine kooperative Bindung des Sauerstoff an Haemoglobin hin."})
	  # 
	  #   }
	  
	   # if ((input$asknutzen=="TRUE") & (input$pH=="7,4"))  {
	   #  
	   #  output$nutzen1 <- renderText ({"Oxigenierung"})
	   #  output$nutzen2 <- renderText ({"Reversible Bindung molekularen Sauerstoffs. Bei niedrigem Sauerstoffpartialdruck bindet der Sauerstoff nur langsam an das Haemoglobin. 
	   #                                Die Bindung des Sauerstoff fuehrt jedoch zu einer Veraenderung der Haemoglobinstruktur.
	   #                                Es kommt zu einer kooperativen Wechselwirkung zwischen den vier Untereinheiten des Haemoglobin, die dazu fuehrt das weitere Sauerstoff-Molekuele leichter gebunden werden."})
	   # 
	   #  }
	  
	  
	  
	  

	  
	

	    
	 
	    
	    
	    
	    
	 # })
	  
	  
	               
	

	
