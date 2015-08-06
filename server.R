## This App is developed by Yan Li for requested project (CRI-BIO-300), last update on August, 2015

hg19genome <- BSgenome.Hsapiens.UCSC.hg19
mm10genome <- BSgenome.Mmusculus.UCSC.mm10

shinyServer(function(input, output, session) {
    
  output$choose_genome <- renderUI({
    radioButtons(inputId="genome",
                 label = "Please choose the genome you want to search",
                 choices = c("Human (hg19)" = "hg19", 
                             "Mouce (mm10)" = "mm10")
    )
  })
  
  output$genome_options <- renderUI({
    if(is.null(input$genome)) return("hg19")
    else {
      if(input$genome == "hg19") chrNames <- c(seqnames(hg19genome)[1:25], "All")
      if(input$genome == "mm10") chrNames <- c(seqnames(mm10genome)[1:22], "All")
    }
    radioButtons(inputId="chr",
                 label=NULL,
                 choices = chrNames,
                 selected = "All")
    
  })
  
  output$genomeInfo <- renderText({
    as.character(input$genome)
  })
  
  output$genomeAndChrInfo <- renderText({
    input$submit
    isolate({
      if (is.null(input$chr) | is.null(input$genome)) return()
      else paste("The NGG search is on the ", as.character(input$chr), " of ", as.character(input$genome), " genome.", sep="")
    })    
  })
  
  
  HitAllRes <- reactive({
    input$submit
    isolate({
      
      #print("****")
      #print(input$chr)
      #print("****")
      seq <- input$seq
      seqAGG <- paste(input$seq, "AGG", sep="")
      seqTGG <- paste(input$seq, "TGG", sep="")
      seqGGG <- paste(input$seq, "GGG", sep="")
      seqCGG <- paste(input$seq, "CGG", sep="")
      seqNGG <- c(seqAGG, seqTGG, seqGGG, seqCGG)
      
      #if(is.null(input$genome)) return("hg19")
      #if(is.null(input$chr)) return("all")
      #print(input$genome)
      #print(paste ("search on " , input$chr, sep="") )
      #print(paste("mismatch is chosen with " , as.numeric(input$mismatchNo), sep=""))
      #print("*********")
      
      withProgress(message = 'Processing ', value = 0, { 
        
        if(input$genome == "hg19") {
          finalRes <- data.frame(A= numeric(0), B= numeric(0), C= numeric(0))
          countRes <- data.frame(A= numeric(0), B= numeric(0))
          
          if(input$chr == "All") {
            # Number of times we'll go through the loop
            n <- 25              
            for (i in 1:25) {          
              
              seqHitNo <- 0          
              seqnames <- seqnames(hg19genome)          
              chrName <- seqnames[i]
              #print(paste("Searching on chromosome ", chrName, sep=""))
              subject <- hg19genome[[chrName]]
              
              # Increment the progress bar, and update the detail text.
              incProgress(1/n, detail = paste(i*100/n, "%", paste(" searching on ", chrName, sep=""), sep="") ) 
              
              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
              ###########
              for (j in 1:length(seqNGG)){
                seq <- seqNGG[j]
                #if (i ==1) print(seq)
                res <- matchPattern(seq, subject, max.mismatch=as.numeric(input$mismatchNo) )
                seqHitNoUpdat <- length(res@ranges@start)
                seqHitNo <- seqHitNo + seqHitNoUpdat
                #print(paste("hit number is" , seqHitNoUpdat, sep=" "))               
                chrRes <- data.frame(chr=rep(chrName,length(res@ranges@start)), start=res@ranges@start, end= (res@ranges@start+res@ranges@width))
                finalRes <- rbind(finalRes, chrRes)          
              }
              #print(seqHitNo)
              #print("*******")
              countResSep <- data.frame(chr=chrName, hitno=seqHitNo)
              countRes <- rbind(countRes, countResSep)
            }
            HitCombRes <- list(finalRes = finalRes, countRes=countRes)
          } else {
            n<-4
            seqHitNo <- 0          
            seqnames <- seqnames(hg19genome)          
            chrName <- input$chr
            #print(paste("Searching on chromosome ", chrName, sep=""))
            subject <- hg19genome[[chrName]]
            
            for (j in 1:length(seqNGG)){
              # Increment the progress bar, and update the detail text.
              incProgress(1/n, detail = paste(j*100/n, "%", paste(" searching on ", chrName, sep=""), sep="") )         
              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
              ###########
              seq <- seqNGG[j]
              res <- matchPattern(seq, subject, max.mismatch=as.numeric(input$mismatchNo) )
              seqHitNoUpdat <- length(res@ranges@start)
              seqHitNo <- seqHitNo + seqHitNoUpdat
              #print(paste("hit number is" , seqHitNoUpdat, sep=" "))               
              chrRes <- data.frame(chr=rep(chrName,length(res@ranges@start)), start=res@ranges@start, end= (res@ranges@start+res@ranges@width))
              finalRes <- rbind(finalRes, chrRes) 
              
            }
            countResSep <- data.frame(chr=chrName, hitno=seqHitNo)
            countRes <- rbind(countRes, countResSep)
            
            HitCombRes <- list(finalRes = finalRes, countRes=countRes)
          }        
          
          HitCombRes <- list(finalRes = finalRes, countRes=countRes)
          
        } else if (input$genome == "mm10") {
          
          finalRes <- data.frame(A= numeric(0), B= numeric(0), C= numeric(0))
          countRes <- data.frame(A= numeric(0), B= numeric(0))
          
          if(input$chr == "All") {
            # Number of times we'll go through the loop
            n <- 22              
            for (i in 1:22) {          
              
              seqHitNo <- 0          
              seqnames <- seqnames(mm10genome)          
              chrName <- seqnames[i]
              #print(paste("Searching on chromosome ", chrName, sep=""))
              subject <- mm10genome[[chrName]]
              ###########
              # Increment the progress bar, and update the detail text.
              incProgress(1/n, detail = paste(as.integer(i*100/n), "%", paste(" searching on ", chrName, sep=""), sep="") ) 
              
              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
              ###########
              for (j in 1:length(seqNGG)){
                seq <- seqNGG[j]
                res <- matchPattern(seq, subject, max.mismatch=as.numeric(input$mismatchNo) )
                seqHitNoUpdat <- length(res@ranges@start)
                seqHitNo <- seqHitNo + seqHitNoUpdat
                #print(paste("hit number is" , seqHitNoUpdat, sep=" "))               
                chrRes <- data.frame(chr=rep(chrName,length(res@ranges@start)), start=res@ranges@start, end= (res@ranges@start+res@ranges@width))
                finalRes <- rbind(finalRes, chrRes)          
              }
              #print(seqHitNo)
              #print("*******")
              countResSep <- data.frame(chr=chrName, hitno=seqHitNo)
              countRes <- rbind(countRes, countResSep)
            }
            HitCombRes <- list(finalRes = finalRes, countRes=countRes)
          } else {
            n<-4
            seqHitNo <- 0          
            seqnames <- seqnames(mm10genome)          
            chrName <- input$chr
            #print(paste("Searching on chromosome ", chrName, sep=""))
            subject <- mm10genome[[chrName]]
            
            for (j in 1:length(seqNGG)){
              # Increment the progress bar, and update the detail text.
              incProgress(1/n, detail = paste(j*100/n, "%", paste(" searching on ", chrName, sep=""), sep="") )         
              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
              ###########
              seq <- seqNGG[j]
              res <- matchPattern(seq, subject, max.mismatch=as.numeric(input$mismatchNo) )
              seqHitNoUpdat <- length(res@ranges@start)
              seqHitNo <- seqHitNo + seqHitNoUpdat
              #print(paste("hit number is" , seqHitNoUpdat, sep=" "))               
              chrRes <- data.frame(chr=rep(chrName,length(res@ranges@start)), start=res@ranges@start, end= (res@ranges@start+res@ranges@width))
              finalRes <- rbind(finalRes, chrRes) 
              
            }
            countResSep <- data.frame(chr=chrName, hitno=seqHitNo)
            countRes <- rbind(countRes, countResSep)
            
            HitCombRes <- list(finalRes = finalRes, countRes=countRes)
          }        
          
          HitCombRes <- list(finalRes = finalRes, countRes=countRes)
          
        } else if (is.null(input$genome)) {
          HitCombRes <- list(finalRes = NULL, countRes = NULL)
        }
        
      })    
      #print(HitCombRes)
      
    })
  })
  
  output$tabRes <- DT::renderDataTable({
    input$submit
    isolate({
      if (is.null(input$genome)) return()
      HitAllRes()$finalRes
    })    
  }, 
  options = list(pageLength = 20, 
                 columnDefs = list(list(className = 'dt-center', targets = c(1,2,3)))),  
  colnames = c("Chr Name"=2, "Start"=3, "End"=4)
  )
  
  output$resDownload <- downloadHandler(  
    
    filename = function() {paste("NGG-Genome-Search-", Sys.Date(), ".txt", sep="")},
    content = function(file) {write.table(HitAllRes()$finalRes, 
                                          file, row.names=T, col.names=NA, quote=F, sep="\t"
    )},
    contentType = "text"
    
  )
  
  output$chrSummary <- renderTable({
    input$submit 
    isolate({
      if (is.null(input$genome) | is.null(input$chr)) return()
      countSummary <- HitAllRes()$countRes
      #print(countSummary)
      if (input$chr == "All") {
        totalCal <- sum(countSummary$hitno)
        totalCalRow <- data.frame(chr="Total", hitno=totalCal)
        orgRowName <- rownames(countSummary)
        countSummary<-rbind(countSummary, totalCalRow)
        #rownames(countSummary) <- c(orgRowName, "Total")
      }    
      colnames(countSummary) <- c("Chr Name", "No. of hit")
      
      countSummary
    })
  }, include.rownames=T, align="l|l|c", digits=0)
   
  output$countresDownload <- downloadHandler(  
    
    filename = function() {paste("NGG-Genome-Search-CountSummary", Sys.Date(), ".txt", sep="")},
    content = function(file) {write.table(HitAllRes()$countRes, 
                                          file, row.names=T, col.names=NA, quote=F, sep="\t"
    )},
    contentType = "text"
    
  )
  
  output$hitSearchPlot <- renderPlot({
    input$submit 
    isolate({
      #print("===")
      #print(input$chr)
      #print("===")
      if (is.null(input$genome) | is.null(input$chr)) return()
      else if (input$chr == "All") {
        data <- HitAllRes()$countRes
        g <- ggplot(data=data, aes(x=chr,y=hitno, fill=chr)) + geom_bar(stat="identity")
        g <- g + theme(axis.text.x=element_text(size=rel(2.5), angle=45, hjust=1)) 
        g <- g + theme(axis.title.x = element_blank()) 
        g <- g + theme(legend.position="none")
        g <- g + ylab("Hit No.") 
        g <- g + theme(axis.text.y=element_text(size=rel(3))) 
        g <- g + theme(axis.title.y=element_text(size=rel(3), vjust=1))
        g      
      } else {
        dataChrHitRes <- HitAllRes()$finalRes
        if (is.null(dataChrHitRes$start) ) return()
        else {
          par(mar=c(5,5,2,2))
          hist(dataChrHitRes$start, breaks = 100, xlab = as.character(input$chr), main="", cex.axis=1.5, cex.lab=1.5)
        }        
      }
    })
  })
  
  
})

