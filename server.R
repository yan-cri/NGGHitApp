## This App is developed by Yan Li for requested project (CRI-BIO-300), last update on August, 2015
library(BSgenome.Hsapiens.UCSC.hg19)
library(BSgenome.Mmusculus.UCSC.mm10)
library(ggplot2)
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
    paste("The NGG search is on the ", as.character(input$chr), " of ", as.character(input$genome), " genome.", sep="")
  })
  
  
  HitAllRes <- reactive({
    seq <- input$seq
    seqAGG <- paste(input$seq, "AGG", sep="")
    seqTGG <- paste(input$seq, "TGG", sep="")
    seqGGG <- paste(input$seq, "GGG", sep="")
    seqCGG <- paste(input$seq, "CGG", sep="")
    seqNGG <- c(seqAGG, seqTGG, seqGGG, seqCGG)

    #if(is.null(input$genome)) return("hg19")
    #if(is.null(input$chr)) return("all")
    print(input$genome)
    print(as.numeric(input$mismatchNo))
    print("*********")
    if(input$genome == "hg19") {
      finalRes <- data.frame(A= numeric(0), B= numeric(0), C= numeric(0))
      countRes <- data.frame(A= numeric(0), B= numeric(0))
      count <- 0 
      for (i in 1:2) {
        seqHitNo <- 0
        for (j in 1:length(seqNGG)){
          seq <- seqNGG[j]
          seqnames <- seqnames(hg19genome)          
          chrName <- seqnames[i]
          subject <- hg19genome[[chrName]]
          res <- matchPattern(seq, subject, max.mismatch=as.numeric(input$mismatchNo) )
          seqHitNoUpdat <- length(res@ranges@start)
          seqHitNo <- seqHitNo + seqHitNoUpdat
          print(seqHitNoUpdat)
          count <- count + 1
          print(count)         
          chrRes <- data.frame(chr=rep(chrName,length(res@ranges@start)), start=res@ranges@start, end= (res@ranges@start+res@ranges@width))
          finalRes <- rbind(finalRes, chrRes)          
        }
        print(seqHitNo)
        countResSep <- data.frame(chr=chrName, hitno=seqHitNo)
        countRes <- rbind(countRes, countResSep)
      }
      HitCombRes <- list(finalRes = finalRes, countRes=countRes)
      
    } else if (input$genome == "mm10") {
      
      finalRes <- data.frame(A= numeric(0), B= numeric(0), C= numeric(0))
      countRes <- data.frame(A= numeric(0), B= numeric(0))
      
      for (i in 1:4) {
        seqHitNo <- 0
        for (j in 1:length(seqNGG)) {
          seq <- seqNGG[j]
          seqnames <- seqnames(mm10genome)
          
          chrName <- seqnames[i]
          subject <- hg19genome[[chrName]]
          res <- matchPattern(seq, subject, max.mismatch=as.numeric(input$mismatchNo) )
          seqHitNoUpdat <- length(res@ranges@start)
          seqHitNo <- seqHitNo + seqHitNoUpdat
          print(seqHitNoUpdat)         
          
          chrRes <- data.frame(chr=rep(chrName,length(res@ranges@start)), start=res@ranges@start, end= (res@ranges@start+res@ranges@width))
          finalRes <- rbind(finalRes, chrRes)          
        }
        print(seqHitNo)
        countResSep <- data.frame(chr=chrName, hitno=seqHitNo)
        countRes <- rbind(countRes, countResSep)
      }        
      
      HitCombRes <- list(finalRes = finalRes, countRes=countRes)
      
    } else if (is.null(input$genome)) {
      HitCombRes <- list(finalRes = NULL, countRes = NULL)
    }
    
  })
  
  output$tabRes <- DT::renderDataTable({
    input$submit | input$chrSubmit
    isolate({
      if (is.null(input$genome)) return()
      HitAllRes()$finalRes
    })    
  })
  
  output$chrSummary <- renderTable({
    input$submit | input$chrSubmit
    isolate({
      if (is.null(input$genome) | is.null(input$chr)) return()
      HitAllRes()$countRes
    })
  })
  
  output$hitSearchPlot <- renderPlot({
    input$submit | input$chrSubmit
    isolate({
      print("===")
      print(input$chr)
      print("===")
      if (is.null(input$genome) | is.null(input$chr)) return()
      else if (input$chr == "All") {
        data <- HitAllRes()$countRes
        g <- ggplot(data=data, aes(x=chr,y=hitno, fill=chr)) + geom_bar(stat="identity")
        g <- g + theme(axis.text.x=element_text(size=rel(3.5), angle=45, hjust=1)) 
        g <- g + theme(axis.title.x = element_blank()) 
        g <- g + theme(legend.position="none")
        g <- g + ylab("Hit No.") 
        g <- g + theme(axis.text.y=element_text(size=rel(3))) 
        g <- g + theme(axis.title.y=element_text(size=rel(3), vjust=1))
        g      
      } else {
        dataChrHitRes <- subset(HitAllRes()$finalRes, chr==as.character(input$chr))
        par(mar=c(5,5,2,2))
        hist(dataChrHitRes$start, breaks = 100, xlab = as.character(input$chr), main="", cex.axis=1.5, cex.lab=1.5)
        
      }
    })
  })
  
})

