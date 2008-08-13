ldacv.gui<-function (dummy = NULL) 
{
    if (interactive()) {
        require(tcltk) || stop("TCLTK support is absent.")
        out <- NULL
        inFileName.var <- tclVar("")
	  inFileg.var<-tclVar("")		
        group<- NULL  
        titleFont <- "Helvetica 14"
        normalFont <- "Helvetica 10"

###read and load data

       finddata <- function() {
            tclvalue(inFileName.var) <- tclvalue(tkgetOpenFile())
        }

        readdata <- function() {
            flnm <- tclvalue(inFileName.var)
            if (flnm == "") {
                postMsg("ERROR: No file selected.\n")
            }
            else {
                postMsg("Reading data...")		     
                 my.data = as.matrix(read.table(flnm,header=T,row.names=1))
		     if(is.null(flnm)==FALSE){
				assign("x",my.data,inherits=TRUE)
				postMsg("done.\n")
			}	             
            }
        }

#####read and load group

        findg <- function() {
            tclvalue(inFileg.var) <- tclvalue(tkgetOpenFile())
        }
        readg <- function() {
            flnm <- tclvalue(inFileg.var)
            if (flnm == "") {
               postMsg("ERROR: No file selected.\n")
            }
            else {
               postMsg("Reading group...")
               my.group = as.factor(scan(flnm))
               if (is.null(my.group) == FALSE) {
               	   assign("group", my.group, inherits = TRUE)
                     postMsg("done.\n")
               }
            }
        }
##Output message

	  postMsg <- function(msg) {
            tkconfigure(message.txt, state = "normal")
            tkinsert(message.txt, "end", msg)
            tkconfigure(message.txt, state = "disabled")
        }

##Output error
        errorHandler <- function() {
            postMsg(paste("An R error has occurred: ", geterrmessage(), sep = ""))
        }

##### compute CV

       execute.fnc <- function() {
		library(MASS)
            if (is.null(x)){
			postMsg("ERROR: Data haven't been read yet.\n")
		 }
           else {               
                  if (is.null(group)){ 
                      postMsg("ERROR: membership haven't been read yet.\n")
		      }
                  else {
                          postMsg("Computing CV:")
		    	        gg<-nlevels(group)
		    	        nn<-NROW(x)
		              pp<-NCOL(x)
		              mis.clust<-NULL
                          for(i in 1:pp){
	              			mis.clusti<-NULL
	             			for(j in 1:nn){
		             			x.prin<-prcomp(x[-j,],scale=T)
					             group1<-group[-j]
		      			       score.pr<-as.matrix(x.prin$x[,1:i])	
					             my.data<-data.frame(cbind(score.pr,group1))		
		      			       my.lda<-lda(group1~.,prior=rep(1,gg)/gg,data=my.data)
					             newdatax<-predict(x.prin,x[c(j,j),])[1,1:i]
		      			       newdata<-rbind(my.data,c(newdatax,10^9))
					             pred.lda<-predict(my.lda,newdata[nn,])$class
		      			       mis.clusti[j]=as.numeric(pred.lda!=group[j])
				               }			
	            		      mis.clust[i]<-mean(mis.clusti)
			   			postMsg(paste("CV for component",i,"is", mis.clust[i],"\n",sep="\t"))	
		     		  }
		        }
		        if (is.null(mis.clust) == FALSE){
                            assign("out", mis.clust, inherits = TRUE)          
                            postMsg("done.\n")
                    }
           }     				    	             
    }

####Save Output
        saveOutput.fnc <- function() {
            if (is.null(out)){ 
                postMsg("ERROR: CV hasn't been computed yet.\n")
		}
            else{
                postMsg("Writing results to file...")
                flnm <- tclvalue(tkgetSaveFile())
                if (flnm != "") {
                      write.table(out, file= flnm)
                      postMsg("done.\n")
                }
             }
        }
####main frame

        options(error = errorHandler, show.error.messages = FALSE)
        base <- tktoplevel()
        tkwm.title(base, "lda.cv")
        top.frm <- tkframe(base, borderwidth = 2)

##### input data
        readdata.frm <- tkframe(top.frm, relief = "raised", bd = 2)
        tkpack(tklabel(readdata.frm, text = "Open:", font = titleFont), 
            anchor = "w")
        readdataInset.frm <- tkframe(readdata.frm, relief = "groove", 
            bd = 2)
        inFileName.frm <- tkframe(readdataInset.frm)
        inFileName.lbl <- tklabel(inFileName.frm, text = "File Name:", 
            font = normalFont)
        inFileName.ety <- tkentry(inFileName.frm, textvariable = inFileName.var, 
            font = normalFont, justify = "center")
        tkpack(inFileName.lbl, side = "left")
        tkpack(inFileName.ety, side = "right", fill = "x", expand = TRUE)
        tkpack(inFileName.frm, fill = "x", expand = TRUE)
        dButtons.frm <- tkframe(readdataInset.frm)
        dbrowse.btn <- tkbutton(dButtons.frm, text = "Browse", 
            font = normalFont, command = finddata)
        dload.btn <- tkbutton(dButtons.frm, text = "Load", font = normalFont, 
            command = readdata)
        tkgrid(dbrowse.btn, dload.btn)
        tkpack(dButtons.frm, anchor = "e")
        tkpack(readdataInset.frm, fill = "x")
 
########Input group
        group.frm <- tkframe(top.frm, relief = "raised", bd = 2)
        tkpack(tklabel(group.frm, text = "Input membership labels", 
            font = titleFont), anchor = "w")
        groupInset.frm <- tkframe(group.frm, relief = "groove", 
            bd = 2)
        ginFileName.frm <- tkframe(groupInset.frm)
        ginFileName.lbl <- tklabel(ginFileName.frm, text = "File Name:", 
            font = normalFont)
        ginFileName.ety <- tkentry(ginFileName.frm, textvariable = inFileg.var, 
            font = normalFont, justify = "center")
        tkpack(ginFileName.lbl, side = "left")
        tkpack(ginFileName.ety, side = "right", fill = "x", expand = TRUE)
        tkpack(ginFileName.frm, fill = "x", expand = TRUE)
        gButtons.frm <- tkframe(groupInset.frm)
        gbrowse.btn <- tkbutton(gButtons.frm, text = "Browse", 
            font = normalFont, command = findg)
        gload.btn <- tkbutton(gButtons.frm, text = "Load", font = normalFont, 
            command = readg)
        tkgrid(gbrowse.btn, gload.btn)
        tkpack(gButtons.frm, anchor = "e")
        tkpack(groupInset.frm, fill = "x")
        tkpack(groupInset.frm, fill = "x", expand = TRUE)

####Compute CV
        action.frm <- tkframe(top.frm, relief = "raised", bd = 2)
        tkpack(tklabel(action.frm, text = "Compute CV:", 
            font = titleFont), anchor = "w")
        actionInset.frm <- tkframe(action.frm, relief = "groove", 
            bd = 2)
        execute.btn <- tkbutton(actionInset.frm, text = "Execute", 
            font = normalFont, command = execute.fnc)
        saveOutput.btn <- tkbutton(actionInset.frm, text = "Save Output", 
            font = normalFont, command = saveOutput.fnc)
        tkpack(saveOutput.btn, side = "right", anchor = "e")
        tkpack(execute.btn, side = "right")
        tkpack(actionInset.frm, fill = "x", expand = TRUE)

######Message output

	  message.frm <- tkframe(top.frm, relief = "raised", bd = 2)
        message.txt <- tktext(message.frm, bg = "white", font = normalFont, 
            height = 5, width = 5)
        message.scr <- tkscrollbar(message.frm, command = function(...) tkyview(message.txt, 
            ...))
        tkconfigure(message.txt, yscrollcommand = function(...) tkset(message.scr, 
            ...))
        tkpack(message.txt, side = "left", fill = "x", expand = TRUE)
        tkpack(message.scr, side = "right", fill = "y")
	
    
        tkpack(readdata.frm, fill = "x")
        tkpack(group.frm, fill = "x")
	  tkpack(action.frm, fill="x")
	  tkpack(message.frm, fill="x")		
        tkpack(top.frm)
        tkwm.focusmodel(base, "active")
   }
}
