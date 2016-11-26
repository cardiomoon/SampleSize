## numericInput3 : numericInputs side by side
numericInput3<-function (inputId, label, value, min=NA,max=NA,step=NA,width=100,...) 
{
  div(style="display:inline-block;",
      tags$label(label, `for` = inputId,class="control-label"), 
      tags$input(id = inputId, type = "number", class="form-control",
                 value = value, min=min,max=max,step=step,style=paste("width: ",width,"px;",sep=""),...)
  )
}

## numericInput : input and label side-by-side
numericInput4<-function (inputId, label, value, min=NA,max=NA,step=NA,width=80,labelwidth=180,...) 
{
    div(class="form-group shiny-input-container",
        
        tags$label(label, `for` = inputId,class="control-label",
                   style=paste0("display:inline-block;width:",labelwidth,"px;")), 
        tags$input(id = inputId, type = "number", class="form-control",style="display:inline-block;",
                   value = value, min=min,max=max,step=step,style=paste("width: ",width,"px;",sep=""),...)
        
    )
}
