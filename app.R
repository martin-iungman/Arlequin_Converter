packages<-c("shiny", "tidyverse","readxl","writexl","shinyjs","crayon")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

bold = function(x) strong(x, .noWS = "outside")
ital = function(x) em(x, .noWS = "outside")
link = function(s, href = s) a(s, href = href, .noWS = "outside")

ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  
  tags$head(
    tags$style(type = "text/css", "
      .body {font-size: small}
      .well {padding-top: 10px;}
      .selectize-dropdown {width: 250px !important;}
      .fa-check { font-size:xx-large; color:Lime}
      
  ")),
  
  # Application title
  h2(id = "title-h2", "Arlequin Converter"),
  tags$style(HTML("#title-h2 {background-color: gray; color: white; padding: 15px}")),
  
  p(bold("Objetivo: "), "Transformar automaticamente una base de datos para su incorporacion al software Arlequin"),
  p(bold("Ingresar una tabla con los haplotipos con las siguientes aracteristicas:  ")),
  
  tableOutput("tabla_ejemplo"),

  p("    - Las microvariantes se pueden introducir como ',' o como '.'"),
  p("    - La ausencia de informacion respecto a un marcador debe quedar vacio"),
  p("    - El programa acepta archivos .tsv, .csv, .txt y .xls o .xlsx (Excel)"),
 
  p(bold("El programa va a modificar la tabla para que sea compatible con el formato requerido por Arlequin:")),
  p("    - Reemplazara los casilleros vacios por '?'"),
  p("    - Desdoblara la informaon de ambos a alelos del marcador, llevandolo a dos distintas filas para cada muestra"),
  p("    - A todas las muestras se les asigna un '1' indicando que se refiere a una sola de ellas; Si hay dos muestras con mismo haplotipo se representaran dos veces"),
  p("    - Se puede optar por mantener el nombre orginal de las muestras, o generares un codigo para que resulten anonimas"),  
   
    fileInput("file", "Suba un archivo", accept = c(".xlsx",".xls",".txt",".tsv",".csv")),
    selectInput("anonimate", "Modificar ID de muestras?", choices = c("ORIGINAL", "ANONIMO")),
    tableOutput("tabla"),
    downloadButton("download")
)

server <- function(input, output) {
  
  output$tabla_ejemplo<-renderTable(read_xlsx("tabla_ejemplo.xlsx")%>%head(n=3))
  
  to_arlequin<-reactive({
    req(input$file)
    if(grepl("(txt|tsv)$",input$file$datapath)) {df<-read_tsv(input$file$datapath)} else 
      if(grepl("xlsx$",input$file$datapath)) {df<-read_xlsx(input$file$datapath)} else
        if (grepl("xls$",input$file$datapath)) {df<-read_xls(input$file$datapath)} else
          if (grepl("csv$", input$file$datapath)){df<-read_csv<-read_csv(input$fle$datapath)} 
    
    if(input$anonimate=="ANONIMO"){df[,1]=paste0("sample",1000:(999+nrow(df)))}
    df<-rename(df,sample_id=`Sample id` )
    df1<-df%>%select(sample_id, ends_with("1"))
    df2<-df%>%select(sample_id, ends_with("2"))
    names(df1)<-names(df2)<-c("sample_id",names(df1)[-1]%>%str_extract(pattern = ".+(?=\\s[:digit:])")%>%unique())
    df_tot<-rbind(df1,df2)
    df_tot<-df_tot%>%arrange(sample_id)
    df_tot<-df_tot%>%mutate(val=1)
    df_tot<-df_tot%>%mutate(across(cols=evenrything(), .fns=~as.character(.x)))
    df_tot[is.na(df_tot)]<-"?"
    df_tot=df_tot[,c(1,ncol(df_tot),seq(2,ncol(df_tot)-1))]
    df_tot<-lapply(df_tot, function(x) gsub("\\.|\\,", "", x))
    df_tot<-as_tibble(df_tot)
    names(df_tot)[2]=""
    df_tot[seq(2,nrow(df_tot), by=2),c(1,2)]<-""
    print(head(df_tot))
    return(df_tot)
  })
  
  output$tabla<-renderTable(head(to_arlequin()))
 
  output$download <- downloadHandler(
    filename = function() {
      paste0(str_replace(input$file, "\\..*",""),"_", "arlequin.xlsx")
    },
    content = function(file) {
      write_xlsx(to_arlequin(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
