library(shiny)
library(data.table)
library(shinydashboard)
library(openxlsx)
library(DT)
library(ECharts2Shiny)

require(graphics)

if (interactive()) {
# Define UI for data upload app ----
ui = dashboardPage(
    dashboardHeader(title = "办公自动化"),
    dashboardSidebar(
		h4(textOutput("currentTime")),
		br(),
		fileInput("dat","上传excel文件",accept = ".xlsx"),
		br(),
		selectInput("vars", "列名", choices=names("varselect"))
    ),
    dashboardBody(
		fluidRow(
			box(
				title = "数据表", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,dataTableOutput("Data")
			)
		),
		fluidRow(
			box(
				title = "饼图", width = 6, solidHeader = TRUE, status = "success", collapsible = TRUE,plotOutput("Plot")
			),
			box(
				title = "信息", width = 6, solidHeader = TRUE, status = "success", collapsible = TRUE,verbatimTextOutput("tabset1Selected")
			)
		)
    )
)

server <- function(input, output, session) {

	output$currentTime <- renderText({
		invalidateLater(1000, session)
		paste("当前时间是", Sys.time())
	})

	tmp_file <- reactiveValues()
    d1 <- reactive({
		inFile1 <- input$dat
		if (is.null(inFile1)) return(NULL)
		dataFile <- read.xlsx(inFile1$datapath,sheet=1)
		tmp_file <- dataFile
		return(dataFile)
    })

	output$Data <- renderDataTable({
		datatable(d1())
	})
	
	output$varselect <- renderUI({})	
	observe({
		updateSelectInput(session, inputId="vars", label="列名",
                       choices=names(d1()), selected=names(d1()))}
	)


	tmp_name <- renderText({
		input$vars
	})
	

	output$Plot <- renderPlot({
		freq = table(d1()[[tmp_name()]])
		piepercent <- paste(row.names(freq),",",freq,",",round(100*freq/sum(freq),2),"%")
		pie(table(d1()[[tmp_name()]]),label=piepercent,col=terrain.colors(length(unique(table(d1()[[tmp_name()]])))))
	})
	
	output$tabset1Selected <- renderText({
		d1()[[tmp_name()]]
    })
}
shinyApp(ui = ui, server = server)

}