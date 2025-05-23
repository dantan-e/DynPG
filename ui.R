##############################################################
#
# Necessary packages  
#
##############################################################

# # check if necessary packages are installed and install if not
# list.of.packages <- c("shiny", "shinyalert","shinyjs", "JM", "shape")
# 
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# 
# # load packages
# lapply(list.of.packages, require, character.only = T)

library(shiny)
library(shinyjs)
library(JM)
library(shape)

##############################################################
#
# Languages  
#
##############################################################
countries <- c("(EN)", "(FR)")

flags <- c(
  "gb.svg",
  "fr.svg"
)


##############################################################
#
# Define UI for application 
#
##############################################################
shinyUI(fluidPage(	
	useShinyjs(),
	
	#--------
	# Title
	#--------
	fluidPage( # un fuildpage en plus pour centrer le titre
		titlePanel(
			fluidRow(
				img(src='logoRISCA.png', align = "left"),
				fluidRow(
				column(8,
					div(htmlOutput("titre", style = 'margin-top:30px;max-width:1000px;'))
				),
				
				column(
					1,
					div(
						radioButtons(
							"langue",
							"",
							choiceNames = mapply(countries, flags, FUN = function(country, flagUrl) {
								tagList(
									tags$img(src=flagUrl, width=20, height=15),
									country
								)
							}, SIMPLIFY = FALSE, USE.NAMES = FALSE),
							choiceValues = c("US", "FR"),
							selected = "US",
							inline = F
						),
						style = 'font-size:14px; margin-top:30px;'
					)
				)
				)
			), 
		"DynPG | Dynamic predictions in renal transplant patients")
	),
	
	
	
	
	
	#----------------------------------------
	# Framework for individual baseline data 
	#----------------------------------------
	column(4, #la somme des width tjrs = 12 
		wellPanel(#Premier bloc avec les paramètres de la greffe et du receveur 
			dateInput("Dgreffeb", "Date of transplantation", format = "dd/mm/yyyy"),
			#tags$label(br(),""), #pour espacer entre 2 entrées 
			radioButtons("rangGreffe", "Transplantation rank", choices = c(1,2), selected = NULL, inline = T),
			numericInput("AgeR_calc", "Recipient age at transplantation (years)", "", min=18, max=90),
			radioButtons("cardioVasc", "Cardiovascular history", choices = c("No"=0, "Yes"=1), selected = NULL, inline = T),
			radioButtons("antiClassI", "Anti-HLA immunization of class I", choices = c("No"=0, "Yes"=1), selected = NULL, inline = T),
			radioButtons("rejet1an", "Acute rejection in the first year post transplantation", choices = c("No"=0, "Yes"=1), selected = NULL, inline = TRUE),
			numericInput("creat_3m", "3-month serum creatinine (µmol/l)", "", min=10, max=2000)
		)
	), 
	
	
	#--------------------------------------------
	# Framework for individual longitudinal data 
	#--------------------------------------------
	column(8,#bloc pour les saisies du suivi et les résultats 
		wellPanel(
			id="topbar",
			fluidPage(
				tags$head(tags$style(HTML('#topbar .form-group {margin:0px;}'))),#0 espace des 4 cotés
				tags$head(tags$style(HTML('.selectize-control {margin-bottom:-5px;}'))),
				column(3,
					div(
						style="display:inline-grid;margin-top:10px;", 
						tags$label( br(), htmlOutput("V1")) )
				),
				column(9,
					div(
						style="display:inline-grid;",
						dateInput(inputId="Dsuivi_12m", label="Date", format = "dd/mm/yyyy", width=110)
					),
					div(
						style="display:inline-grid;",
						numericInput(inputId="creat_12m", label="Serum creatinine (µmol/l)", value="", min=20, max=2000, width=200)
					)#block;margin-left:40px rq inline-grid permet d'aligner date et valeur créat. avec block c etait décalé
				)
			),
			div(id="toto", br()),
			actionButton("ajouter", "Add a visit"),
			actionButton("supprimer", "Delete the last visit", style="display:none;")
		),		
		
		#---------------------------------------------------------
		# Panel (Graphs, Interpretation, Inclusion criteria, Refs) 
		#---------------------------------------------------------
		tabsetPanel(
			id = "tabset",
			
			
			tabPanel(
				htmlOutput("titreongletgraph"),
				br(),
				htmlOutput("control"),
				
				column(6,
				       style = "min-width:400px;padding:0px;",
				       div(style="margin-left:40px;",hidden(sliderInput("slider_graph","Visit",min=1,max=6, value=6, ticks=FALSE))),
				       
				plotOutput("graph",height=500)
				),
				column(6,
				       style = "min-width:400px;padding:0px;",
				       plotOutput("hist",height=400),
				       
				       column(12,
				              div(style="text-align: center;", hidden(imageOutput("ABCDE", height=75))),
				              htmlOutput("phraseABCDE", style='max-width:500px;'))
				              
				       
				)
				
				
				
				
				
				
			),
			
			
			tabPanel(value="interp",
			  htmlOutput("titreongletinterp"), 				
				htmlOutput("phrase", style='max-width:500px;'),
				fluidRow(
					id = "bonhomme",
					style='max-width:450px;margin-left:0px;display:inline-block;',
					div(style="display: inline-block;",imageOutput("bonhomme1", width=80, height=0)),
					div(style="display: inline-block;",imageOutput("bonhomme2", width=80, height=0)),
					div(style="display: inline-block;",imageOutput("bonhomme3", width=80, height=0)),
					div(style="display: inline-block;",imageOutput("bonhomme4", width=80, height=0)),
					div(style="display: inline-block;",imageOutput("bonhomme5", width=80, height=0)),
					div(style="display: inline-block;",imageOutput("bonhomme6", width=80, height=0)),
					div(style="display: inline-block;",imageOutput("bonhomme7", width=80, height=0)),
					div(style="display: inline-block;",imageOutput("bonhomme8", width=80, height=0)),
					div(style="display: inline-block;",imageOutput("bonhomme9", width=80, height=0)),
					div(style="display: inline-block;",imageOutput("bonhomme10", width=80, height=0))
				),
				htmlOutput("definition", style='max-width:800px;')
			),
			
			
			tabPanel(value="InclusionCriteria",
			         htmlOutput("titreongletinfo"),
			          br(),
			          htmlOutput("desc_DynPG"),
			          br(),
			          htmlOutput("crit_inc"),
			          br(),
			          htmlOutput("desc_DynPG_suite"), 
			          br()
			          # htmlOutput("crit_inc")  
			         ),
			
			
			tabPanel(value="Refs",
			         htmlOutput("titreongletref"),
			         h5("Lenain et al. External Validation of the DynPG for Kidney Transplant Recipients.", a("Transplantation. 2020", href="https://journals.lww.com/transplantjournal/Abstract/9000/External_Validation_of_the_DynPG_for_Kidney.95727.aspx", target="_blank")),
			         h5("Asar et al. Dynamic predictions of kidney graft survival in the presence of longitudinal outliers.", a("Stat Methods Med Res. 2020", href="https://journals-sagepub-com.proxy.insermbiblio.inist.fr/doi/10.1177/0962280220945352?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%20%200pubmed", target="_blank")),
			         h5("Fournier et al. Dynamic predictions of long-term kidney graft failure: an information tool promoting patient-centred care.", a("Nephrol Dial Transplant. 2019", href="https://doi.org/10.1093/ndt/gfz027", target="_blank")),
			         h5("Fournier et al. A joint model for longitudinal and time-to-event data to better assess the specific role of donor and recipient factors on long-term kidney transplantation outcomes.", a("Eur J Epidemiol. 2016", href="https://link.springer.com/article/10.1007/s10654-016-0121-2", target="_blank"))
			)
		)
		
		
		
	),
	
	
	#--------------------
	# Popup window
	#--------------------

	
))
##############################################################
#
# End   
#
##############################################################

