##############################################################
#
# Chargement de RData, fonction nécessaire, data  
#
##############################################################
load("JMsimplified.RData")	

source("plot.survfitJM-modiffctok.r", encoding="UTF-8")

app.id <<- read.table("app.idEJE.csv", sep=";",head=T,dec = ".",na.strings = "NA")

pi.learning <<- read.table("pi_learning.csv", sep=";",head=T,dec = ".",na.strings = "NA")



##############################################################
#
# Texte en français/anglais --> Gestion de la langue
#
##############################################################
translation <- list(
	"titre" = list(
	  "US" = "DynPG: Dynamic predictions of Patient and Graft survival in renal transplant patients", 
	  "FR" = "DynPG: Prédictions dynamiques de la survie à un échec de greffe rénale"
	),
	"desc_DynPG" = list(
	  "US" = "The DynPG is a dynamic predictive score of graft failure (defined as the first event between return to dialysis and death). The predictions are said to be dynamic because they are updated throughout the follow-up according to the evolution of creatinine.", 
	  "FR" = "Le DynPG est un score prédictif dynamique de l'échec de greffe (défini comme le premier événement entre le retour en dialyse et le décès). Les prédictions sont dites dynamiques car elles peuvent être mises à jour tout au long du suivi en fonction de l'évolution de la créatinine."
	),
	
	"desc_DynPG_suite" = list(
	  "US" = "Please follow corresponding references for more details.", 
	  "FR" = "Merci de vous référer aux références associées pour plus de détails."
	),
	
	"confirm" = list(	
	  "US" = "I  confirm",
	  "FR" = "Je confirme"
	),
	
	"date_greffe" = list(	
		"US" = "Date of transplantation", 
		"FR" = "Date de greffe"
	),
	"rang_greffe" = list(	
		"US" = "Transplantation rank", 
		"FR" = "Rang de la greffe"
	),
	"age" = list(	
		"US" = "Recipient age at transplantation (years)", 
		"FR" = "Age du receveur à la transplantation (années)"
	),
	"cardiovasc" = list(	
		"US" = "Cardiovascular history", 
		"FR" = "Antécédent cardiovasculaire"
	),
	"antiClassI" = list(	
		"US" = "Anti-HLA immunization of class I", 
		"FR" = "Immunisation anti-HLA de classe I"
	),
	"rejet" = list(	
		"US" = "Acute rejection in the first year post transplantation", 
		"FR" = "Rejet aigu sur la première année de greffe"
	),
	"creat3m" = list(	
		"US" = "3-month serum creatinine (µmol/l)", 
		"FR" = "Créatininémie à 3 mois (µmol/l)"
	),
	"creat12m" = list(	
		"US" = "Serum creatinine (µmol/l)", 
		"FR" = "Créatininémie (µmol/l)"
	),
	"visite12m" = list(	
		"US" = "12-months post transplantation visit", 
		"FR" = "Visite à 12 mois post-transplantation"
	),
	"completer_champs" = list(	
		"US" = "Please complete all fields",
		"FR" = "Veuillez compléter tous les champs"
	),
	"age_valide" = list(	
		"US" = "Recipient age must be comprised between 18 and 90 years",
		"FR" = "L'age du receveur doit Ãªtre compris entre 18 et 90 ans"
	),
	"creat_valide" = list(	
		"US" = "Serum creatinine values must be comprised between 10 and 2000 µmol/L",
		"FR" = "Les valeurs de créatininémies doivent Ãªtre comprises entre 10 et 2000 µmol/L"
	),
	"dsuivi_valide" = list(	
		"US" = "The one year post-transplant visit is invalid (it must take place at 1 year post-transplantation ± 4 months)",
		"FR" = "La date de la visite à un an est invalide (elle doit avoir lieu à 1 an post-transplantation ± 4 mois)"
	),
	"suivitroplong" = list(	
		"US" = "The provided follow-up is too long. The predictions are not yet validated for time of follow-up longer than 6 years post transplantation.",
		"FR" = "Le suivi renseigné est trop long. Nos prédictions ne sont pas encore validées pour des temps de suivi supérieurs à 6 ans post-transplantation."
	),
	"ajouter_visite" = list(	
		"US" = "Add a visit",
		"FR" = "Ajouter une visite"
	),
	"Show_button" = list(	
	  "US" = "Show inclusion criteria",
	  "FR" = "Rappel des critères d'inclusion"
	),
	"supp_visite" = list(	
		"US" = "Delete the last visit",
		"FR" = "Supprimer la dernière visite"
	),
	"visite" = list(	
		"US" = "Visit",
		"FR" = "Visite"
	),
	"info" = list(	
		"US" = "Important information",
		"FR" = "Informations importantes"
	),
	"crit_inc" = list(	
		"US" = "<br>Let recall that these predictions are <u>only applicable for</u>: <br>- <strong>adult</strong> recipients,<br>- transplanted after <strong>2000</strong>, <br>- only <strong>renal</strong> graft, <br>- for the <strong>first or second time</strong>, <br>- maintained under <strong>Tacrolimus</strong> and <strong>Mycofenolate</strong>, <br>- from a <strong>living or heart beating deceased donor</strong>;<br>- <strong>alive with a functioning graft at one year post-transplantation</strong>. <br> All of these works were done in a <strong>French</strong> population. <br><br>",
		"FR" = "<br>Il est important de rappeler que ces prédictions sont <u> uniquement valables chez</u>: <br>- des receveurs <strong>adultes,</strong><br>- transplantés après <strong>2000</strong>, <br>- d'un <strong>rein</strong> seul, <br>- pour la <strong>première ou seconde</strong> fois, <br>- prenant comme traitements du <strong>Tacrolimus</strong> et <strong>Mycofenolate</strong>, <br>- à partir d'un <strong>donneur vivant ou décédé à coeur battant</strong>; <br>- <strong>vivant avec un greffon fonctionnel à un an post-transplantation</strong>. <br> Tous ces travaux ont été réalisés chez des patients <strong>français</strong>. <br><br>"
	),
	"loading" = list(
		"US" = "Loading... Please wait",
		"FR" = "Chargement... Veuillez patienter"
	),
	"erreur" = list(	
		"US" = "Please fill in all fields", 
		"FR" = "Veuillez remplir tous les champs"
	),
	"graph" = list(	
		"US" = "Graph", 
		"FR" = "Graphique"
	),
	"def_gf" = list(	
		"US" = "Graft failure was defined as the first event among return to dialysis and death.", 
		"FR" = "L'échec de greffe est défini comme le premier événement entre le retour en dialyse et le décès."
	),
	"interp" = list(	
	  "US" = "Interpretation", 
	  "FR" = "Interprétation"
	),
	"info" = list(	
	  "US" = "Important informations",
	  "FR" = "Informations importantes"
	),
	"ref" = list(	
	  "US" = "References",
	  "FR" = "Références"
	),
	"chargement" = list(	
		"US" = "Loading...", 
		"FR" = "Chargement..."
	),
	"effectif" = list(	
	  "US" = "Frequency", 
	  "FR" = "Effectif"
	),
	"Xlab.hist" = list(	
	  "US" = "Patient-graft survival probability", 
	  "FR" = "Probabilité de survie patient-greffon"
	),
	"Title.hist" = list(	
	  "US" = "Distribution for patients at risk at the prediction time (learning data) ", 
	  "FR" = "Distribution pour les patients à risque au temps de prédiction (learning data) "
	),
	"confirm_sentence" = list(	
	  "US" = "Please confirm that you have read and understand the inclusion criteria allowing the use of the DynPG ",
	  "FR" = "Confirmez-vous avoir pris connaissance des critères d'inclusion des patients pour lesquels le calcul du DynPG est envisageable"
	)
)




##############################################################
#
# Shinyserver 
#
##############################################################
shinyServer(function(input, output, session) {

  
# ce qui est doit etre mis en commentaire pour enlever l'exemple pre-defini
  # observe({
  #   updateDateInput(session,inputId="Dgreffeb",
  #                   value="2010-01-01")
  # 
  #   updateRadioButtons(session,
  #                      inputId="rangGreffe",
  #                      selected=1)
  #   updateNumericInput(session,
  #                      inputId="AgeR_calc",
  #                      value=50)
  #   updateRadioButtons(session,
  #                      inputId="cardioVasc",
  #                      selected=1)
  #   updateRadioButtons(session,
  #                      inputId="antiClassI",
  #                      selected=1)
  #   updateRadioButtons(session,
  #                      inputId="rejet1an",
  #                      selected=1)
  #   updateNumericInput(session,
  #                      inputId="creat_3m",
  #                      value=120)
  #   updateNumericInput(session,
  #                      inputId="creat_12m",
  #                      value=130)
  # 
  #   updateDateInput(session,inputId="Dsuivi_12m",
  #                   value="2011-01-01")
  # 
  #   })
  

  
  
  
	tr <- function(text){ # translates text into current language
		sapply(text, function(s) translation[[s]][[input$langue]], USE.NAMES=FALSE)
	}

	
	rv <- reactiveValues() 

	rv$nb_suivi <- 0	
	
	output$titre <- renderText({
		tr("titre")
	})
	
	output$desc_DynPG <- renderText({
		tr("desc_DynPG")
	})
	
	output$desc_DynPG_suite <- renderText({
	  tr("desc_DynPG_suite")
	})
	
    #----------------------------------
	# Popup Window critères d'inclusion
	#----------------------------------
	observeEvent(input$Show,{
		 # translation$shorttitre$FR
		 shinyalert(
		   title = "",
		   size = "m",
		   html = TRUE,
		   showConfirmButton = TRUE,
		   closeOnEsc = TRUE,
		   closeOnClickOutside = FALSE,
		   text=   p(h1("Important message",style='text-align:left;'),
					 br(),
					 h4("Please confirm that ")
					 ),
		   confirmButtonText = p(textOutput("confirm"))		   
		)
	})
	
	
	## Gestion du multilangue ##
	observeEvent(input$langue, {
	
		 updateDateInput(session,inputId="Dgreffeb", 
						 label = tr("date_greffe"))						 
		 updateRadioButtons(session, 
						   inputId="rangGreffe", 
						   label=tr("rang_greffe"))
		 updateNumericInput(session, 
							inputId="AgeR_calc", 
							label=tr("age"))		 
		 updateRadioButtons(session, 
							inputId="cardioVasc", 
							label=tr("cardiovasc"))		 
		 updateRadioButtons(session, 
							inputId="antiClassI", 
							label=tr("antiClassI"))
		 updateRadioButtons(session, 
							inputId="rejet1an", 
							label=tr("rejet"))
		 updateNumericInput(session, 
							inputId="creat_3m", 
							label=tr("creat3m"))		 
		 updateNumericInput(session, 
							inputId="creat_12m", 
							label=tr("creat12m"))
		
		 #param suivi
		 updateActionButton(session, 
							inputId="ajouter", 
							label=tr("ajouter_visite"))
		 updateActionButton(session, 
		                    inputId="Show", 
		                    label=tr("Show_button"))
		 updateActionButton(session, 
							inputId="supprimer", 
							label=tr("supp_visite"))
		 updateSliderInput(session, 
							inputId="slider_graph", 
							label=tr("visite"))	 
		 
		 
		 if(input$langue == "US"){
		   updateRadioButtons(session, 
		                      inputId="cardioVasc", 
		                      label=tr("cardiovasc"),
		                      choices = c("Non"=0, "Oui"=1),inline = TRUE)						   
		   updateRadioButtons(session, 
		                      inputId="antiClassI", 
		                      label=tr("antiClassI"),
		                      choices = c("Non"=0, "Oui"=1),inline = TRUE)
		   updateRadioButtons(session, 
		                      inputId="rejet1an", 
		                      label=tr("rejet"),
		                      choices = c("Non"=0, "Oui"=1),inline = TRUE)
		 }else{
		   updateRadioButtons(session, 
		                      inputId="cardioVasc", 
		                      label=tr("cardiovasc"),
		                      choices = c("No"=0, "Yes"=1),inline = TRUE)						   
		   updateRadioButtons(session, 
		                      inputId="antiClassI", 
		                      label=tr("antiClassI"),
		                      choices = c("No"=0, "Yes"=1),inline = TRUE)
		   updateRadioButtons(session, 
		                      inputId="rejet1an", 
		                      label=tr("rejet"),
		                      choices = c("No"=0, "Yes"=1),inline = TRUE)
		 }
		
	})

	output$V1 <- renderText({
		paste0("<p style='height:25px;padding-top:0px;'>", tr("visite12m"), "</p>")
	})
	
	output$titreongletgraph<- renderText({
		tr("graph")
	})
	
	output$titreongletinterp<- renderText({
	  tr("interp")
	})
	
	output$titreongletref<- renderText({
		tr("ref")
	})
	
	output$titreongletinfo<- renderText({
	  tr("info")
	})
	
	output$crit_inc<- renderText({
		tr("crit_inc")
	})
	
	output$confirm_sentence<- renderText({
	  tr("confirm_sentence")
	})
	
	output$confirm<- renderText({
	  tr("confirm")
	})
	
	
	
  ##---------------------------------------------------------------##
  ##  Récupération des paramètres passés dans l'URL (s'il y en a)
  ##---------------------------------------------------------------##
	observe({
		rv$query <- parseQueryString(session$clientData$url_search)
		
		isolate({
		
			if (!is.null(rv$query[['nb_suivi']])) {
			
				rv$nb_suivi <- as.numeric(rv$query[['nb_suivi']])
				
				for(i in 1:rv$query[['nb_suivi']]){
					insertUI(
						selector = '#toto',
						where = "beforeBegin",
						ui = fluidPage(
							id = paste0("fluidpage_", i),
							tags$head(tags$style(HTML('#VarLong .form-group {margin:0px;}'))),#0 espace des 4 cotés
							tags$head(tags$style(HTML('.selectize-control {margin-bottom:-5px;}'))),
							column(3,
								div(
									style="display:inline-grid;margin-top:10px;font-size:14px;font-weight: 700;",
									tags$label(htmlOutput(paste0("V",i+1)))
								)
							),
							column(9,
								div(
									style="display:inline-block;margin-left:0px;vertical-align:top;margin-top:-15px;",
									dateInput(inputId=paste0("Dsuivi_", i), 
											label="",  
											format = "dd/mm/yyyy", 
											value=rv$query[[paste0("Dsuivi_", i)]], 
											width=110)
								),
								div(
									style="display:inline-block;margin-left:0px;vertical-align:top;margin-top:-15px;",
									numericInput(inputId=paste0("creat_", i), 
									label="", 
									value=rv$query[[paste0("creat_", i)]], min=20, max=2000, width=200)
								)
							)
						)
					);
					
					output[[paste0("V", (i+1))]] <- renderText({
						num_visite <- substring(getCurrentOutputInfo(session = getDefaultReactiveDomain()), 2, last = 1000000L)
						paste0( tr("visite"), " ", num_visite)
					})
				}
				
				if(rv$nb_suivi > 0){
					show("supprimer")
				}
			
			}
		
			for (i in 1:(length(reactiveValuesToList(input)))) {
				nameval = names(reactiveValuesToList(input)[i]) # nom des reactives values de l'ui
				valuetoupdate = rv$query[[nameval]]
				if (!is.null(rv$query[[nameval]])) {#toutes les var Ã  choix multiples (radioButtons)
					if(
					  nameval == "cardioVasc" | 
						nameval == "antiClassI" | 
						nameval == "rejet1an"	  |
						nameval == "rangGreffe"
					){
							updateRadioButtons(session, nameval, selected = valuetoupdate)
					}else{
						if (is.na(as.numeric(valuetoupdate))) {
						  updateTextInput(session, nameval, value = valuetoupdate)
						}
						else {
						  updateTextInput(session, nameval, value = as.numeric(valuetoupdate))
						}
					}
				}
			}
			
			if (!is.null(rv$query[['sidebar']])) {#si sidebar est défini dans l'url
				if(rv$query[['sidebar']] == "hide"){#on regarde si on a demandé Ã  affiché sidebar ou non (ds url sidebar=T ou F)
					shinyjs::hideElement(id = "sidebar")
					shinyjs::hideElement(id = "topbar")
				}
				show("window")
			}else{	#si sidebar n'est pas mis dans l'url, on affiche tout
				show("window")
			}
		
		})
    })
	
	## Gestion du bouton ajouter une visite : au clic ajoute 2 champs supplémentaires date et créat ##
	observeEvent(input$ajouter, {	
	
		rv$validform <- FALSE
		rv$nb_suivi <- rv$nb_suivi + 1 
		
		insertUI(
			selector = '#toto',
			where = "beforeBegin",
			ui = fluidPage(
				id = paste0("fluidpage_",rv$nb_suivi),
				tags$head(tags$style(HTML('#topbar .form-group {margin:0px;}'))),#0 espace des 4 cotés
				tags$head(tags$style(HTML('.selectize-control {margin-bottom:-5px;}'))),
				column(3,
					div(
						style="display:inline-grid;margin-top:10px;font-size:14px;font-weight: 700;",
						tags$label(htmlOutput(paste0("V",rv$nb_suivi+1)))
					)  
				),
				column(9,
					div(
						style="display:inline-block;margin-left:0px;vertical-align:top;margin-top:-15px;",
						dateInput(inputId=paste0("Dsuivi_", rv$nb_suivi), label="",  format = "dd/mm/yyyy", value="", width=110)
					),
					div(
						style="display:inline-block;margin-left:0px;vertical-align:top;margin-top:-15px;",
						numericInput(inputId=paste0("creat_", rv$nb_suivi), label="", value="", min=20, max=2000, width=200)
					)
				)
			)
		);
		
		output[[paste0("V", (rv$nb_suivi+1))]] <- renderText({
			num_visite <- substring(getCurrentOutputInfo(session = getDefaultReactiveDomain()), 2, last = 1000000L)
			paste0( tr("visite"), " ", num_visite)
		})
	
		if(rv$nb_suivi==1){
		  show("supprimer")
		}
	})

		
	## Gestion du bouton supprimer la derniere visite : au clic supprimer 2 champs date et créat ##
	observeEvent(input$supprimer, {	
	  removeUI(selector = paste0('#creat_', rv$nb_suivi),immediate=TRUE, multiple=TRUE)
	  removeUI(selector = paste0("#Dsuivi_", rv$nb_suivi), immediate=TRUE, multiple=TRUE)
	  removeUI(selector = paste0("#Visite", rv$nb_suivi), immediate=TRUE, multiple=TRUE)
	  removeUI(selector = paste0('#fluidpage_', rv$nb_suivi),immediate=TRUE, multiple=TRUE)
	  if(rv$nb_suivi==1){
	    hide("supprimer")
	  }
	  
	  rv$nb_suivi <- rv$nb_suivi - 1 
	  #+1 car j ai fait -1 au dessus 
	  #probleme : je n'arrive pas à supprimer le texte mis dans un div() : paste0("Visite ", rv$nb_suivi+1) 
	})
	
	
	

	
	## Contrôle de la validité du formulaire ##
	# A la fin si le formulaire est valide rv$validform prend TRUE #
	observe({
		if( !isTruthy(input$Dgreffeb) | 
			is.na(input$rangGreffe) | 
			is.na(input$AgeR_calc) | 
			is.null(input$cardioVasc) | 
			is.null(input$antiClassI) | 
			is.null(input$rejet1an) | 
			is.na(input$creat_3m) | 
			!isTruthy(input$Dsuivi_12m) | 
			is.na(input$creat_12m)  
		){
			rv$validform <- FALSE
		}else{
            #vérif des valeurs (min max ne fonctionne pas si la pers saisi la valeur)
		    #je fais en 2 temps sinon erreur car les var quanti ne sont pas initialisées
            # elles sont vides Ã  l'ouverture donc les tests ci dessous ne peuvent Ãªtre fait	
			if(
				input$AgeR_calc<18 | input$AgeR_calc>90 | 
				input$creat_3m<10 | input$creat_3m >2000 |
				input$creat_12m<10 | input$creat_12m >2000 | 
				input$Dsuivi_12m - input$Dgreffeb > (365 + 4*30) | 
				input$Dsuivi_12m - input$Dgreffeb < (365 - 4*30)
			 ){
				rv$validform <- FALSE
			 }else{
				temp <- TRUE
				temp2<-FALSE
				temp3<-FALSE
				if(rv$nb_suivi != 0){
					for(i in 1:rv$nb_suivi){					
						if(!isTruthy(input[[paste0("Dsuivi_", i)]])){
							temp <- FALSE
						}else{
							if( (input[[paste0("Dsuivi_", i)]] - input$Dsuivi_12m) > (5*365+4*30.5) ){#on ne doit pas afficher les prédictions si le temps de landmark est > 5 ans post 1 an post transplantation avec 4 mois de marge comme pour la visite des un an  
								temp <- FALSE
								temp2 <- TRUE
							}
						}
						if(!isTruthy(input[[paste0("creat_", i)]])){
							temp <- FALSE 
						}else{
							if(input[[paste0("creat_", i)]] > 2000 | input[[paste0("creat_", i)]] < 10){
								temp <- FALSE 
								temp3<-TRUE
							}
						}
					}				
				}
				rv$validform <- temp
				rv$suivitroplong<-temp2
				rv$creatinvalide<-temp3
			}
		}
	})

	# Texte affiché en cas de formulaire non valide
	output$control <- renderText({
		if( rv$validform == FALSE ){
			if(
			  !isTruthy(input$Dgreffeb) | 
			  is.na(input$rangGreffe) | 
			  is.na(input$AgeR_calc) | 
			  is.null(input$cardioVasc) | 
			  is.null(input$rejet1an) | 
			  is.na(input$creat_3m) | 
			  !isTruthy(input$Dsuivi_12m) | 
			  is.na(input$creat_12m)
			){
				paste0(
					"<p style='font-size:18px;'>",
					tr("erreur"),
					"</p>"
				)
			}else{	
				if(input$AgeR_calc<18 | input$AgeR_calc>90){
					paste0("<p style='font-size:18px;color:red;'>", 
							tr("age_valide"), 
							"</p>"
					)
				}else{
					if(input$creat_3m<10 | input$creat_3m >2000 |
						input$creat_12m<10 | input$creat_12m >2000 ){
					  paste0("<p style='font-size:18px;color:red;'>", 
					         tr("creat_valide"), 
					         "</p>"
					  )
					}else{
						if(input$Dsuivi_12m - input$Dgreffeb > (365 + 4*30) |
							 input$Dsuivi_12m - input$Dgreffeb < (365 - 4*30)){
							paste0("<p style='font-size:18px;color:red;'>", 
										 tr("dsuivi_valide"), 
										 "</p>"
							)
						}else{
							if(rv$suivitroplong == TRUE){
								paste0("<p style='font-size:18px;color:red;'>",
											 tr("suivitroplong"),
											 "</p>"
								)
							}else{
								if(rv$creatinvalide == TRUE){
									paste0(
										"<p style='font-size:18px;color:red;'>", 
										tr("creat_valide"), 
										"</p>"
									)
								}else{	
								paste0(
									"<p style='font-size:18px;color:red;'>", 
											 tr("completer_champs"), 
											 "</p>"
									)
								}
							}
						}
					}
				}
			}
		}
	})
	
	output$phrase_graph <- renderText({
		if(rv$validform){
			tr("phrase_graph")
		}
	})
	
	## Affichage du slider si plusieurs visites ##
	observe({
		if(rv$validform & rv$nb_suivi != 0){
			updateSliderInput(session, 
				"slider_graph", 
				value = (rv$nb_suivi+1), 
				min = 1, 
				max = (rv$nb_suivi+1), 
				step = 1)
			show("slider_graph")
		}else{
			hide("slider_graph")
		}
	})
	
	## Création du data.frame à partir des données du formulaire ##
	observe({
		if(rv$validform){
		
			Dsuivi <- input$Dsuivi_12m
			if(rv$nb_suivi != 0){
				for(i in 1:rv$nb_suivi){				
					Dsuivi <- c(Dsuivi, input[[paste0("Dsuivi_", i)]])
				}
			}
			rv$Dsuivi <- Dsuivi
			
			creat <- input$creat_12m
			if(rv$nb_suivi != 0){
				for(i in 1:rv$nb_suivi){
					creat <- c(creat, input[[paste0("creat_", i)]])
				}
			}
			rv$creat <- creat
		
			rv$bdd.long <- data.frame(
				clef = "111", 
				Dsuivi = rv$Dsuivi,
				creat = rv$creat,
				Dgreffeb = input$Dgreffeb, 
				yearGreffe = format(input$Dgreffeb,"%Y"),
				periode2008 = ifelse(as.numeric(format(input$Dgreffeb,"%Y"))<2008,1,0),
				rangGreffe = as.numeric(input$rangGreffe),
				AgeR_calc = input$AgeR_calc,
				cardioVasc = as.numeric(input$cardioVasc),
				antiClassI = as.numeric(input$antiClassI),
				Rejet1an = as.numeric(input$rejet1an),
				creat_3m = input$creat_3m,
				#param suivi 
				Dsuivi_12mb = input$Dsuivi_12m,
				TpsEvtAns_depM12 = as.numeric(max(as.numeric(rv$Dsuivi - input$Dsuivi_12m)/365.25)),
				tps_postM12 = as.numeric((rv$Dsuivi - input$Dsuivi_12m)/365.25),
				#standardisation des var QT (/sd cf papier EJE)
				AgeR_S = input$AgeR_calc/13.6,		
				creat_3m_S = input$creat_3m/53.4
			)
		}
	})
	
	
	## Graphique résultat ##	
	output$graph <- renderPlot({
		if(rv$validform){
			if(dim(rv$bdd.long)[1]>=1){
				
				withProgress(message = tr("chargement"), {
									
					if(rv$nb_suivi != 0){
						rv$ind <- input$slider_graph
					}else{
						rv$ind <- 1
					}
					
					set.seed(123)
					
					rv$survPreds <- survfitJM(JM_dynPG, 
											newdata=rv$bdd.long[1:rv$ind,], 
											last.time=round(as.numeric(rv$bdd.long[rv$ind,"tps_postM12"]),2), 
											idVar="clef", 
											survTimes=seq(round(as.numeric(rv$bdd.long[rv$ind,"tps_postM12"]),2), 
														  round(as.numeric(rv$bdd.long[rv$ind,"tps_postM12"]),2)+5,
														  0.33), 
											simulate=T,
					  					M=500) #200 par défaut mais meme avec 10000je ne vois pas d amélioration des IC ! par contre le temps de calcul s'allonge +++ ...

					par(xpd=T,
						oma = c(2,1,0,2), #le 2 tout à droite permet de bien afficher la proba indiv 
						mai=c(1.5, 0,0,0),
						#plt=c(0.15,0.85,0.15,0.9), 
						mar=c(8.7, 4.1, 2.5, 3),
						usr=c(-1,1,0, 10)
					)	#-1,1,0, 10			
					
					rv$plotMC <- plot.survfitMC(rv$survPreds,  
									estimator="median", 
									 xaxt='n',
									 conf.int=TRUE, 
									 include.y=TRUE, 
									 langue=input$langue,
									 xlab=NULL, 
									 ylab=NULL, 
									 ylab2=NULL,
									 legendgraph=T,
									 main="",
									 cex.main=0.8,
									 fleches=FALSE, 
									 col.area = rgb(255,182,193,alpha=170,max=255),
									 #version N&B : rgb(170,182,190,alpha=70,max=255), 
									 col.testimate = "red",#"black",
									 col.km="black", #"black", 
									 lty.km=2 , 
									 col.yevo="blue", #"black",
									 col.rectobs="gray97", 
									 col.rectpred="antiquewhite",#"gray80",
									 hachurefondpred=NULL,
									 horizon=5
					)

				})	
			}	
		}
	},width = function() {
		min(session$clientData$output_graph_width,500)
	}
	)
	
	compute_pred <- function(){
		if(rv$nb_suivi != 0){
			rv$ind2 <- rv$nb_suivi + 1
		}else{
			rv$ind2 <- 1
		}
					
		set.seed(123)
					
		rv$survPreds <- survfitJM(JM_dynPG, 
								newdata=rv$bdd.long[1:rv$ind2,], 
								last.time=round(as.numeric(rv$bdd.long[rv$ind2,"tps_postM12"]),2), 
								idVar="clef", 
								survTimes=seq(round(as.numeric(rv$bdd.long[rv$ind2,"tps_postM12"]),2), 
											  round(as.numeric(rv$bdd.long[rv$ind2,"tps_postM12"]),2)+5,
											  0.33), 
								simulate=T,
								M=500) #200 par défaut mais meme avec 10000je ne vois pas d amélioration des IC ! par contre le temps de calcul s'allonge +++ ...
		
		rv$plotMC <- plot.survfitMC(rv$survPreds,  
						estimator="median", 
						 xaxt='n',
						 conf.int=TRUE, 
						 include.y=TRUE, 
						 langue=input$langue,
						 xlab=NULL, 
						 ylab=NULL, 
						 ylab2=NULL,
						 legendgraph=T,
						 main="",
						 cex.main=0.8,
						 fleches=FALSE, 
						 col.area = rgb(255,182,193,alpha=170,max=255),
						 #version N&B : rgb(170,182,190,alpha=70,max=255), 
						 col.testimate = "red",#"black",
						 col.km="black", #"black", 
						 lty.km=2 , 
						 col.yevo="blue", #"black",
						 col.rectobs="gray97", 
						 col.rectpred="antiquewhite",#"gray80",
						 hachurefondpred=NULL,
						 horizon=5
		)
	}
	
	

	## Histogramme résultat ##
	output$hist <- renderPlot(
	  {
	    if(rv$validform){

	      
	      
	      rv$test<-rv$plotMC
	      rv$tps<-round(rv$bdd.long[rv$ind,]$tps_postM12,0)   
	      
	      
	      rv$ss.base<-pi.learning[pi.learning$u==rv$tps,]
	      
	      
	      rv$ss.base0<-pi.learning[pi.learning$u==0,]

	      rv$res<-hist(1-rv$ss.base0$pi,nclass=30)
	    
	      rv$ylim.sup<-max(rv$res$counts)
	    
	      

	      par(cex.axis=1, cex.lab=1.1, xpd=FALSE, mar=c(5,5,1,2))
	      hist(1-rv$ss.base$pi,ylim=c(0,rv$ylim.sup), xlim=c(0,1), nclass=30,
	           xlab=tr("Xlab.hist"), main=tr("Title.hist"),cex.main=1, ylab=tr("effectif"))
	      segments(x0=rv$test, x1=rv$test, y0=-50, y1=rv$ylim.sup, col="red", lwd=3, xpd=TRUE)
	      text(rv$test, -125, round(rv$test,2), cex=1.5, xpd=TRUE, col="red")
	    }
	  }
	  ,width = function() {
	    min(session$clientData$output_graph_width,500)
	  }
	  ,height = function() {
	    min(session$clientData$output_survie_height,350)
	  }
	)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	observeEvent(rv$bdd.long,{	

		if(rv$validform & (input$tabset == "interp" | input$tabset == "InclusionCriteria" | input$tabset == "Refs")){				
			withProgress(message = tr("chargement"), {
				hide("phrase")
			  hide("phraseABDE")
			  hide("bonhomme")
				hide("definition")
				compute_pred()
				show("phrase")
				show("phraseABCDE")
				show("bonhomme")
				show("definition")
			})
			
		  
		  
		}
	})
	
	
	
	
	
	
	observeEvent(input$tabset,{
	  if(input$tabset=="InclusionCriteria"){
	    
	    
	  # translation$shorttitre$FR
	  shinyalert(
	    title = "",
	    size = "m",
	    html = TRUE,
	    showConfirmButton = TRUE,
	    closeOnEsc = TRUE,
	    closeOnClickOutside = FALSE,
	    text=   p(
	             h1(tr("info")),
	             br(),
	             h5(tr("desc_DynPG"),style='text-align:left;'),
	             br(),
	             h5(HTML(tr("crit_inc")),style='text-align:left;'),
	             br(),
	             br(),
	             h5(tr("confirm_sentence"),style='text-align:left;')
	             )
	      # p(h1("Important message"),
	      #         br(),
	      #         h4("Please confirm that "))
	    ,
	    confirmButtonText = tr("confirm")
	    
	  )
	  }    
	}
	)
	
	
	
	################################
	##### 	Pour les bonhommes 	####
	################################
	
	output$phrase <- renderText({ 
		if(rv$validform){
			rv$nbevt <- round((1-rv$plotMC)*10, 0)
			if( input$langue == "FR" ){
				paste0(
					"<p style='font-size:16px;margin-top:10px;' align='justify'>
					La probabilité d'être en vie avec un greffon fonctionnel dans 5 ans est estimée à ", 100-round((1-rv$plotMC)*100, 0), "%.<br>
					Parmi 10 patients ayant des caractéristiques comparables aux vôtres on peut attendre que ", 
					10 - rv$nbevt, 
					ifelse(10 - rv$nbevt > 1, " patients seront en vie avec leur", " patient sera en vie avec son"),
					" greffon fonctionnel dans 5 ans.</p>"
				)
			}else{
				paste0(
					"<p style='font-size:16px;margin-top:10px;' align='justify'>
					The probability of being alive with a functioning transplant at 5 years is estimated at ", 100-round((1-rv$plotMC)*100, 0), "%.<br>
					Among 10 patients with comparable characteristics, one can expect that ", 
					10 - rv$nbevt, 
					ifelse(10 - rv$nbevt > 1, " patients", " patient"),
					" will be alive with a functioning graft at 5 years.</p>"					
				)	      
			}
		}
	})
	
	output$definition <- renderText({ 
		if(rv$validform){
			paste0(
				"<p style='font-size:14px;margin-top:50px;'>*",
				tr("def_gf"),
				"</p>"
			)
		}
	})
	
	test <- function(numero){
		if(rv$validform){
			if(numero <= round((1-rv$plotMC)*10, 0)){
				figure <- 'www/bonhommepasok.jpg'
			}else{
				figure <- 'www/bonhommeok.jpg'
			}
			return(list(
				src=figure, width=80, align = "left"
			))
		}else{
			return(list(
				src='', width=80, align = "left"
			))
		}
	}
	
	output$bonhomme1 <- renderImage({
		test(1)
	}, deleteFile = FALSE)
	
	output$bonhomme2 <- renderImage({
		test(2)
	}, deleteFile = FALSE)
	
	output$bonhomme3 <- renderImage({
		test(3)
	}, deleteFile = FALSE)
	
	output$bonhomme4 <- renderImage({
		test(4)
	}, deleteFile = FALSE)
	
	output$bonhomme5 <- renderImage({
		test(5)
	}, deleteFile = FALSE)
	
	output$bonhomme6 <- renderImage({
		test(6)
	}, deleteFile = FALSE)
	
	output$bonhomme7 <- renderImage({
		test(7)
	}, deleteFile = FALSE)
	
	output$bonhomme8 <- renderImage({
		test(8)
	}, deleteFile = FALSE)
	
	output$bonhomme9 <- renderImage({
		test(9)
	}, deleteFile = FALSE)
	
	output$bonhomme10 <- renderImage({
		test(10)
	}, deleteFile = FALSE)
	
	
	
	
	
	################################
	##### 	Pour le plot ABCDE 	####
	################################
	
	observeEvent(rv$validform,{
	  
	  if(rv$validform){
	    
	    show("ABCDE")
	    
	  }else{
	    
	    hide("ABCDE")
	    
	  }
	  
	})
	
	output$ABCDE <- renderImage({
	  
	  rv$test<-rv$plotMC

	  rv$tps<-round(as.numeric(rv$bdd.long[rv$ind,]$tps_postM12),0)   
	  rv$ss.base<-pi.learning[pi.learning$u==rv$tps,]
	  
	  rv$cut<-round(quantile(1-rv$ss.base$pi,probs=c(0,0.2,0.4,0.6,0.8,1),na.rm=TRUE),2)

	  if(!is.null(rv$plotMC)){
	  if(rv$test>rv$cut[1] & rv$test<=rv$cut[2]){
	    
	    list(src='www/ScoreE.jpg', width=160)
	    
	  }else{
	    if(rv$test>rv$cut[2] & rv$test<=rv$cut[3]){
	    
	      list(src='www/ScoreD.jpg', width=160)
	      
	    }else{
	      if(rv$test>rv$cut[3] & rv$test<=rv$cut[4]){
	      
	        list(src='www/ScoreC.jpg', width=160)
	        
	      }else{
	        if(rv$test>rv$cut[4] & rv$test<=rv$cut[5]){
	        
	          list(src='www/ScoreB.jpg', width=160)
	          
	        }else{
	          if(rv$test>rv$cut[5] & rv$test<=rv$cut[6]){
	            
	           list(src='www/ScoreA.jpg', width=160)
	            
	           }
	        }
	      }
	    }
	  }
	  }else{ list(src='www/ScoreA.jpg', width=0)}
	  
	  
	  
	}, deleteFile = FALSE)
	
	output$phraseABCDE <- renderText({ 
	  if(rv$validform){
	    
	    rv$test<-rv$plotMC
	    
	    rv$tps<-round(as.numeric(rv$bdd.long[rv$ind,]$tps_postM12),0)   
	    rv$ss.base<-pi.learning[pi.learning$u==rv$tps,]
	    
	    rv$cut<-round(quantile(1-rv$ss.base$pi,probs=c(0,0.2,0.4,0.6,0.8,1),na.rm=TRUE),2)
	    
	    if(!is.null(rv$plotMC)){
	    
	    if(rv$test>rv$cut[1] & rv$test<=rv$cut[2]){
	      if(input$langue == "US"){
	        paste0("<p style='font-size:16px;margin-top:10px;' align='justify'>
	                       The Patient-Graft survival probability is in the fifth quintile of predictions of the learning dataset.</p>"					
	        )	
	      }else{
	        paste0("<p style='font-size:16px;margin-top:10px;' align='justify'>
	                       La probabilité de survie Patient-Greffon prédite pour le sujet appartient au 5ème quintile des prédictions de l’échantillon d’apprentissage.</p>"					
	        )
	      }
	      
	    }else{
	      if(rv$test>rv$cut[2] & rv$test<=rv$cut[3]){
	        if(input$langue == "US"){
	          paste0("<p style='font-size:16px;margin-top:10px;' align='justify'>
	                       The Patient-Graft survival probability is in the fourth quintile of predictions of the learning dataset.</p>"					
	          )	
	        }else{
	          paste0("<p style='font-size:16px;margin-top:10px;' align='justify'>
	                       La probabilité de survie Patient-Greffon prédite pour le sujet appartient au 4ème quintile des prédictions de l’échantillon d’apprentissage.</p>"					
	          )
	        }
	      }else{
	        if(rv$test>rv$cut[3] & rv$test<=rv$cut[4]){
	          if(input$langue == "US"){
	            paste0("<p style='font-size:16px;margin-top:10px;' align='justify'>
	                       The Patient-Graft survival probability is in the third quintile of predictions of the learning dataset.</p>"					
	            )	
	          }else{
	            paste0("<p style='font-size:16px;margin-top:10px;' align='justify'>
	                       La probabilité de survie Patient-Greffon prédite pour le sujet appartient au 3eme quintile des prédictions de l’échantillon d’apprentissage.</p>"					
	            )
	          }
	          
	        }else{
	          if(rv$test>rv$cut[4] & rv$test<=rv$cut[5]){
	            if(input$langue == "US"){
	              paste0("<p style='font-size:16px;margin-top:10px;' align='justify'>
	                       The Patient-Graft survival probability is in the second quintile of predictions of the learning dataset.</p>"					
	              )	
	            }else{
	              paste0("<p style='font-size:16px;margin-top:10px;' align='justify'>
	                       La probabilité de survie Patient-Greffon prédite pour le sujet appartient au 2ème quintile des prédictions de l’échantillon d’apprentissage.</p>"					
	              )
	            }
	          }else{
	            if(rv$test>rv$cut[5] & rv$test<=rv$cut[6]){
	              if(input$langue == "US"){
	                paste0("<p style='font-size:16px;margin-top:10px;' align='justify'>
	                       The Patient-Graft survival probability is in the first quintile of predictions of the learning dataset.</p>"					
	                )	
	              }else{
	                paste0("<p style='font-size:16px;margin-top:10px;' align='justify'>
	                       La probabilité de survie Patient-Greffon prédite pour le sujet appartient au 1er quintile des prédictions de l’échantillon d’apprentissage.</p>"					
	                )
	              }
	            }
	          }
	        }
	      }
	    }
	    }
	    
	    
	    
	    
	    
	  }
	})
	
	
})
##############################################################
#
# End   
#
##############################################################