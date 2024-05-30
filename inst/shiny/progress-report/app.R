# Individual student's progress report for BioDataScience-Course at UMONS
# Version 3.0.0, Copyright (c), 2021-2024, Philippe Grosjean & Guyliann Engels
#
# TODO:
# - Rework to allow using outside of SDD context
# - Specify academic year
# - Check messages in the report
# - Better plots... may be using plotly for interactivity! + use median and max
#   for learnr plots
# - Help pages /progress-login and /progress-report to implement

# Need shinyFeedback v 0.3.0 from GitHub in svbox2020... OK for svbox2021
#remotes::install_github("merlinoa/shinyFeedback@v0.3.0")

#date_query_def <- '"date": { "$gt": "2022-09-18 00:00:00.000000" }, '

library(shiny)
#library(shinyjs)
library(shinybusy)
library(shinythemes)
#library(shinyFeedback)
library(RCurl)
library(mongolite)
#library(PKI)
library(dplyr)
library(ggplot2)
library(cowplot)
library(fs)
#library(svFlow)
library(learnitprogress)


# Connect to the course LRS (MongoDB database) ----------------------------

# TODO: make independent from SDD
options(learnitr.lrs_url = getOption("learnitr.lrs_url",
  default = Sys.getenv("LEARNITR_LRS_URL",
    unset = "mongodb://127.0.0.1/sdd")))

mdb_h5p <- mongolite::mongo('h5p', url = getOption("learnitr.lrs_url"))
mdb_h5p$disconnect()
mdb_learnr <- mongolite::mongo('learnr', url = getOption("learnitr.lrs_url"))
mdb_learnr$disconnect()
mdb_shiny <- mongolite::mongo('shiny', url = getOption("learnitr.lrs_url"))
mdb_shiny$disconnect()


# The Shiny app -----------------------------------------------------------

ui <- fluidPage(theme = shinytheme("lumen"),
  titlePanel(textOutput("title"), windowTitle = "Progression"),

  # State #1: message when the app is launched with incorrect/incomplete URL
  conditionalPanel('output.wrongURL !== null & output.wrongURL != ""',
    span(tags$b(textOutput("wrongURL")), style = "color:red")
  ),

#  # State #2: ask for login and user recording if not registered yet
#  conditionalPanel('output.loginMessage !== null & output.loginMessage != ""',
#    tags$b(textOutput("loginMessage")),
#    shinyjs::useShinyjs(),
#    shinyFeedback::useShinyFeedback(),
#    textInput("login", "Login Github :", ""),
#    actionButton("register", "Enregistrement", icon = icon("key")),
#    helpText(htmlOutput("registerMessage")),
#    tags$a("Page d'aide",
#      href = "https://wp.sciviews.org/progress-login", target = "_blank")
#  ),

  # State #3: display the progress report for the registered user
  conditionalPanel('output.wrongURL == ""', # & output.loginMessage == ""',

    tabsetPanel(
      tabPanel("Progression",
        h4("Progression des H5Ps, Shiny apps & Learnrs"),
        htmlOutput("reportMessage"), # General report message
        htmlOutput("learnrMessage"), # Message specific for the learnrs
        plotOutput("learnrPlot", height = 600),

        #h4("Progression sur Github"),
        #htmlOutput("githubMessage"), # Message specific for Github activity
        #plotOutput("githubPlot"),
        tags$a("Page d'aide",
          href = "https://wp.sciviews.org/progress-report")
      ),
      tabPanel("Projets GitHub",
        h4("Grilles de correction des projets GitHub"),
        htmlOutput("gridMessage"), # Message specific for the correction grids
        selectInput("gridSelect", " ", c()),
        dataTableOutput("gridTable"),

        tags$a("Page d'aide",
          href = "https://wp.sciviews.org/progress-report")
      ),
      tabPanel("Note",
        h4("D\u00e9tail de la note"),
        htmlOutput("gradeMessage"), # Message specific for the grade plot
        plotOutput("gradePlot", height = 600),

        tags$a("Page d'aide",
          href = "https://wp.sciviews.org/progress-report")
      )
    )
  )
)

server <- function(input, output, session) {
  # User login, email, ... globally used in the app
  user <- reactiveValues(data = NULL, learnr = NULL, github = NULL,
    grade = NULL, grid = NULL, course = NULL, module = NULL, login = " ",
    email = " ", firstname = " ", lastname = " ", message = "",
    learnr_message = "", github_message = "",
    grade_message =
      "La note finale pour cette AA n'est pas encore attribu\u00e9e.",
    grid_message =
      "Pas de grille de correction de projet GitHub actuellement disponible.",
    done = FALSE)

  # Modal busy box with spinner
  shinybusy::show_modal_spinner(spin = "radar")

  # During calculations, login inputbox & register button are visible
  # This was the only solution I found for now to avoid it
#  shinyjs::hide("register")
#  shinyjs::hide("login")

  # Get parameters transmitted by Moodle through the URL
  observe({
    # Avoid displaying wrongURL and login conditional panels for now
    query <- parseQueryString(session$clientData$url_search)
    if (!is_correct_query(query)) {# Wrong URL
      # Place (and lock down) the application in state #1 - nothing more to do
      user$email <- "" # This way, conditionalPanel 'wrongURL' is activated
      showNotification(type = "error", duration = 10,
        "Le rapport de progression n'est disponible que depuis le cours (si on est correctement enregistr\u00e9) ou depuis Moodle.")

    } else if (!isTRUE(isolate(user$done))) {# Correct URL, process only once
      user$course <- query$course
      user$module <- query$module
      user$data <- user_data <- profile_from_query(query)
      user$firstname <- user_data['ifirstname']
      user$lastname <- user_data['ilastname']
      user_profile <- get_profile(user_data['iemail'])
      # If there is an error, print it at the console
      if (!is.null(comment(user_profile)))
        cat("User profile error:", comment(user_profile), "\n")

    #   if (!length(user_profile)) {# If the profile is empty
    #     # Switch in state #2: ask for user registration
    #     user$email <- user_data['iemail']
    #     user$login <- ""
    #     shinyjs::show("login")
    #     shinyjs::show("register")
    #
    #   } else {# Profile exists
         # Check provided data match
         res <- check_profile(user_profile, user_data)
         if (!res) {
           # Indicate what was wrong at the console
           cat("Profile verification:", comment(res), "\n")
           # Switch the app in state #1 (lock down)
           user$email <- "" # This way, conditionalPanel 'wrongURL' is activated
           showNotification(type = "error", duration = 10,
             "Profil utilisateur incorrect, contactez vos enseignants.")

         } else {# Our app will be in state #3, create the progress report now
           user$login <- user_profile$login
           user$email <- user_profile$email

           # Get data for the learnrs and the Github activity
           if (is.null(user$course)) {
             course_message <- paste0("de ", user$data["ictitle"], " (",
               user$data["icourse"], ")")
           } else {
             course_message <- paste(user$course)
           }
           if (is.null(user$module)) {
             module_message <- ""
           } else {
             module_message <- paste(", module", user$module)
           }
           user$message <- paste0("Votre progression pour le cours ",
             course_message, module_message,
             ". Gris = progression de la classe, bleu = exercices r\u00e9ussis, rouge = r\u00e9ponses incorrectes. Ceci ne correspond pas \u00e0 la note finale mais seulement \u00e0 l'\u00e9tat d'avancement. Le rapport s'actualise toutes les 5 minutes (donc il se peut que vos derni\u00e8res actions ne soient pas encore visibles).")

           # Get learnr data
           #user_learnr <- get_learnr_data(user$email)
           if (is.null(user$course)) {
             icourse <- user$data["icourse"]
           } else {
             icourse <- user$course
           }
           user_learnr <- get_all_data(user$data["iemail"], icourse, user$module)
           if (!is.null(comment(user_learnr))) {
             cat("Error getting Learnr, Shiny & H5P data:", comment(user_learnr),
               "\n")
             user$message <- paste0(user$message,
               " <b>Pas de donn\u00e9es Learnr, Shiny, H5P",
               " ou donn\u00e9es erron\u00e9es.</b>")
           } else {
             # TODO: get latest data from the database now!
             user$learnr <- user_learnr
             user$learnr_message <- user_learnr$comment
             # Uncomment for debugging purposes
             #user$learnr_message <- paste0("Nlines learnr data:",
             #  nrow(user_learnr$data))
             # Also get general message
             if (!is.null(user_learnr$message))
               user$message <- paste0(user$message, "\n<br/>\n",
                 user_learnr$message)
           }

           # Get Github data
           user_github <- get_github_data(user$email)
           if (!is.null(comment(user_github))) {
             cat("Error getting Github data:", comment(user_github), "\n")
             user$message <- paste0(user$message,
               " <b>Pas de donn\u00e9es Github,",
               " ou donn\u00e9es erron\u00e9es.</b>")
           } else {
             user$github <- user_github
             user$github_message <- user_github$comment
             # Uncomment for debugging purposes
             #user$github_message <- paste0("Nlines Github data:",
             #  nrow(user_github$data),
             #  " - Nlines trend: ", nrow(user_github$trend))
           }

           # Get grade data
           user_grade <- get_grade_data(user$data["iemail"], icourse)
           if (!is.null(user_grade)) {
             user$grade <- user_grade
           }

           # Get grid data
           user_grid <- get_grid_data(user$data["iemail"], icourse, user$module)
           if (!is.null(user_grid)) {
             user$grid <- user_grid
             updateSelectInput(session, "gridSelect", choices = user_grid,
               selected = user_grid[1])
           }
         }
       #}
     }

    # Avoid running these long calculations twice and close the busy modal box
    user$done <- TRUE
    shinybusy::remove_modal_spinner()
  })

  # App title, with user firstname and lastname if possible
  output$title <- renderText({
    #paste("Progression", user$firstname, user$lastname)
    # Don't put "Progression in the title, cf. already above in Moodle
    paste(user$firstname, user$lastname)
  })

  # State #1: wrong URL parameters. Display only this message
  output$wrongURL <- renderText({
    if (!is.null(user$email) && user$email == "") {
      "Impossible d'afficher votre progression: vous devez lancer cette application depuis le cours et y être enregistré comme \u00e9tudiant ou depuis votre compte Moodle institutionnel. Si c'est le cas, voyez vos enseignants pour qu'ils corrigent le bug !"
    } else ""
  })

#  # State #2: ask for Github login to register this user, no report yet
#  output$loginMessage <- renderText({
#    if (!is.null(user$login) && user$login == "" &&
#        !is.null(user$email) && user$email != "") {
#      "Votre rapport de progression n'est pas accessible car votre compte n'est pas encore valid\u00e9 ou est incorrect. Entrez votre login GitHub une fois votre compte GitHub cr\u00e9\u00e9, et cliquez sur 'Enregistrement'."
#    } else ""
#  })
#
#  observeEvent(input$login,
#    shinyFeedback::feedbackWarning("login", input$login == "",
#      "Complétez votre login GitHub")
#  )
#
#  response <- eventReactive(input$register, {
#    cat("registering!\n")
#    if (input$login == "") {
#
#      # The message to display
#      "<font color=\"#FF0000\"><b>Vous devez entrer votre login GitHub pour pouvoir vous enregistrer !</b></font>"
#
#    } else if (!is_github_user(input$login)) {# Non-existing Github account
#      shinyFeedback::feedbackDanger("login", TRUE,
#        "Vérifiez votre login GitHub")
#
#      # The message to display
#      "<font color=\"#FF0000\"><b>Login GitHub incorrect (cr\u00e9er d'abord un compte GitHub).</b></font>"
#
#    } else {# Correct Github account
#      shinyFeedback::feedbackSuccess("login", TRUE)
#
#      # Create a record in the sdd/logins database table
#      res <- record_sdd_login(input$login, user$data,
#        role = "student", comment = "")
#      if (res == "") {# OK, registering request recorded
#        # Inactivate the register button + login
#        shinyjs::disable("register")
#        shinyjs::disable("login")
#        #shinyjs::hide("register")
#        #shinyjs::hide("login")
#
#        # The message to display
#        "Demande d'enregistrement effectu\u00e9e. Vous recevrez un mail quand il sera effectif."
#      } else {# Error registering the user in the database
#
#        # The message to display
#        paste0("<font color=\"#FF0000\"><b>Login GitHub correct, mais probl\u00e8me lors de l'enregistrement : ", res,
#          ". V\u00e9rifiez votre connexion Internet et r\u00e9essayez (\u00e9ventuellement plus tard).</b></font>")
#      }
#    }
#  })
#
#  output$registerMessage <- renderText({response()})

  # State #3: display progress report
  output$reportMessage <- renderText({
    if (!is.null(user$message)) {
      user$message
    } else ""
  })

  output$learnrMessage <- renderText({
    if (!is.null(user$learnr_message)) {
      user$learnr_message
    } else ""
  })

  output$learnrPlot <- renderPlot({
    if (!is.null(user$learnr)) {
      #user_data <- user$learnr$data
      user_data <- user$learnr
      #      user_data$`Mediane classe` <- user_data$median / user_data$max * 100
      #      user_data$`Meilleur` <- user_data$best / user_data$max * 100
      #      user_data$`Votre score` <- user_data$result / user_data$max * 100
      #      user_data$tutorial <- factor(user_data$tutorial,
      #        levels = sort(unique(user_data$tutorial), decreasing = TRUE))
      #      # TODO: ajouter les infos relatives à la médiane de la classe et au meilleur
      #      res <- try(ggplot(user_data, aes(x = tutorial, y = `Votre score`, fill = score)) +
      #        #geom_col(aes(x = tutorial, y = `Mediane classe`, fill = "#888888")) +
      #        geom_col() +
      #        scale_fill_gradient(low = "firebrick", high = "lightblue") +
      #        coord_flip() +
      #        ylab("Pourcentage du tutoriel fait") +
      #        xlab("Tutoriel") +
      #        cowplot::theme_cowplot(font_size = 14))
      #res <- try(ggplot(user_data, aes(x = date, y = cum_max)) +
      #  geom_area() +
      #  xlab("Date") +
      #  ylab(("Nbre d'exercices")) +
      #  geom_area(aes(x = date, y = cum_grade), fill = "blue") +
      #  cowplot::theme_cowplot(font_size = 14)
      res <- try(plot_progression(user$login, user_data))
      if (inherits(res, "try-error")) {
        user$learnr_message <- as.character(res)
      } else {
        res
      }
    }
  })

  output$githubMessage <- renderText({
    if (!is.null(user$github_message)) {
      user$github_message
    } else ""
  })

  output$githubPlot <- renderPlot({
    if (!is.null(user$github)) {
      user_data <- user$github$data
      trend <- user$github$trend
      res <- try(ggplot(user_data, aes(x = day, y = cusum)) +
          geom_smooth(data = trend, aes(x = day, y = trend),
            method = "loess", formula = y ~ x,
            color = "#2e9ce6", alpha = 0.4, se = FALSE, span = 0.3) +
          geom_line() +
          geom_point() +
          geom_vline(xintercept = as.Date("2020-03-15"), color = "red") +
          geom_vline(xintercept = as.Date("2020-03-26"), color = "green") +
          labs(y = "Somme des commits", x = "Temps",
            #title = paste("Activité de", student) ,
            caption = "Somme des commits au cours du temps en noir. La ligne bleue repr\u00e9sente l'activit\u00e9 moyenne de la classe.
    Le trait vertical rouge repr\u00e9sente le d\u00e9but du confinement et le trait vertical vert marque la fin du dernier cours.") +
          cowplot::theme_cowplot(font_size = 14))
      if (inherits(res, "try-error")) {
        user$github_message <- as.character(res)
      } else {
        res
      }
    }
  })

  output$gradeMessage <- renderText({
    if (!is.null(user$grade_message)) {
      user$grade_message
    } else ""
  })

  output$gradePlot <- renderPlot({
    if (!is.null(user$grade)) {
      user_grade <- user$grade

      res <- try(plot_grade(user_grade, show.name = FALSE))
      if (inherits(res, "try-error")) {
        user$grade_message <- as.character(res)
      } else {
        if (is.null(user$course)) {
          course_message <- paste0("de ", user$data["ictitle"], " (",
            user$data["icourse"], ")")
        } else {
          course_message <- paste(user$course)
        }
        user$grade_message <- paste0("Notes obtenues par type d'exercice pour le cours ",
          course_message,
          ". Les notes peuvent \u00eatre inf\u00e9rieures \u00e0 la progression car elles tiennent compte des tentatives pour les H5P, des \u00e9l\u00e9ments corrects pour les applications Shiny et de la visualisation des aides pour les learnrs.")
        res
      }
    }
  })

  output$gridMessage <- renderText({
    if (!is.null(user$grid_message)) {
      user$grid_message
    } else ""
  })

  output$gridTable <- renderDataTable({
    grid <- input$gridSelect
    if (!is.null(grid) && !is.na(grid) && grid != "") {
      tab <- try(read.csv(grid))
      if (inherits(tab, "try-error")) {
        user$grid_message <- as.character(res)
      } else {
        # Get data for the learnrs and the Github activity
        if (is.null(user$course)) {
          course_message <- paste0("de ", user$data["ictitle"], " (",
            user$data["icourse"], ")")
        } else {
          course_message <- paste(user$course)
        }
        if (is.null(user$module)) {
          module_message <- ""
        } else {
          module_message <- paste(", module", user$module)
        }
        user$grid_message <- paste0("Evaluation de vos projets GitHub pour le cours ",
          course_message, module_message,
          ". Le crit\u00e8re indique l'\u00e9l\u00e9ment \u00e9valu\u00e9 (# titre =, @chunk =, !user =, YAML =, ...). Les notes sont cumulatives et certains crit\u00e8res peuvent servir \u00e0 diminuer la note globale ou pour un utilisateur en particulier (points n\u00e9gatifs).")
        tab$note <- paste(round(tab$score, 2), tab$max, sep = "/")
        tab$comment[is.na(tab$comment)] <- ""
        tab <- tab[, c("file", "criterion", "note", "comment")]
        names(tab) <- c("Fichier", "Crit\u00e8re", "Note", "Commentaire")
        tab
      }
    }
  }, options = list(pageLength = 100))

}

shinyApp(ui = ui, server = server)
