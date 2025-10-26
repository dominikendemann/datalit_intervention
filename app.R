# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak("surveydown-dev/surveydown") # Development version from GitHub

# Load packages
library(surveydown)
library(quarto)
library(reactable)
library(shiny)
library(shinyjs)

# Database setup --------------------------------------------------------------
# 
# Details at: https://surveydown.org/docs/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# For this demo, we set ignore = TRUE in the following code, which will ignore
# the connection settings and won't attempt to connect to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.

db <- sd_db_connect(ignore = F)

# UI setup --------------------------------------------------------------------

ui <- sd_ui()

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {

  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if()

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if()

#sd_skip_forward(
 #   input$pw != "vera25" ~ "page29")

  # Database designation and other settings
  sd_server(
    db = db,
    required_questions = c("einverstanden", "info_2", "code_1", "code_2", "code_3", "code_4", "code_5"), #Pflichtrfgen 
    language = "de"
        )

  
  daten_text <- "SuS;Test1_Mathematik_Geometrische_Korper;Test2_Mathematik_Schriftliches_Addieren_Subtrahieren;Test3_Mathematik_Tabellen_und_Diagramme;Test1_Deutsch_Lesetest;Test2_Deutsch_RS_Test;Test3_Deutsch_RS_Test;Anstrengungsbereitschaft;Ausserschulisches_Engagement;Sonstiges
1;2;2;4;5;3;4;mittel;keine Förderung;
2;3;4;3;5;4;5;mittel;keine Förderung;
3;4;3;4;5;4;4;gering;keine Förderung;52 Fehltage
4;1;3;2;3;3;3;hoch;Eltern sehr aktiv;
5;4;2;2;4;2;2;mittel;Hausaufgabenbetreuung;'Abwesend bei VERA Vorbereitung'
6;2;2;2;3;3;2;mittel;Eltern sehr aktiv;
7;2;1;2;3;3;4;hoch;Eltern sehr aktiv;'Leseprobleme, v.a. Leseflüssigkeit'
8;4;4;4;5;4;4;mittel;Hausaufgabenbetreuung;Leseprobleme
9;3;3;4;3;3;3;mittel;Hausaufgabenbetreuung;'Abwesend bei VERA Vorbereitung'
10;4;2;4;5;4;5;mittel;wenig Förderung;'Abwesend bei VERA Vorbereitung'
11;3;4;4;3;2;2;hoch;keine Förderung;
12;5;4;5;5;5;4;mittel;wenig Förderung;
13;2;2;3;2;1;3;hoch;wenig Förderung;
14;1;1;2;1;2;2;hoch;Eltern sehr aktiv; 'Abwesend bei VERA Vorbereitung'
15;3;3;3;2;3;2;mittel;Hausaufgabenbetreuung;
16;3;3;3;5;4;5;gering;keine Förderung;'Unzureichende Sprachkenntnisse'
17;3;3;3;4;5;3;mittel;DaZ-Kurs;'Teilw. Probleme mit D. Sprache'
18;3;4;4;5;3;4;gering;Hausaufgabenbetreuung;
19;2;2;3;4;4;2;mittel;'Eltern sehr aktiv, HA- Betreuung;
20;2;3;4;6;4;5;mittel;keine Förderung;'Probleme mit Sprache'
21;2;2;2;4;3;4;hoch;DaZ-Kurs;'Teilw. Probleme mit D. Sprache'
22;2;4;4;2;4;3;mittel;Eltern sehr aktiv;"
  
  # Daten in ein Dataframe umwandeln
  daten1 <- read.table(text = daten_text, sep = ";", header = TRUE, quote = "")
  
  output$tabelle <- renderReactable({
    
    reactable(
      daten1,
      filterable = TRUE,
      paginationType = "simple", 
      showPageSizeOptions = TRUE,
      striped = TRUE, 
      compact = TRUE,
      fullWidth = FALSE,
      showSortable = TRUE,
      showPageInfo = FALSE,
      defaultPageSize = 23,
      showPagination = FALSE,
      wrap = TRUE,
      style = list(fontFamily = "Work Sans, sans-serif", fontSize = "0.875rem", backgroundColor = "hsl(0, 0%, 95%)", width = "100%"), #das ist css style hier ebstimme ich Schriftgroesse und art
      theme = reactable::reactableTheme(color = "hsl(0, 0%, 0%)", #backgroundColor = "hsl(0, 0%, 24%)", 
                                        borderColor = "hsl(0, 0%, 22%)", stripedColor = "rgba(255, 255, 255, 0.04)", 
                                        highlightColor = "rgba(255, 255, 255, 0.06)", inputStyle = list(backgroundColor = "hsl(0, 0%, 90%)"), 
                                        selectStyle = list(backgroundColor = "hsl(0, 0%, 24%)"), 
                                        pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 24%)"), 
                                        pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 28%)")) 
        ,
      height = 600,
      width = 1250,
      columns = list(
        SuS = colDef(name = "Schüler*in", width = 99, align = "center"),
        Test1_Mathematik_Geometrische_Korper = colDef(name = "<u> Noten </u> <br> Test 1 Mathematik: <br> Geometrische <br> Körper", width = 125, align = "center", html = TRUE),
        Test2_Mathematik_Schriftliches_Addieren_Subtrahieren = colDef(name = "<u> Noten </u> <br> Test 2 Mathematik: Schriftliches Addieren & Subtrahieren", width = 120, align = "center", html = TRUE),
        Test3_Mathematik_Tabellen_und_Diagramme = colDef(name = "<u> Noten </u> <br> Test 3 Mathematik: Tabellen und Diagramme", width = 120, align = "center", html = TRUE),
        Test1_Deutsch_Lesetest = colDef(name = "<u> Noten </u> <br> Test 1 Deutsch: Lesetest", width = 90, align = "center", html = TRUE),
        Test2_Deutsch_RS_Test = colDef(name = "<u> Noten </u> <br> Test 2 Deutsch: RS-Test", width = 90, align = "center", html = TRUE),
        Test3_Deutsch_RS_Test = colDef(name = "<u> Noten </u> <br> Test 3 Deutsch: RS-Test", width = 90, align = "center", html = TRUE),
        Anstrengungsbereitschaft = colDef(name = "Anstrengungsbereitschaft", width = 160, align = "left"),
        Ausserschulisches_Engagement = colDef(name = "Außerschulisches Engagement", width = 170, align = "left"),
        Sonstiges = colDef(name = "Sonstiges", width = 140, align = "left") 
        
      )
    )})
  
  
}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
