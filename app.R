#
# ============================================================================
# LMBC TELEMETRY SHINY APP
# ============================================================================
# This app allows users to upload telemetry .txt files and download processed CSV files

library(shiny)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(tidygeocoder)
library(DT)
library(readr)
library(tidyr)

# Set maximum upload size to 30MB (default is 5MB)
options(shiny.maxRequestSize = 30*1024^2)

# Define UI
ui <- fluidPage(
    
    # CSS for styling
    tags$head(
        tags$style(HTML("
            .header { 
                background-color: #f0f0f0; 
                padding: 20px; 
                margin-bottom: 20px;
                border-radius: 5px;
            }
            .processing { 
                color: #856404;
                background-color: #fff3cd;
                border: 1px solid #ffeeba;
                padding: 10px;
                border-radius: 5px;
                margin: 10px 0;
            }
            .success { 
                color: #155724;
                background-color: #d4edda;
                border: 1px solid #c3e6cb;
                padding: 10px;
                border-radius: 5px;
                margin: 10px 0;
            }
            .error { 
                color: #721c24;
                background-color: #f8d7da;
                border: 1px solid #f5c6cb;
                padding: 10px;
                border-radius: 5px;
                margin: 10px 0;
            }
            .help-content {
                max-width: 800px;
                line-height: 1.6;
                padding: 20px;
            }
            .help-content h2 {
                color: #2c3e50;
                border-bottom: 2px solid #3498db;
                padding-bottom: 10px;
            }
            .help-content h3 {
                color: #34495e;
                margin-top: 25px;
            }
        "))
    ),
    
    # Application header
    div(class = "header",
        h2("LMBC Telemetry Processor"),
        p("Upload a telemetry .txt file to process and download the results as CSV")
    ),
    
    # Tabbed interface
    tabsetPanel(
        # Main processing tab
        tabPanel("Process Data",
            sidebarLayout(
                sidebarPanel(
                    h4("Upload Telemetry File"),
                    fileInput("telemetryFile", 
                              "Choose .txt file:",
                              accept = c(".txt", "text/plain")),
                    hr(),
                    h4("File Information"),
                    uiOutput("fileInfo"),
                    hr(),
                    h4("Processing Status"),
                    uiOutput("processingStatus")
                ),
                
                mainPanel(
                    h3("Processed Data Preview"),
                    uiOutput("downloadSection"),
                    hr(),
                    DT::dataTableOutput("dataPreview")
                )
            )
        ),
        
        # Help tab
        tabPanel("Help & Documentation",
            div(class = "help-content",
                h1("LMBC Telemetry Processor - User Guide"),
                
                h2("What This App Does"),
                p("This application takes telemetry data files from rowing sessions and converts them into easy-to-read spreadsheets with detailed performance metrics for each crew member. The app calculates:"),
                tags$ul(
                    tags$li(tags$strong("Power metrics"), " - Overall power output, stroke-side and bow-side power"),
                    tags$li(tags$strong("Work distribution"), " - How power is distributed across the stroke (Q1-Q4 percentages)"),
                    tags$li(tags$strong("Angle measurements"), " - Catch angles, finish angles, and stroke length"),
                    tags$li(tags$strong("Slip analysis"), " - Catch slip, finish slip, and total slip"),
                    tags$li(tags$strong("Efficiency metrics"), " - Effective length and stroke effectiveness")
                ),
                
                h2("How to Use the App"),
                
                h3("Step 1: Getting Started"),
                tags$ol(
                    tags$li("Click on the 'Process Data' tab above"),
                    tags$li("You'll see an upload area on the left and a preview area on the right")
                ),
                
                h3("Step 2: Upload Your Data File"),
                tags$ol(
                    tags$li("Click the ", tags$strong("'Choose .txt file'"), " button in the sidebar"),
                    tags$li("Select your telemetry data file from your computer"),
                    tags$ul(
                        tags$li("The file must be a .txt file from your rowing telemetry system"),
                        tags$li("Files can be up to 30MB in size")
                    ),
                    tags$li("Once selected, you'll see the file name and size displayed")
                ),
                
                h3("Step 3: Processing"),
                tags$ol(
                    tags$li("The app will automatically start processing your file"),
                    tags$li("You'll see a yellow 'Processing...' message while the app works"),
                    tags$li("This usually takes 10-30 seconds depending on file size")
                ),
                
                h3("Step 4: View Results"),
                tags$ol(
                    tags$li("When processing is complete, you'll see a green 'Success!' message"),
                    tags$li("A data preview table will appear showing your processed results"),
                    tags$li("Each row represents one crew member with all their performance metrics")
                ),
                
                h3("Step 5: Download Your Results"),
                tags$ol(
                    tags$li("Click the blue ", tags$strong("'Download CSV'"), " button"),
                    tags$li("Save the file to your computer"),
                    tags$li("The filename will be automatically generated with the date and time (e.g., data_2024_12_06_14_30_15.csv)")
                ),
                
                h2("Understanding Your Results"),
                p("Your downloaded CSV file contains 36 columns of data for each crew member:"),
                
                h3("Basic Information"),
                tags$ul(
                    tags$li(tags$strong("seat"), " - Seat position (1-8)"),
                    tags$li(tags$strong("name"), " - Crew member name"),
                    tags$li(tags$strong("day/date"), " - When the session took place"),
                    tags$li(tags$strong("type"), " - Type of rowing piece"),
                    tags$li(tags$strong("boat"), " - Boat configuration (e.g., '8+', '4-')")
                ),
                
                h3("Session Metrics"),
                tags$ul(
                    tags$li(tags$strong("duration"), " - Length of the piece"),
                    tags$li(tags$strong("rate"), " - Stroke rate (strokes per minute)"),
                    tags$li(tags$strong("distance"), " - Total distance covered"),
                    tags$li(tags$strong("speed"), " - Average boat speed"),
                    tags$li(tags$strong("pace"), " - Time per 500m")
                ),
                
                h3("Power Analysis"),
                tags$ul(
                    tags$li(tags$strong("power"), " - Overall power output (watts)"),
                    tags$li(tags$strong("power_stroke"), " - Power for stroke-side rowers (watts)"),
                    tags$li(tags$strong("power_bow"), " - Power for bow-side rowers (watts)")
                ),
                
                h3("Work Distribution (with benchmarks)"),
                tags$ul(
                    tags$li(tags$strong("work_q1_percent"), " - Power in first quarter of stroke (benchmark: 18%)"),
                    tags$li(tags$strong("work_q2_percent"), " - Power in second quarter (benchmark: 37%)"),
                    tags$li(tags$strong("work_q3_percent"), " - Power in third quarter (benchmark: 34%)"),
                    tags$li(tags$strong("work_q4_percent"), " - Power in fourth quarter (benchmark: 11%)"),
                    tags$li("Each work metric includes a 'difference' column showing how you compare to the benchmark")
                ),
                
                h3("Technique Analysis"),
                tags$ul(
                    tags$li(tags$strong("angle_catch"), " - Catch angle (degrees)"),
                    tags$li(tags$strong("angle_finish"), " - Finish angle (degrees)"),
                    tags$li(tags$strong("angle_70_percent"), " - Angle at 70% of maximum force"),
                    tags$li(tags$strong("length"), " - Total stroke length"),
                    tags$li(tags$strong("effective_length"), " - Useful stroke length (after accounting for slip)")
                ),
                
                h3("Slip Analysis"),
                tags$ul(
                    tags$li(tags$strong("slip_catch"), " - Blade slip at catch"),
                    tags$li(tags$strong("slip_finish"), " - Blade slip at finish"),
                    tags$li(tags$strong("slip_total"), " - Total slip during stroke")
                ),
                
                h2("Troubleshooting"),
                
                h3("File Won't Upload"),
                tags$ul(
                    tags$li(tags$strong("Check file format"), ": Only .txt files are accepted"),
                    tags$li(tags$strong("Check file size"), ": Files must be under 30MB")
                ),
                
                h3("Processing Errors"),
                tags$ul(
                    tags$li(tags$strong("Special characters"), ": The app handles special characters, but very unusual formatting might cause issues"),
                    tags$li(tags$strong("Corrupted files"), ": Try re-exporting the file from your telemetry system"),
                    tags$li(tags$strong("Missing data sections"), ": Some telemetry files may be incomplete")
                ),
                
                h3("No Data in Results"),
                tags$ul(
                    tags$li(tags$strong("Check your file"), ": Ensure it contains the expected telemetry sections"),
                    tags$li(tags$strong("Verify format"), ": File should have clear section breaks marked with '======'")
                ),
                
                h2("Tips for Best Results"),
                tags$ul(
                    tags$li(tags$strong("Regular backups"), " - Keep copies of your original telemetry files"),
                    tags$li(tags$strong("Consistent naming"), " - Use consistent names for your crew members")
                ),
                
                h2("Data Privacy"),
                tags$ul(
                    tags$li("Files are processed temporarily and not permanently stored"),
                    tags$li("Your data remains private and is not shared"),
                    tags$li("Original files are not modified"),
                    tags$li("Processed results are only accessible to you")
                ),
                
                hr(),
                tags$em("This application processes LMBC telemetry data to provide comprehensive rowing performance analysis. For technical support or feature requests, please contact your system administrator.")
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive values to store processing state
    values <- reactiveValues(
        processedData = NULL,
        fileName = NULL,
        status = NULL,
        error = NULL
    )
    
    # Function to parse time series data
    parse_time_series_section <- function(section_lines) {
      
      if (length(section_lines) < 2) {
        return(data.frame())
      }
      
      # Create temporary file
      temp_file <- tempfile()
      writeLines(section_lines, temp_file)
      
      tryCatch({
        # Read as tab-delimited with first row as headers
        timeseries_data <- read.table(temp_file, 
                                      sep = "\t", 
                                      header = TRUE, 
                                      stringsAsFactors = FALSE,
                                      fill = TRUE,
                                      blank.lines.skip = TRUE,
                                      comment.char = "",
                                      na.strings = c("", "NA"),
                                      quote = "\"'",  # Handle both double and single quotes
                                      encoding = "UTF-8")  # Handle special characters
        
        # Clean up
        unlink(temp_file)
        
        # Convert Time column to numeric if it exists
        if ("Time" %in% names(timeseries_data)) {
          timeseries_data$Time <- as.numeric(timeseries_data$Time)
        }
        
        # Convert other numeric columns
        numeric_cols <- names(timeseries_data)[names(timeseries_data) != "Time"]
        for (col in numeric_cols) {
          if (is.character(timeseries_data[[col]])) {
            # Try to convert to numeric
            numeric_version <- suppressWarnings(as.numeric(timeseries_data[[col]]))
            if (!all(is.na(numeric_version))) {
              timeseries_data[[col]] <- numeric_version
            }
          }
        }
        
        return(timeseries_data)
        
      }, error = function(e) {
        unlink(temp_file)
        
        # Return basic dataframe if parsing fails
        timeseries_data <- data.frame(
          Line = 1:length(section_lines),
          Content = section_lines,
          stringsAsFactors = FALSE
        )
        
        return(timeseries_data)
      })
    }
    
    # Function to parse tabular data - Robust version using readr
    parse_tabular_section <- function(section_lines) {
      
      if (length(section_lines) < 2) {
        return(data.frame())
      }
      
      # Create temporary file for reading
      temp_file <- tempfile()
      writeLines(section_lines, temp_file)
      
      # Try using readr's read_tsv which handles special characters better
      tryCatch({
        library(readr)
        
        # First try tab-delimited
        metadata <- read_tsv(temp_file, 
                             col_types = cols(.default = col_character()),
                             show_col_types = FALSE,
                             na = c("", "NA"),
                             locale = locale(encoding = "UTF-8"))
        
        unlink(temp_file)
        return(as.data.frame(metadata))
        
      }, error = function(e) {
        
        # If tab fails, try comma-delimited
        tryCatch({
          metadata <- read_csv(temp_file,
                               col_types = cols(.default = col_character()),
                               show_col_types = FALSE,
                               na = c("", "NA"),
                               locale = locale(encoding = "UTF-8"))
          
          unlink(temp_file)
          return(as.data.frame(metadata))
          
        }, error = function(e2) {
          
          # Fallback to base R with more permissive settings
          tryCatch({
            # Try with readLines and manual parsing
            lines <- readLines(temp_file, encoding = "UTF-8")
            unlink(temp_file)
            
            if (length(lines) < 2) {
              return(data.frame())
            }
            
            # Split by tabs
            split_lines <- strsplit(lines, "\t", fixed = TRUE)
            
            # Get headers
            headers <- split_lines[[1]]
            
            # Create data frame
            data_list <- list()
            for (i in 1:length(headers)) {
              col_data <- sapply(split_lines[-1], function(x) {
                if (length(x) >= i) x[i] else NA
              })
              data_list[[headers[i]]] <- col_data
            }
            
            return(as.data.frame(data_list, stringsAsFactors = FALSE))
            
          }, error = function(e3) {
            unlink(temp_file)
            
            # Final fallback
            return(data.frame(
              Line = 1:length(section_lines),
              Content = section_lines,
              stringsAsFactors = FALSE
            ))
          })
        })
      })
    }
    
    # Function to process data
    parse_peach_file <- function(file_path) {
      
      # Read the entire file
      raw_lines <- readLines(file_path, warn = FALSE)
      
      # Find section breaks (lines starting with "=====")
      section_breaks <- which(grepl("^=====", raw_lines))
      
      # Initialize list to store all dataframes
      peach_data <- list()
      
      # Process each section
      for (i in 1:length(section_breaks)) {
        
        # Extract section name
        section_line <- raw_lines[section_breaks[i]]
        section_name <- trimws(gsub("^=====\\s*", "", section_line))
        
        # Determine start and end lines for this section
        start_line <- section_breaks[i] + 1
        
        if (i < length(section_breaks)) {
          end_line <- section_breaks[i + 1] - 1
        } else {
          end_line <- length(raw_lines)
        }
        
        # Extract section data
        section_lines <- raw_lines[start_line:end_line]
        
        # Remove empty lines
        section_lines <- section_lines[section_lines != ""]
        
        # Skip if no data in section
        if (length(section_lines) == 0) {
          next
        }
        
        # Parse based on section type
        if (section_name == "File Info") {
          
          # Parse file info section
          metadata <- parse_tabular_section(section_lines)
          peach_data[["file_info"]] <- metadata
          
        } else if (section_name == "GPS Info") {
          
          metadata <- parse_tabular_section(section_lines)
          peach_data[["gps_info"]] <- metadata
          
        } else if (section_name == "Crew Info") {
          
          metadata <- parse_tabular_section(section_lines)
          peach_data[["crew_info"]] <- metadata
          
        } else if (section_name == "Rig Info") {
          
          metadata <- parse_tabular_section(section_lines)
          peach_data[["rig_info"]] <- metadata
          
        } else if (section_name == "Venue Info") {
          
          metadata <- parse_tabular_section(section_lines)
          peach_data[["venue_info"]] <- metadata
          
        } else if (section_name == "Misc Info") {
          
          metadata <- parse_tabular_section(section_lines)
          peach_data[["misc_info"]] <- metadata
          
        } else if (section_name == "Boat Info") {
          
          metadata <- parse_tabular_section(section_lines)
          peach_data[["boat_info"]] <- metadata
          
        } else if (section_name == "Parameter Info") {
          
          metadata <- parse_tabular_section(section_lines)
          peach_data[["parameter_info"]] <- metadata
          
        } else if (section_name == "Sensor Info") {
          
          metadata <- parse_tabular_section(section_lines)
          peach_data[["sensor_info"]] <- metadata
          
        } else if (section_name == "Piece") {
          
          metadata <- parse_tabular_section(section_lines)
          peach_data[["piece_info"]] <- metadata
          
        } else if (section_name == "Intervals") {
          
          # Skip intervals section - not needed
          next
          
        } else if (grepl("Periodic", section_name)) {
          
          # Parse periodic time series data (high-frequency measurements)
          periodic <- parse_time_series_section(section_lines)
          peach_data[["periodic"]] <- periodic
          
        } else if (grepl("Aperiodic", section_name)) {
          
          # Parse aperiodic time series data
          # Extract the hex identifier if present
          hex_id <- gsub(".*\\s+(0x[0-9A-Fa-f]+).*", "\\1", section_name)
          
          aperiodic <- parse_time_series_section(section_lines)
          
          # Create meaningful names for aperiodic sections
          if (hex_id == "0x8013") {
            section_key <- "aperiodic_boat"
          } else if (hex_id == "0x800A") {
            section_key <- "aperiodic_crew"
          } else {
            # Fallback for unknown hex IDs
            section_key <- paste0("aperiodic_", hex_id)
          }
          
          peach_data[[section_key]] <- aperiodic
          
        } else {
          
          # Generic tabular parsing for unknown sections
          metadata <- parse_tabular_section(section_lines)
          
          # Create safe dataframe name
          safe_name <- tolower(gsub("[^A-Za-z0-9_]", "_", section_name))
          safe_name <- gsub("_+", "_", safe_name)  # Remove multiple underscores
          safe_name <- gsub("^_|_$", "", safe_name)  # Remove leading/trailing underscores
          
          peach_data[[safe_name]] <- metadata
        }
      }
      
      return(peach_data)
    }
    
    # Process uploaded file
    observeEvent(input$telemetryFile, {
        
        # Reset values
        values$processedData <- NULL
        values$fileName <- NULL
        values$status <- "processing"
        values$error <- NULL
        
        # Get file info
        file <- input$telemetryFile
        if (is.null(file)) return(NULL)
        
        tryCatch({
            # Parse the file
            parsed_data <- parse_peach_file(file$datapath)
            
            # Extract components
            file_info <- parsed_data$file_info
            gps_info <- parsed_data$gps_info
            crew_info <- parsed_data$crew_info
            rig_info <- parsed_data$rig_info
            venue_info <- parsed_data$venue_info
            misc_info <- parsed_data$misc_info
            boat_info <- parsed_data$boat_info
            parameter_info <- parsed_data$parameter_info
            sensor_info <- parsed_data$sensor_info
            piece_info <- parsed_data$piece_info
            aperiodic_boat <- parsed_data$aperiodic_boat  # GPS and boat data (0x8013)
            aperiodic <- parsed_data$aperiodic_crew  # Stroke analysis data (0x800A)
            periodic <- parsed_data$periodic
            
            # Clean metadata
            boat_info <- select(boat_info, `Boat Name`, Seats, Coxed, Rig, Manufacturer)
            colnames(boat_info) <- c("crew", "seats", "coxed", "rig", "manufacturer")
            
            crew_info <- select(crew_info, Position, Name)
            colnames(crew_info) <- c("position", "name")
            
            file_info <- select(file_info, Filename, `Start Time`)
            colnames(file_info) <- c("filename", "date")
            file_info$date <- format(as.Date(file_info$date, format = "%a, %d %b %Y"), 
                                     "%A, %dth of %B %Y")
            
            gps_info <- select(gps_info, Lat, Lon, UTC)
            colnames(gps_info) <- c("latitude", "longitude", "time_utc")
            
            gps_info <- gps_info %>%
                reverse_geocode(lat = latitude, long = longitude, method = "osm") %>%
                rename(location = address) %>%
                mutate(
                    # Parse the UTC time string and set timezone
                    time_utc_parsed = dmy_hms(str_remove(time_utc, " \\(UTC\\)"), tz = "UTC"),
                    # Convert to London time (automatically handles GMT/BST)
                    time_london = with_tz(time_utc_parsed, tzone = "Europe/London")
                ) %>%
                # Remove original string version and rename parsed version
                select(-time_utc) %>%
                rename(time_utc = time_utc_parsed) %>%
                relocate(time_utc, .before = time_london)
            
            parameter_info <- select(parameter_info, Parameter, Value, Units)
            colnames(parameter_info) <- c("parameter", "value", "units")
            
            piece_info <- select(piece_info, Start, End, `#`, Duration, Distance, Rating, Pace)
            colnames(piece_info) <- c("start_time", "end_time", "description", "duration", "distance", "rate", "pace")
            
            rig_info <- select(rig_info, Position)
            rig_info <- rig_info %>% slice(-1)
            rig_info$seat <- 1:8
            rig_info <- select(rig_info, seat, Position)
            colnames(rig_info) <- c("seat", "side")
            rig_info <- rig_info %>%
                mutate(side_uk = case_when(
                    side == "Port" ~ "Stroke",
                    side == "Stbd" ~ "Bow",
                    TRUE ~ NA_character_  # For any unexpected values
                ))
            
            venue_info <- select(venue_info, VenueName)
            colnames(venue_info) <- c("venue")
            
            misc_info <- select(misc_info, SessionComments)
            colnames(misc_info) <- c("comments")
            
            # Clean aperiodic data
            # Remove first two rows
            aperiodic <- aperiodic %>% slice(-1, -2)
            
            # Remove duplicate data (keeps first occurrence)
            aperiodic <- aperiodic[, !duplicated(t(aperiodic))]
            
            aperiodic <- aperiodic %>% 
                select_if(~ !all(is.na(.)))
            
            aperiodic_boat <- select(aperiodic, "Time", "StrokeNumber", "AvgBoatSpeed", "Rating", "Dist.Stroke", "Average.Power")
            
            colnames(aperiodic_boat) <- c("time", "stroke", "speed", "rate", "distance_per_stroke", "boat_power")
            
            # Remove rows with the lowest stroke number
            aperiodic_boat <- aperiodic_boat %>%
                filter(stroke != min(stroke, na.rm = TRUE))
            
            aperiodic <- select(aperiodic, "Time", "StrokeNumber", "SwivelPower", "SwivelPower.1", "SwivelPower.2", "SwivelPower.3", "SwivelPower.4", "SwivelPower.5", "SwivelPower.6", "SwivelPower.7", "Max.Force.PC", "Max.Force.PC.1", "Max.Force.PC.2", "Max.Force.PC.3", "Max.Force.PC.4", "Max.Force.PC.5", "Max.Force.PC.6", "Max.Force.PC.7", "Angle.0.7.F", "Angle.0.7.F.1", "Angle.0.7.F.2", "Angle.0.7.F.3", "Angle.0.7.F.4", "Angle.0.7.F.5", "Angle.0.7.F.6", "Angle.0.7.F.7", "Work.PC.Q1", "Work.PC.Q1.1", "Work.PC.Q1.2", "Work.PC.Q1.3", "Work.PC.Q1.4", "Work.PC.Q1.5", "Work.PC.Q1.6", "Work.PC.Q1.7", "Work.PC.Q2", "Work.PC.Q2.1", "Work.PC.Q2.2", "Work.PC.Q2.3", "Work.PC.Q2.4", "Work.PC.Q2.5", "Work.PC.Q2.6", "Work.PC.Q2.7", "Work.PC.Q3", "Work.PC.Q3.1", "Work.PC.Q3.2", "Work.PC.Q3.3", "Work.PC.Q3.4", "Work.PC.Q3.5", "Work.PC.Q3.6", "Work.PC.Q3.7", "Work.PC.Q4", "Work.PC.Q4.1", "Work.PC.Q4.2", "Work.PC.Q4.3", "Work.PC.Q4.4", "Work.PC.Q4.5", "Work.PC.Q4.6", "Work.PC.Q4.7", "MinAngle", "MinAngle.1", "MinAngle.2", "MinAngle.3", "MinAngle.4", "MinAngle.5", "MinAngle.6", "MinAngle.7", "CatchSlip", "CatchSlip.1", "CatchSlip.2", "CatchSlip.3", "CatchSlip.4", "CatchSlip.5", "CatchSlip.6", "CatchSlip.7", "MaxAngle", "MaxAngle.1", "MaxAngle.2", "MaxAngle.3", "MaxAngle.4", "MaxAngle.5", "MaxAngle.6", "MaxAngle.7", "FinishSlip", "FinishSlip.1", "FinishSlip.2", "FinishSlip.3", "FinishSlip.4", "FinishSlip.5", "FinishSlip.6", "FinishSlip.7")
            
            colnames(aperiodic) <- c("time", "stroke", "power_seat_1", "power_seat_2", "power_seat_3", "power_seat_4", "power_seat_5", "power_seat_6", "power_seat_7", "power_seat_8",  "work_max_seat_1", "work_max_seat_2", "work_max_seat_3", "work_max_seat_4", "work_max_seat_5", "work_max_seat_6", "work_max_seat_7", "work_max_seat_8", "angle_70_seat_1", "angle_70_seat_2", "angle_70_seat_3", "angle_70_seat_4", "angle_70_seat_5", "angle_70_seat_6", "angle_70_seat_7", "angle_70_seat_8", "work_q1_seat_1", "work_q1_seat_2", "work_q1_seat_3", "work_q1_seat_4", "work_q1_seat_5", "work_q1_seat_6", "work_q1_seat_7", "work_q1_seat_8",  "work_q2_seat_1", "work_q2_seat_2", "work_q2_seat_3", "work_q2_seat_4", "work_q2_seat_5", "work_q2_seat_6", "work_q2_seat_7", "work_q2_seat_8",  "work_q3_seat_1", "work_q3_seat_2", "work_q3_seat_3", "work_q3_seat_4", "work_q3_seat_5", "work_q3_seat_6", "work_q3_seat_7", "work_q3_seat_8",  "work_q4_seat_1", "work_q4_seat_2", "work_q4_seat_3", "work_q4_seat_4", "work_q4_seat_5", "work_q4_seat_6", "work_q4_seat_7", "work_q4_seat_8", "angle_minimum_seat_1", "angle_minimum_seat_2", "angle_minimum_seat_3", "angle_minimum_seat_4", "angle_minimum_seat_5", "angle_minimum_seat_6", "angle_minimum_seat_7", "angle_minimum_seat_8", "slip_catch_seat_1", "slip_catch_seat_2", "slip_catch_seat_3", "slip_catch_seat_4", "slip_catch_seat_5", "slip_catch_seat_6", "slip_catch_seat_7", "slip_catch_seat_8", "angle_maximum_seat_1", "angle_maximum_seat_2", "angle_maximum_seat_3", "angle_maximum_seat_4", "angle_maximum_seat_5", "angle_maximum_seat_6", "angle_maximum_seat_7", "angle_maximum_seat_8", "slip_finish_seat_1", "slip_finish_seat_2", "slip_finish_seat_3", "slip_finish_seat_4", "slip_finish_seat_5", "slip_finish_seat_6", "slip_finish_seat_7", "slip_finish_seat_8")
            
            aperiodic_long <- aperiodic %>%
                pivot_longer(
                    cols = -c(time, stroke),  # Keep both time and stroke as is
                    names_to = c(".value", "seat"),
                    names_pattern = "(.+)_seat_(.+)")
            
            # Remove rows with the lowest stroke number
            aperiodic_long <- aperiodic_long %>%
                filter(stroke != min(stroke, na.rm = TRUE))
            
            # Format data
            distance_per_stroke <- aperiodic_boat %>%
                summarise(distance_per_stroke = mean(distance_per_stroke, na.rm = TRUE), .groups = 'drop')
            
            speed <- aperiodic_boat %>%
                summarise(speed = mean(speed, na.rm = TRUE), .groups = 'drop')
            
            power <- aperiodic_long %>%
                group_by(seat) %>%
                summarise(power = mean(power, na.rm = TRUE), .groups = 'drop')
            
            power_stroke <- power
            power_stroke$side_uk <- rig_info$side_uk
            power_stroke$power_stroke <- ifelse(power_stroke$side_uk == "Stroke", power_stroke$power, NA)
            power_stroke <- select(power_stroke, seat, power_stroke)
            
            power_bow <- power
            power_bow$side_uk <- rig_info$side_uk
            power_bow$power_bow <- ifelse(power_bow$side_uk == "Bow", power_bow$power, NA)
            power_bow <- select(power_bow, seat, power_bow)
            
            work_max_percent <- aperiodic_long %>%
                group_by(seat) %>%
                summarise(work_max_percent = mean(work_max, na.rm = TRUE), .groups = 'drop')
            
            angle_70_percent <- aperiodic_long %>%
                group_by(seat) %>%
                summarise(angle_70_percent = abs(mean(angle_70, na.rm = TRUE)), .groups = 'drop')
            
            work_q1_percent <- aperiodic_long %>%
                group_by(seat) %>%
                summarise(work_q1_percent = mean(work_q1, na.rm = TRUE), .groups = 'drop')
            
            work_q2_percent <- aperiodic_long %>%
                group_by(seat) %>%
                summarise(work_q2_percent = mean(work_q2, na.rm = TRUE), .groups = 'drop')
            
            work_q3_percent <- aperiodic_long %>%
                group_by(seat) %>%
                summarise(work_q3_percent = mean(work_q3, na.rm = TRUE), .groups = 'drop')
            
            work_q4_percent <- aperiodic_long %>%
                group_by(seat) %>%
                summarise(work_q4_percent = mean(work_q4, na.rm = TRUE), .groups = 'drop')
            
            angle_catch <- aperiodic_long %>%
                group_by(seat) %>%
                summarise(angle_catch = abs(mean(angle_minimum, na.rm = TRUE)), .groups = 'drop')
            
            angle_finish <- aperiodic_long %>%
                group_by(seat) %>%
                summarise(angle_finish = abs(mean(angle_maximum, na.rm = TRUE)), .groups = 'drop')
            
            slip_catch <- aperiodic_long %>%
                group_by(seat) %>%
                summarise(slip_catch = mean(slip_catch, na.rm = TRUE), .groups = 'drop')
            
            slip_finish <- aperiodic_long %>%
                group_by(seat) %>%
                summarise(slip_finish = mean(slip_finish, na.rm = TRUE), .groups = 'drop')
            
            data <- crew_info %>% 
                select(., seat = position, name) %>%
                filter(seat != "Coach" & seat != "Cox") %>%
                mutate(
                    session = NA,
                    day = wday(gps_info$time_london, label = TRUE, abbr = TRUE),
                    date = date(gps_info$time_london),
                    type = piece_info$description,
                    boat = case_when(
                        boat_info$rig == "scull" & boat_info$coxed == "yes" ~ paste0(boat_info$seat, "x+"),
                        boat_info$rig == "scull" & boat_info$coxed == "no" ~ paste0(boat_info$seat, "x"),
                        boat_info$rig == "sweep" & boat_info$coxed == "yes" ~ paste0(boat_info$seat, "+"),
                        boat_info$rig == "sweep" & boat_info$coxed == "no" ~ paste0(boat_info$seat, "-"),
                        TRUE ~ NA_character_),
                    duration = piece_info$duration,
                    rate = piece_info$rate,
                    distance = piece_info$distance,
                    distance_per_stroke = distance_per_stroke$distance_per_stroke,
                    speed = speed$speed,
                    pace = piece_info$pace) %>%
                left_join(power, by = "seat") %>%
                left_join(power_stroke, by = "seat") %>%
                left_join(power_bow, by = "seat") %>%
                left_join(work_max_percent, by = "seat") %>%
                left_join(angle_70_percent, by = "seat") %>%
                left_join(work_q1_percent, by = "seat") %>%
                left_join(work_q2_percent, by = "seat") %>%
                left_join(work_q3_percent, by = "seat") %>%
                left_join(work_q4_percent, by = "seat") %>%
                left_join(angle_catch, by = "seat") %>%
                left_join(angle_finish, by = "seat") %>%
                left_join(slip_catch, by = "seat") %>%
                left_join(slip_finish, by = "seat")
            
            data$angle_70_percent_benchmark <- data$angle_catch - 10
            data$angle_70_percent_difference <- data$angle_70_percent_benchmark - data$angle_70_percent
            
            # work q1 benchmark is constant = 18
            data$work_q1_percent_difference <- data$work_q1_percent - 18
            
            # work q2 benchmark is constant = 37
            data$work_q2_percent_difference <- data$work_q2_percent - 37
            
            # work q3 benchmark is constant = 34
            data$work_q3_percent_difference <- data$work_q3_percent - 34
            
            # work q4 benchmark is constant = 11
            data$work_q4_percent_difference <- data$work_q4_percent - 11
            
            data$slip_total <- data$slip_catch + data$slip_finish
            
            data$length <- data$angle_catch + data$angle_finish
            
            data$effective_length <- data$length - data$slip_total
            
            data$effective_length_percent <- (data$effective_length / data$length) * 100
            
            # Arrange data according to spreadsheet
            data <- data %>% select(., seat, name, session, day, date, type, boat, duration, rate, distance, distance_per_stroke, speed, pace, power_stroke, power, power_bow, work_max_percent, angle_70_percent, angle_70_percent_benchmark, angle_70_percent_difference, work_q1_percent, work_q1_percent_difference, work_q2_percent, work_q2_percent_difference, work_q3_percent, work_q3_percent_difference, work_q4_percent, work_q4_percent_difference, angle_catch, angle_finish, length, effective_length, effective_length_percent, slip_catch, slip_finish, slip_total)
            
            # Round data according to spreadsheet
            data <- data %>%
                mutate(
                    # Round to 2 decimal places
                    distance_per_stroke = round(distance_per_stroke, 2),
                    speed = round(speed, 2),
                    effective_length_percent = round(effective_length_percent, 2),
                    
                    # Round to nearest integer (0 decimal places)
                    power_stroke = round(power_stroke, 0),
                    power = round(power, 0),
                    power_bow = round(power_bow, 0),
                    
                    # Round to 1 decimal place
                    work_max_percent = round(work_max_percent, 1),
                    angle_70_percent = round(angle_70_percent, 1),
                    angle_70_percent_benchmark = round(angle_70_percent_benchmark, 1),
                    angle_70_percent_difference = round(angle_70_percent_difference, 1),
                    work_q1_percent = round(work_q1_percent, 1),
                    work_q1_percent_difference = round(work_q1_percent_difference, 1),
                    work_q2_percent = round(work_q2_percent, 1),
                    work_q2_percent_difference = round(work_q2_percent_difference, 1),
                    work_q3_percent = round(work_q3_percent, 1),
                    work_q3_percent_difference = round(work_q3_percent_difference, 1),
                    work_q4_percent = round(work_q4_percent, 1),
                    work_q4_percent_difference = round(work_q4_percent_difference, 1),
                    angle_catch = round(angle_catch, 1),
                    angle_finish = round(angle_finish, 1),
                    length = round(length, 1),
                    effective_length = round(effective_length, 1),
                    slip_catch = round(slip_catch, 1),
                    slip_finish = round(slip_finish, 1),
                    slip_total = round(slip_total, 1)
                )
            
            # Generate filename
            formatted_string <- gps_info$time_utc %>%
                ymd_hms() %>%
                format("%Y_%m_%d_%H_%M_%S") %>%
                paste0("data_", ., ".csv")
            
            # Store results
            values$processedData <- data
            values$fileName <- formatted_string
            values$status <- "success"
            
        }, error = function(e) {
            values$status <- "error"
            values$error <- paste("Error processing file:", e$message)
        })
    })
    
    # File info display
    output$fileInfo <- renderUI({
        file <- input$telemetryFile
        if (is.null(file)) {
            return(p("No file uploaded"))
        }
        
        div(
            p(strong("Name:"), file$name),
            p(strong("Size:"), paste(round(file$size / 1024, 2), "KB"))
        )
    })
    
    # Processing status display
    output$processingStatus <- renderUI({
        if (is.null(values$status)) {
            return(NULL)
        }
        
        if (values$status == "processing") {
            div(class = "processing",
                h5("Processing..."),
                p("Please wait while the file is being processed."))
        } else if (values$status == "success") {
            div(class = "success",
                h5("Success!"),
                p("File processed successfully."),
                p(strong("Output filename:"), values$fileName))
        } else if (values$status == "error") {
            div(class = "error",
                h5("Error"),
                p(values$error))
        }
    })
    
    # Download section
    output$downloadSection <- renderUI({
        if (!is.null(values$processedData) && values$status == "success") {
            div(
                p("Your processed data is ready for download:"),
                downloadButton("downloadData", "Download CSV", class = "btn-primary")
            )
        }
    })
    
    # Data preview table
    output$dataPreview <- DT::renderDataTable({
        if (!is.null(values$processedData)) {
            DT::datatable(values$processedData,
                          options = list(
                              pageLength = 10,
                              scrollX = TRUE
                          ))
        }
    })
    
    # Download handler
    output$downloadData <- downloadHandler(
        filename = function() {
            values$fileName
        },
        content = function(file) {
            write_csv(values$processedData, file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)