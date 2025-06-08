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
        "))
    ),
    
    # Application header
    div(class = "header",
        h2("LMBC Telemetry Processor"),
        p("Upload a telemetry .txt file to process and download the results as CSV")
    ),
    
    # Main content
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
    
    # Parse telemetry functions (from your original script)
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
                                          na.strings = c("", "NA"))
            
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
    
    parse_tabular_section <- function(section_lines) {
        
        if (length(section_lines) < 2) {
            return(data.frame())
        }
        
        # Create temporary file for reading
        temp_file <- tempfile()
        writeLines(section_lines, temp_file)
        
        # Try to read as tab-delimited
        tryCatch({
            metadata <- read.table(temp_file, 
                                   sep = "\t", 
                                   header = TRUE, 
                                   stringsAsFactors = FALSE,
                                   fill = TRUE,
                                   blank.lines.skip = TRUE,
                                   comment.char = "")
            
            # Clean up temp file
            unlink(temp_file)
            
            return(metadata)
            
        }, error = function(e) {
            
            # If tab-delimited fails, try comma-delimited
            tryCatch({
                periodic <- read.table(temp_file, 
                                       sep = ",", 
                                       header = TRUE, 
                                       stringsAsFactors = FALSE,
                                       fill = TRUE,
                                       blank.lines.skip = TRUE,
                                       comment.char = "")
                
                unlink(temp_file)
                return(periodic)
                
            }, error = function(e2) {
                
                # If both fail, create simple dataframe from lines
                unlink(temp_file)
                
                periodic <- data.frame(
                    Line = 1:length(section_lines),
                    Content = section_lines,
                    stringsAsFactors = FALSE
                )
                
                return(periodic)
            })
        })
    }
    
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
            aperiodic_boat <- parsed_data$aperiodic_boat
            aperiodic <- parsed_data$aperiodic_crew
            periodic <- parsed_data$periodic
            
            # Clean metadata
            boat_info <- select(boat_info, Boat.Name, Seats, Coxed, Rig, Manufacturer)
            colnames(boat_info) <- c("crew", "seats", "coxed", "rig", "manufacturer")
            
            crew_info <- select(crew_info, Position, Name)
            colnames(crew_info) <- c("position", "name")
            
            file_info <- select(file_info, Filename, Start.Time)
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
            
            piece_info <- select(piece_info, Start, End, X., Duration, Distance, Rating, Pace)
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
            aperiodic <- aperiodic %>% slice(-1, -2)
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
                    cols = -c(time, stroke),
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
