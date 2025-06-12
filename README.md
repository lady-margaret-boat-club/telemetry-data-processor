# LMBC Telemetry Processor

A user-friendly web application for processing rowing telemetry data files. Upload your telemetry .txt files and download comprehensive performance analysis as CSV files.

## What This App Does

This application takes telemetry data files from rowing sessions and converts them into easy-to-read spreadsheets with detailed performance metrics for each crew member. The app calculates:

- **Power metrics** - Overall power output, stroke-side and bow-side power
- **Work distribution** - How power is distributed across the stroke (Q1-Q4 percentages)
- **Angle measurements** - Catch angles, finish angles, and stroke length
- **Slip analysis** - Catch slip, finish slip, and total slip
- **Efficiency metrics** - Effective length and stroke effectiveness

## How to Use the App

### Step 1: Getting Started
1. Open the application in your web browser
2. You'll see the "LMBC Telemetry Processor" homepage with an upload area on the left and a preview area on the right

### Step 2: Upload Your Data File
1. Click the **"Choose .txt file"** button in the sidebar
2. Select your telemetry data file from your computer
   - The file must be a `.txt` file from your rowing telemetry system
   - Files can be up to 30MB in size
3. Once selected, you'll see the file name and size displayed

### Step 3: Processing
1. The app will automatically start processing your file
2. You'll see a yellow "Processing..." message while the app works
3. This usually takes 10-30 seconds depending on file size

### Step 4: View Results
1. When processing is complete, you'll see a green "Success!" message
2. A data preview table will appear showing your processed results
3. Each row represents one crew member with all their performance metrics

### Step 5: Download Your Results
1. Click the blue **"Download CSV"** button
2. Save the file to your computer
3. The filename will be automatically generated with the date and time (e.g., `data_2024_12_06_14_30_15.csv`)

## Understanding Your Results

Your downloaded CSV file contains 36 columns of data for each crew member:

### Basic Information
- **seat** - Seat position (1-8)
- **name** - Crew member name
- **day/date** - When the session took place
- **type** - Type of rowing piece
- **boat** - Boat configuration (e.g., "8+", "4-")

### Session Metrics
- **duration** - Length of the piece
- **rate** - Stroke rate (strokes per minute)
- **distance** - Total distance covered
- **speed** - Average boat speed
- **pace** - Time per 500m

### Power Analysis
- **power** - Overall power output (watts)
- **power_stroke** - Power for stroke-side rowers (watts)
- **power_bow** - Power for bow-side rowers (watts)

### Work Distribution (with benchmarks)
- **work_q1_percent** - Power in first quarter of stroke (benchmark: 18%)
- **work_q2_percent** - Power in second quarter (benchmark: 37%)
- **work_q3_percent** - Power in third quarter (benchmark: 34%)
- **work_q4_percent** - Power in fourth quarter (benchmark: 11%)
- Each work metric includes a "difference" column showing how you compare to the benchmark

### Technique Analysis
- **angle_catch** - Catch angle (degrees)
- **angle_finish** - Finish angle (degrees)
- **angle_70_percent** - Angle at 70% of maximum force
- **length** - Total stroke length
- **effective_length** - Useful stroke length (after accounting for slip)

### Slip Analysis
- **slip_catch** - Blade slip at catch
- **slip_finish** - Blade slip at finish
- **slip_total** - Total slip during stroke

## Troubleshooting

### File Won't Upload
- **Check file format**: Only `.txt` files are accepted
- **Check file size**: Files must be under 30MB

### Processing Errors
- **Special characters**: The app handles special characters, but very unusual formatting might cause issues
- **Corrupted files**: Try re-exporting the file from your telemetry system
- **Missing data sections**: Some telemetry files may be incomplete

### No Data in Results
- **Check your file**: Ensure it contains the expected telemetry sections
- **Verify format**: File should have clear section breaks marked with "====="

## Technical Requirements

### For Users
- **Web browser**: Any modern browser (Chrome, Firefox, Safari, Edge)
- **Internet connection**: Required to run the application
- **File format**: Telemetry .txt files with standard formatting

### File Format Requirements
Your telemetry file should contain sections like:
- File Info
- GPS Info  
- Crew Info
- Boat Info
- Piece Info
- Aperiodic data (crew performance metrics)
- Periodic data (time-series measurements)

## Getting Help

If you encounter issues:

1. **Check your file format** - Ensure it's a proper telemetry .txt file
2. **Try a different file** - Test with a known working file
3. **Refresh the page** - Sometimes a browser refresh helps
4. **Check file size** - Very large files (>30MB) won't work

## Tips for Best Results

- **Regular backups** - Keep copies of your original telemetry files
- **Consistent naming** - Use consistent names for your crew members

## Data Privacy

- Files are processed temporarily and not permanently stored
- Your data remains private and is not shared
- Original files are not modified
- Processed results are only accessible to you

---

*This application processes LMBC telemetry data to provide comprehensive rowing performance analysis. For technical support or feature requests, please contact your system administrator.*