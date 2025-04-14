#?--------------------------------------------------------------------
# SECTION 1   : COURSE AND ASSIGNMENT DETAILS
# --------------------------------------------------------------------
# Course      : LIS-4370-Week-12
# Assignment  : Module 12 - Social Network Visualization
# URL         : https://usflearn.instructure.com/courses/1926966/assignments/17693281
# Filename    : LIS4370Week12code.R
# Purpose     : Create a social network visualization using ggnet2 in RStudio
# Author      : Steven Barden
# Email       : StevenBarden@usf.edu
# Created     : 2025-04-13-0600-00
# Updated     : 2025-04-14-0800-00
# License     : The Stuningly Free Unlicense
# Description : This script generates a social network visualization 
#             : using the ggnet2 package as part of Module 12 assignment.
#             : It creates a random network, visualizes it, and reports outcomes.
#             :

# --------------------------------------------------------------------
# SECTION 2: ENVIRONMENT SETUP
# --------------------------------------------------------------------

show_comments <- TRUE  # Set to FALSE to hide all instructional comments

# Set the base directory.
baseDir <- r"(C:\Users\Steve\OneDrive\College\_____DESKTOP ICONS\Remeye\Classes\4370\Mod12\)"

# Ensure and set the working directory.
tryCatch({
  print(paste("Current working directory:", getwd()))
  if (!dir.exists(baseDir)) stop("Directory does not exist: ", baseDir)
  setwd(baseDir)
  print(paste("Working directory successfully set to:", baseDir))
}, error = function(e) {
  stop("Directory setup failed: ", e$message)
})

# Ensure Output Width for Terminal Display (Optional, Unix-based Systems)
tryCatch({
  options(width = 80) # Adjust width as needed
}, error = function(e) {
  print("Could not set terminal width.")
})

# --------------------------------------------------------------------
# SECTION 3: DEPENDENCIES & INSTALLATION
# --------------------------------------------------------------------

# Required Libraries
required_packages <- c("GGally", "network", "sna", "ggplot2")

# Initialize log file
log_file <- file.path(baseDir, "script_log.txt")
write_log <- function(message) {
  cat(paste0("[", Sys.time(), "] ", message, "\n"), file = log_file, append = TRUE)
}

# Check, Install, and Load Required Libraries
tryCatch({
  write_log("Checking and loading packages")
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
      cat("Installing package:", pkg, "\n")
      write_log(paste("Installing package:", pkg))
      install.packages(pkg, dependencies = TRUE)
      if (!require(pkg, character.only = TRUE)) {
        write_log(paste("Failed to load package after installation:", pkg))
        stop("Failed to load package after installation: ", pkg)
      }
      cat("Successfully loaded:", pkg, "\n")
      write_log(paste("Successfully loaded:", pkg))
    } else {
      cat("Package already loaded:", pkg, "\n")
      write_log(paste("Package already loaded:", pkg))
    }
  }
}, error = function(e) {
  write_log(paste("Library setup failed:", e$message))
  stop("Library setup failed: ", e$message)
})

# --------------------------------------------------------------------
# SECTION 4: DATA UTILITY FUNCTIONS
# --------------------------------------------------------------------

# Function to Create Sample Network
create_network <- function(nodes = 10, prob = 0.5) {
  if (show_comments) {
    cat("Creating a random network with", nodes, "nodes...\n")
  }
  tryCatch({
    write_log("Creating network")
    net <- sna::rgraph(nodes, mode = "graph", tprob = prob)
    net <- network::network(net, directed = FALSE)
    network::network.vertex.names(net) <- letters[1:nodes]
    write_log("Network created successfully")
    return(net)
  }, error = function(e) {
    write_log(paste("Error creating network:", e$message))
    stop("Error creating network: ", e$message)
  })
}

# Function to Check Network Validity
check_network <- function(net) {
  if (show_comments) {
    cat("Checking network validity...\n")
  }
  tryCatch({
    write_log("Checking network validity")
    vertex_count <- network::network.size(net)
    edge_count <- network::network.edgecount(net)
    if (vertex_count == 0) {
      write_log("Network has no vertices")
      stop("Network has no vertices.")
    }
    if (show_comments) {
      cat("Network has", vertex_count, "vertices and", edge_count, "edges.\n")
    }
    write_log(paste("Network valid:", vertex_count, "vertices,", edge_count, "edges"))
    return(list(vertices = vertex_count, edges = edge_count))
  }, error = function(e) {
    write_log(paste("Error checking network:", e$message))
    stop("Error checking network: ", e$message)
  })
}

# Function to Summarize Network
summarize_network <- function(net) {
  if (show_comments) {
    cat("Summarizing network properties...\n")
  }
  tryCatch({
    write_log("Summarizing network")
    stats <- sna::degree(net, gmode = "graph")
    cat("Degree distribution:\n")
    print(summary(stats))
    cat("Number of vertices:", network::network.size(net), "\n")
    cat("Number of edges:", network::network.edgecount(net), "\n")
    write_log("Network summarized successfully")
    return(invisible(NULL))
  }, error = function(e) {
    write_log(paste("Error summarizing network:", e$message))
    stop("Error summarizing network: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 5: DATA I/O HANDLERS
# --------------------------------------------------------------------

# Read a File Based on Format (CSV Only by Default)
read_data_file <- function(filePath, fileType = "csv") {
  tryCatch({
    write_log(paste("Reading file:", filePath))
    if (fileType == "csv") {
      data <- read.csv(filePath)
    } else {
      write_log(paste("Unsupported file type:", fileType))
      stop("Unsupported file type:", fileType)
    }
    if (show_comments) {
      cat("Successfully read", fileType, "file from", filePath, "\n")
    }
    write_log(paste("Successfully read", fileType, "file"))
    return(data)
  }, error = function(e) {
    write_log(paste("Error reading file:", e$message))
    stop("Error reading file: ", e$message)
  })
}

# Load Sample Data for Testing
load_sample_data <- function() {
  if (show_comments) {
    cat("Creating sample data...\n")
  }
  tryCatch({
    write_log("Creating sample data")
    data <- data.frame(
      category = c("A", "B", "C"),
      value = c(10, 20, 15)
    )
    write_log("Sample data created")
    return(data)
  }, error = function(e) {
    write_log(paste("Error creating sample data:", e$message))
    stop("Error creating sample data: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 6: DATA PROCESSING WORKFLOWS
# --------------------------------------------------------------------

# Process Network: Create and Validate
process_network <- function(nodes = 10, prob = 0.5) {
  tryCatch({
    if (show_comments) cat("Processing network pipeline started...\n")
    write_log("Starting network pipeline")
    net <- create_network(nodes, prob)
    check_network(net)
    summarize_network(net)
    if (show_comments) cat("Network pipeline completed.\n")
    write_log("Network pipeline completed")
    return(net)
  }, error = function(e) {
    write_log(paste("Error in network pipeline:", e$message))
    stop("Error in network pipeline: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 7: DATABASE OPERATIONS (SQLite Placeholder)
# --------------------------------------------------------------------

# Connect to SQLite Database
connect_sqlite <- function(dbPath) {
  if (show_comments) cat("Connecting to SQLite DB at:", dbPath, "\n")
  tryCatch({
    write_log(paste("Connecting to SQLite DB:", dbPath))
    dbConnection <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
    write_log("SQLite connection established")
    return(dbConnection)
  }, error = function(e) {
    write_log(paste("Error connecting to SQLite:", e$message))
    stop("Error connecting to SQLite: ", e$message)
  })
}

# CRUD Operations Placeholder
create_table <- function(dbConnection, schema) { }
insert_record <- function(dbConnection, tableName, recordData) { }
read_records <- function(dbConnection, query) { }
update_record <- function(dbConnection, tableName, condition, newValues) { }
delete_record <- function(dbConnection, tableName, condition) { }

# --------------------------------------------------------------------
# SECTION 8: ANALYSIS FUNCTIONS
# --------------------------------------------------------------------

analyze_network <- function(net) {
  if (show_comments) cat("Analyzing network...\n")
  tryCatch({
    write_log("Analyzing network")
    degree_dist <- sna::degree(net, gmode = "graph")
    analysis_result <- data.frame(
      vertex = network::network.vertex.names(net),
      degree = degree_dist
    )
    if (show_comments) cat("Network analysis completed successfully.\n")
    write_log("Network analysis completed")
    return(analysis_result)
  }, error = function(e) {
    write_log(paste("Error during analysis:", e$message))
    stop("Error during analysis: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 9: VISUALIZATION FUNCTIONS
# --------------------------------------------------------------------

visualize_network <- function(net) {
  if (show_comments) cat("Creating network visualization...\n")
  tryCatch({
    write_log("Creating visualization")
    plot_object <- GGally::ggnet2(
      net,
      node.size = 6,
      node.color = "black",
      edge.size = 1,
      edge.color = "grey",
      label = TRUE
    ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Social Network Visualization",
        x = "X Coordinate",
        y = "Y Coordinate"
      )
    
    print(plot_object)
    if (show_comments) cat("Visualization completed successfully.\n")
    write_log("Visualization completed")
    return(invisible(plot_object))
  }, error = function(e) {
    write_log(paste("Error creating visualization:", e$message))
    stop("Error creating visualization: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 10: MAIN EXECUTION BLOCK
# --------------------------------------------------------------------

main <- function() {
  if (show_comments) cat("Starting script execution...\n")
  tryCatch({
    write_log("Starting main execution")
    if (show_comments) cat("Step 1: Creating network...\n")
    network_data <- process_network(nodes = 10, prob = 0.5)
    
    if (show_comments) cat("Step 2: Analyzing network...\n")
    analysis_output <- analyze_network(network_data)
    
    if (show_comments) cat("Step 3: Visualizing network...\n")
    visualize_network(network_data)
    
    if (show_comments) cat("Step 4: Saving results for blog report...\n")
    cat("Success: Network visualization created with ggnet2.\n")
    cat("Challenges: Initial setup of packages required troubleshooting.\n")
    
    if (show_comments) cat("Script execution completed successfully.\n")
    write_log("Main execution completed")
    return(invisible(NULL))
  }, error = function(e) {
    write_log(paste("Main execution failed:", e$message))
    cat("Failure: Script encountered an error - ", e$message, "\n")
    stop("Script execution failed: ", e$message)
  })
}

# Execute main function
write_log("Script started")
main()
write_log("Script ended")

# --------------------------------------------------------------------
# SECTION 11: VERSION HISTORY
# --------------------------------------------------------------------
# Version History:
# - Version 1.0 (2025-04-13-0600-00): Initial script for Module 12.
# - Version 1.1 (2025-04-14-0800-00): Adjusted visualization parameters.
# - Version 1.2 (2025-04-14-0900-00): Added error logging to all functions.
# - Version 1.3 (2025-04-14-1100-00): Reverted label size and plot saving changes.

# --------------------------------------------------------------------
# SECTION 12: ADDITIONAL NOTES
# --------------------------------------------------------------------

# Best Practices:
# - Ensure secure handling of API keys and credentials.
# - Keep code modular and organized for maintainability.
# - Validate data inputs to prevent unexpected errors.
# - Use consistent naming conventions for variables and functions.
# - Include appropriate documentation and comments.
# - Test functions with small datasets before full execution.
# - Check script_log.txt for detailed error messages.

# --------------------------------------------------------------------
# END OF TEMPLATE
# --------------------------------------------------------------------