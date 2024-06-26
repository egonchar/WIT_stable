##########################
##########################

# ANALYTICS FUNCTIONS

##########################

##########################
##########################

# COSINE MATRIX FUNCTIONS

##########################

# inside compute_cosine_similarity
cosine_similarity <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    return(NA)
  }
  
  # Check for equal length of x and y
  if (length(x) != length(y)) {
    stop("Vectors x and y must have the same length")
  }
  
  dot <- x * y
  dot_prod <- sum(dot, na.rm = TRUE)
  x_sq <- x^2
  y_sq <- y^2
  mag_x <- sqrt(sum(x_sq, na.rm = TRUE))
  mag_y <- sqrt(sum(y_sq, na.rm = TRUE))
  
  if (mag_x == 0 || mag_y == 0) {
    return(NA)
  }
  cosine_sim <- dot_prod / (mag_x * mag_y)
  return(invisible(cosine_sim))
}


############

cosine_similarity_matrix <- function(area_grouped_data) {
  imp_vectors <- area_grouped_data$imp_vector
  occ_titles <- area_grouped_data$OCC_CODE
  
  n_rows <- length(imp_vectors)
  cosine_sim <- matrix(NA, nrow = n_rows, ncol = n_rows, dimnames = list(occ_titles, occ_titles))
  
  # Create a parallel backend with available cores
  cl <- makeCluster(detectCores() - 1)
  clusterExport(cl, "cosine_similarity")
  registerDoParallel(cl)

  # Parallel computation
  results <- foreach(i = 1:n_rows, .combine = 'rbind') %dopar% {
    row_results <- rep(NA, n_rows)
    for (j in 1:n_rows) {
      if (i == j) {
        row_results[j] <- NA
      } else if (j > i) {
        row_results[j] <- cosine_similarity(imp_vectors[[i]], imp_vectors[[j]])
      }
    }
    return(row_results)
  }
  
  # Fill the lower triangle of the matrix
  for (i in 1:n_rows) {
    for (j in 1:i) {
      cosine_sim[j, i] <- results[j, i]
      cosine_sim[i, j] <- results[j, i]
    }
  }
  
  # Stop the cluster
  stopCluster(cl)
  
  return(invisible(cosine_sim))
}


############

# Define function to generate a cosine matrix if user specifies weighted and an empty matrix if user specifies no weighting
# An empty matrix is generated but never used (only needed to run check_skill_thresholds)

compute_cosine_similarity_imp <- function(inputs, occupationMatrix, testVector) {
  
  weighted_imp <- inputs$weighted_imp
  soc_level <- inputs$soc_level
  
  # Ensure the testVector has an AREA column
  if (!"AREA" %in% colnames(testVector)) {
    testVector$AREA <- NA
  }
  
  if (weighted_imp == "yes") {
    occupationMatrix <- bind_rows(data.frame(occupationMatrix), data.frame(testVector))
    occupations <- unique(occupationMatrix$OCC_CODE)
    
    selected_data <- occupationMatrix %>%
      dplyr::select(AREA, Title, OCC_CODE, elementID, elementName, surveyValue_IM) %>%
      distinct()
    
    grouped_data <- selected_data %>%
      group_by(AREA, OCC_CODE) %>%
      summarize(imp_vector = list(surveyValue_IM), .groups = "drop") %>%
      ungroup()
    
    unique_areas <- unique(grouped_data$AREA)
    
    area_cosine_matrices <- list()
    
    # Determine the areas to process based on soc_level
    if (soc_level == "broad") {
      # Filter out the NA AREA value
      filtered_unique_areas <- unique_areas[!is.na(unique_areas)]
      
      areas_to_process <- filtered_unique_areas
    } else if (soc_level == "detailed") {
      areas_to_process <- 99
      filtered_unique_areas <- 99
    } else {
      stop("Invalid soc_level value. Please specify 'broad' or 'detailed'.")
    }
    
    for (area in areas_to_process) {
      area_grouped_data <- if (all(is.na(testVector$AREA))) {
        grouped_data %>%
          filter(AREA == area | OCC_CODE %in% testVector$OCC_CODE)
      } else {
        grouped_data %>%
          filter((AREA == area & !OCC_CODE %in% testVector$OCC_CODE) | (AREA == area & OCC_CODE %in% testVector$OCC_CODE))
      }
      
      area_cosine_matrix <- cosine_similarity_matrix(area_grouped_data)
      area_cosine_matrices[[as.character(area)]] <- area_cosine_matrix
    }
    
    # Add the resulting matrix to each unique_area if soc_level is "detailed"
    if (soc_level == "detailed") {
      for (area in filtered_unique_areas) {
        area_cosine_matrices[[as.character(area)]] <- area_cosine_matrices[["99"]]
      }
    }
    
    return(invisible(area_cosine_matrices))
    
  } else {
    print("Please specify 'weighted_imp' as 'yes' or 'no'.")
  }

}


#################################################

compute_cosine_similarity_level <- function(inputs, occupationMatrix, testVector) {
  
  weighted_level <- inputs$weighted_level
  soc_level <- inputs$soc_level
  
  # Ensure the testVector has an AREA column
  if (!"AREA" %in% colnames(testVector)) {
    testVector$AREA <- NA
  }
  
  if (weighted_level == "yes") {
    occupationMatrix <- bind_rows(data.frame(occupationMatrix), data.frame(testVector))
    occupations <- unique(occupationMatrix$OCC_CODE)
    
    selected_data <- occupationMatrix %>%
      dplyr::select(AREA, Title, OCC_CODE, elementID, elementName, surveyValue_LV) %>%
      distinct()
    
    grouped_data <- selected_data %>%
      group_by(AREA, OCC_CODE) %>%
      summarize(imp_vector = list(surveyValue_LV), .groups = "drop") %>%
      ungroup()
    
    unique_areas <- unique(grouped_data$AREA)
    
    area_cosine_matrices <- list()
    
    # Filter out the NA AREA value
    filtered_unique_areas <- unique_areas[!is.na(unique_areas)]
    
    # Determine the areas to process based on soc_level
    if (soc_level == "broad") {
      areas_to_process <- filtered_unique_areas
    } else if (soc_level == "detailed") {
      areas_to_process <- 99
    } else {
      stop("Invalid soc_level value. Please specify 'broad' or 'detailed'.")
    }
    
    for (area in areas_to_process) {
      area_grouped_data <- if (all(is.na(testVector$AREA))) {
        grouped_data %>%
          filter(AREA == area | OCC_CODE %in% testVector$OCC_CODE)
      } else {
        grouped_data %>%
          filter((AREA == area & !OCC_CODE %in% testVector$OCC_CODE) | (AREA == area & OCC_CODE %in% testVector$OCC_CODE))
      }
      
      area_cosine_matrix <- cosine_similarity_matrix(area_grouped_data)
      area_cosine_matrices[[as.character(area)]] <- area_cosine_matrix
    }
    
    # Add the resulting matrix to each unique_area if soc_level is "detailed"
    if (soc_level == "detailed") {
      for (area in filtered_unique_areas) {
        area_cosine_matrices[[as.character(area)]] <- area_cosine_matrices[["99"]]
      }
    }
    
    return(invisible(area_cosine_matrices))
    
  } else {
    print("Please specify 'weighted_level' as 'yes' or 'no'.")
  }
}



######################################

check_skill_thresholds <- function(area_occupationMatrix, testVector, occupation1, occupation2, importance_matrix, level_matrix, inputs) {
  
  # Convert data frames to data.tables for faster operations
  setDT(area_occupationMatrix)
  setDT(testVector)

    subset_cols <- c("elementName", "elementID", "surveyValue_LV", "standardError_LV")
    
    skills1 <- area_occupationMatrix[OCC_CODE == occupation1, ..subset_cols]
    skills2 <- testVector[OCC_CODE == occupation2, ..subset_cols]
    
    join_data <- skills1[skills2, on = .(elementName, elementID), nomatch = 0] # Efficient join

    
    logical_vector <- with(join_data, {
      result <- numeric(nrow(join_data))
      if (inputs$certainty == "mean") {
        result <- (!is.na(surveyValue_LV) & surveyValue_LV >= get("i.surveyValue_LV"))
      } else if (inputs$certainty == "bound") {
        result <- (!is.na(surveyValue_LV) & !is.na(get("i.surveyValue_LV")) & surveyValue_LV >= (get("i.surveyValue_LV") - get("i.standardError_LV")) & surveyValue_LV <= (get("i.surveyValue_LV") + get("i.standardError_LV")))
      }
      result
    })
    
    pct_met <- mean(logical_vector, na.rm = TRUE)
    weight_imp <- ifelse(inputs$weighted_imp == "yes", importance_matrix[occupation1, occupation2], 1)
    weight_level <- ifelse(inputs$weighted_level == "yes", level_matrix[occupation1, occupation2], 1)
    
    
    result <- pct_met * weight_imp * weight_level
    
    return(result)
}


############################

run_network_function <- function(inputs, occupationMatrix, testVector, testNames_list, cosine_imp, cosine_level) {
  # Prepare your environment
  plan(multisession, workers = detectCores() - 1)
  unique_areas <- unique(occupationMatrix$AREA)
  results_list <- list()
  
  occTOtest_or_testTOocc <- inputs$occTOtest_or_testTOocc
  
  # Initial columns setup, modified to include dynamic test names
  initial_columns <- c("AREA", "AREA_TITLE", "OCC_CODE", testNames_list)
  
  # Filter data for AREA == "99" and run the computations only for this area
  area_99_data <- occupationMatrix[occupationMatrix$AREA == "99", ]
  area_99_results <- data.frame(matrix(ncol = length(initial_columns), nrow = 0))  
  colnames(area_99_results) <- initial_columns 
  
  # Loop through each occupation in AREA 99
  for (occupation in unique(area_99_data$OCC_CODE)) {
    # Initialize temp_results with a single row for the current occupation
    temp_results <- data.frame(AREA = "99", AREA_TITLE = unique(area_99_data$AREA_TITLE)[1], OCC_CODE = occupation, matrix(ncol = length(testNames_list), nrow = 1))
    colnames(temp_results) <- initial_columns  
    
    for (testName in testNames_list) {
      # Compute the edge weight only for AREA 99
      edge_weight <- check_skill_thresholds(area_99_data, testVector, occupation, testName, cosine_imp[["99"]], cosine_level[["99"]], inputs)
      # Assign edge weight to column named after testName
      temp_results[[testName]] <- edge_weight  
    }
    
    # Combine all test results for this occupation
    area_99_results <- rbind(area_99_results, temp_results)
  }
  
  # Copy AREA 99 results to all other areas
  for (area in unique_areas) {
    if(area != "99") {
      temp_results <- area_99_results
      temp_results$AREA <- area
      temp_results$AREA_TITLE <- unique(occupationMatrix[occupationMatrix$AREA == area, ]$AREA_TITLE)[1]
      results_list[[area]] <- temp_results
    } else {
      results_list[["99"]] <- area_99_results
    }
  }
  
  # Combine all areas' results into a single data frame
  output_df <- do.call(rbind, results_list)
  
  # Ensure the result is a data frame
  output_df <- as.data.frame(output_df)
  
  # Reset computation to normal
  plan(sequential)
  
  # Return the final output
  return(output_df)
}



#################################################

compute_average_skill_satisfaction <- function(inputs, occupationMatrix, testVector, importance_matrix, level_matrix) {
  
  certainty <- inputs$certainty 
  weighted_imp <- inputs$weighted_imp
  weighted_level <- inputs$weighted_level
  soc_level <- inputs$soc_level
  occTOtest_or_testTOocc <- inputs$occTOtest_or_testTOocc
  
  # Basic validation
  if (!("elementName" %in% names(occupationMatrix)) || !("surveyValue_LV" %in% names(occupationMatrix))) {
    stop("occupationMatrix must contain 'elementName' and 'surveyValue_LV' columns.")
  }
  
  if (!("elementName" %in% names(testVector)) || !("surveyValue_LV" %in% names(testVector))) {
    stop("testVector must contain 'elementName' and 'surveyValue_LV' columns.")
  }
  
  if (soc_level == "detailed"){
    occupationMatrix <- occupationMatrix[occupationMatrix$AREA == "99", ]
    testVector <- testVector[testVector$AREA == "99", ]
  }
  
  # Merge the occupationMatrix with the testVector based on occTOtest_or_testTOocc
  if (occTOtest_or_testTOocc == "occTOtest") {
    merged_data <- merge(occupationMatrix, testVector, by = c("elementID", "elementName"), suffixes = c("_occ", "_test"))
  } else if (occTOtest_or_testTOocc == "testTOocc") {
    merged_data <- merge(testVector, occupationMatrix, by = c("elementID", "elementName"), suffixes = c("_test", "_occ"))
  } else {
    stop("Please specify 'occTOtest_or_testTOocc' as 'occTOtest' or 'testTOocc'.")
  }
  
  # Calculate the satisfaction level based on occTOtest_or_testTOocc
  if (certainty == "mean") {
    if (occTOtest_or_testTOocc == "occTOtest") {
      merged_data$satisfaction <- with(merged_data, ifelse(surveyValue_LV_occ >= surveyValue_LV_test, 1, 0))
    } else {
      merged_data$satisfaction <- with(merged_data, ifelse(surveyValue_LV_test >= surveyValue_LV_occ, 1, 0))
    }
  } else if (certainty == "bound") {
    if (occTOtest_or_testTOocc == "occTOtest") {
      merged_data$satisfaction <- with(merged_data, ifelse(abs(surveyValue_LV_occ - surveyValue_LV_test) <= standardError_LV_test, 1, 0))
    } else {
      merged_data$satisfaction <- with(merged_data, ifelse(abs(surveyValue_LV_test - surveyValue_LV_occ) <= standardError_LV_occ, 1, 0))
    }
  } else {
    stop("Please specify 'certainty' as 'mean' or 'bound'.")
  }
  
  # Weight the satisfaction if required
  if (weighted_imp == "yes" || weighted_level == "yes") {
    merged_data$satisfaction_weighted <- merged_data$satisfaction
    if (weighted_imp == "yes") {
      merged_data$satisfaction_weighted <- merged_data$satisfaction_weighted * apply(merged_data[, c("elementName", "OCC_CODE")], 1, function(x) importance_matrix[x["elementName"], x["OCC_CODE"]])
    }
    if (weighted_level == "yes") {
      merged_data$satisfaction_weighted <- merged_data$satisfaction_weighted * apply(merged_data[, c("elementName", "OCC_CODE")], 1, function(x) level_matrix[x["elementName"], x["OCC_CODE"]])
    }
  }
  
  # Aggregate these satisfaction levels for each skill across all occupations
  result <- if (weighted_imp == "yes" || weighted_level == "yes") {
    aggregate(satisfaction_weighted ~ elementID + elementName, data = merged_data, FUN = mean)
  } else {
    aggregate(satisfaction ~ elementID + elementName, data = merged_data, FUN = mean)
  }
  
  return(result)
}


#################################################


# Define function to get the top n rows by testOccupation

get_top_n_rows <- function(inputs, subset_matrix, testNames_list, n) {
  
  geo_level <- inputs$geo_level
  existing_or_proposed <- inputs$existing_or_proposed
  
  # Initialize an empty data frame to store the top n rows for each test occupation
  top_n_subset_matrix <- data.frame()
  
  # Create a parallel backend with available cores
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  # if (existing_or_proposed == "existing") {
  
  # Remove any duplicate OCC_CODEs from test in subset_matrix
  # Remove * from each entry in testNames_list
  tests_codes <- gsub("\\*", "", testNames_list)
  
  # Filter subset_matrix to remove any OCC_CODE value that is in the modified testNames_list
  subset_matrix <- subset_matrix[!subset_matrix$OCC_CODE %in% tests_codes, ]
  # }
  
  # Parallel computation
  top_n_subset_matrix <- foreach(test_name = testNames_list, .combine = 'rbind') %dopar% {
    
    # Sort the subset_matrix by the current test occupation in descending order
    sorted_subset_matrix <- subset_matrix[order(subset_matrix[[test_name]], decreasing = TRUE), ]
    
    # if (geo_level != "national") {
    unique_areas <- unique(sorted_subset_matrix$AREA_TITLE)
    area_top_n_rows <- data.frame()
    
    for (area in unique_areas) {
      area_sorted_subset_matrix <- sorted_subset_matrix[sorted_subset_matrix$AREA_TITLE == area, ]
      area_top_n <- head(area_sorted_subset_matrix, n)
      area_top_n_rows <- rbind(area_top_n_rows, area_top_n)
    }
    
    top_n_rows <- area_top_n_rows
    
    # Add a column to top_n_rows to store the test occupation OCC_CODE
    top_n_rows$TEST_OCC_CODE <- test_name
    
    # Select only the columns relevant to the current test occupation
    top_n_rows <- top_n_rows[, c("AREA_TITLE", "OCC_CODE", "TEST_OCC_CODE", test_name)]
    
    # Rename the edge_weight column to a generic name
    colnames(top_n_rows)[colnames(top_n_rows) == test_name] <- "edge_weight"
    
    return(top_n_rows)
  }
  
  # Stop the cluster
  stopCluster(cl)
  
  return(top_n_subset_matrix)
}

#################################################

# Update generate_top_n_list to generate a table instead of a list

generate_top_n_table <- function(top_n_subset_matrix, testOccupation, n, geo_level, occ_codes_and_titles, soc_level) {
  
  # Filter and manipulate data
  table_data <- top_n_subset_matrix %>%
    filter(TEST_OCC_CODE == testOccupation) %>%
    select(Title, OCC_CODE, edge_weight, TOT_EMP, MEAN, PCT10, PCT25, PCT50, PCT75, PCT90) %>%
    mutate(edge_weight = round(edge_weight, 3)) %>%
    slice_head(n = n)
  
  colnames(table_data) <- c("Job Title", "SOC Code", "Skill Similarity", "Employment", "Mean Wage", "10th Pctl. Wage", "25th Pctl. Wage", "50th Pctl. Wage", "75th Pctl. Wage", "90th Pctl. Wage")
  table_data$`Employment` <- format(as.integer(table_data$`Employment`), big.mark = ",")
  
  return(table_data)
}


#################################################

graph_cdf_interactive <- function(inputs, subset_matrix, testNames_list, titles) {

  geo_level <- inputs$geo_level
  desired_skill_transferability <- inputs$desired_skill_transferability
  soc_level <- inputs$soc_level

  if (soc_level=='detailed') {

    # Compute the CDFs for each test occupation
    cdf_data <- lapply(testNames_list, function(name) {
      data <- subset_matrix[, c("AREA_TITLE", name)]
      data <- data[complete.cases(data), ]
      cdf <- ecdf(data[[name]])
      data$ecdf <- 1- cdf(data[[name]])
      names(data)[2] <- "edge_weight"
      data$test_name <- name
      return(data)
    })


    cdf_data <- do.call(rbind, cdf_data)

    cdf_data <- cdf_data %>%
      filter(ecdf != 0)

    cat("cdf_data:\n")
    print(cdf_data)

    cat("titles:\n")
    print(titles)

    titles_names <- titles %>%
      rename(test_name = Test)

    cdf_data <- left_join(cdf_data, titles_names, by = "test_name")

    # Create the plot for the national level
    plot <- ggplot(cdf_data, aes(x = edge_weight, y = ecdf, color = Title)) +
      geom_line(linewidth = 1.25, alpha = 0.75) +
      scale_color_brewer(name = "Test Occupations", palette = "Dark2", labels = scales::label_wrap(26)) +
      geom_vline(xintercept = desired_skill_transferability, linetype = "dashed", color = "#063970") +
      labs(x = "Skill Similarity",
           y = "Cumulative Proportion") +
      theme(
        rect = element_rect(fill = "transparent"),
        text = element_text(size = 17), # Increase font size for axis and legend text
        axis.title = element_text(size = 15), # Increase font size for axis titles
        axis.text = element_text(size = 14), # Increase font size for axis tick labels
        legend.title = element_text(size = 17, face = "bold"), # Increase font size for legend title
        legend.text = element_text(size = 16)
      ) 

    plot <- plot + coord_cartesian(xlim = c(0,1))

    return(plot)

  } else if (geo_level == "state") {
    # Compute the CDFs for each test occupation and state
    cdf_data <- lapply(testNames_list, function(name) {
      data <- subset_matrix[, c("AREA_TITLE", name)]
      data <- data[complete.cases(data), ]
      cdf <- ecdf(data[[name]])
      data$ecdf <- cdf(data[[name]])
      names(data)[2] <- "edge_weight"
      data$test_name <- name
      return(data)
    })
    cdf_data <- do.call(rbind, cdf_data)

    cdf_data <- cdf_data %>%
      filter(ecdf != 0)

    # Create the geofacet plot
    plot <- ggplot(cdf_data, aes(x = edge_weight, y = ecdf, color = Title)) +
      geom_line(linewidth = 1.25, alpha = 0.75) +
      scale_color_manual(name = "Test Occupations", values=c('#006400', '#7843a6','#063970','#6b0e0e', '#A69943'), labels = scales::label_wrap(26)) +
      geom_vline(xintercept = desired_skill_transferability, linetype = "dashed", color = "#063970") +
      labs(x = "Skill Similarity",
           y = "Cumulative Proportion") +
      facet_geo(~AREA_TITLE, grid = "us_state_with_DC_PR_grid1", label = "name") +
      theme(
        rect = element_rect(fill = "transparent"),
        text = element_text(size = 17), # Increase font size for axis and legend text
        axis.title = element_text(size = 15), # Increase font size for axis titles
        axis.text = element_text(size = 14), # Increase font size for axis tick labels
        legend.title = element_text(size = 17, face = "bold"), # Increase font size for legend title
        legend.text = element_text(size = 16)
                    )

    plot <- plot + coord_cartesian(xlim = c(0,1))

    return(plot)

  } else {
    stop("Please specify 'geo_level' as 'national' or 'state'.")
  }
}

#################################################

graph_cdf_interactive_unweighted <- function(inputs, subset_matrix, testNames_list, titles) {
  
  geo_level <- inputs$geo_level
  desired_skill_transferability <- inputs$desired_skill_transferability
  soc_level <- inputs$soc_level
  
  if (soc_level == 'detailed') {
    
    # Compute the unweighted CDFs for each test occupation
    cdf_data <- lapply(testNames_list, function(name) {
      data <- subset_matrix[, c("AREA_TITLE", name)]
      data <- data[complete.cases(data), ]
      cdf <- ecdf(data[[name]])
      data$ecdf <- 1 - cdf(data[[name]])
      names(data)[2] <- "edge_weight"
      data$test_name <- name
      return(data)
    })
    
    cdf_data <- do.call(rbind, cdf_data)
    cdf_data <- cdf_data %>%
      filter(ecdf >= 0 & ecdf <= 1)
    
    titles_names <- titles %>%
      rename(test_name = Test)
    
    cdf_data <- left_join(cdf_data, titles_names, by = "test_name")
    
    # Create the plot for the national level with matching formatting
    plot <- ggplot(cdf_data, aes(x = edge_weight, y = ecdf, color = Title)) +
      geom_line(linewidth = 1.25, alpha = 0.75) +
      scale_color_brewer(name = "Test Occupations", palette = "Dark2", labels = scales::label_wrap(26)) +
      geom_vline(xintercept = desired_skill_transferability, linetype = "dashed", color = "#063970") +
      labs(x = "Skill Similarity", y = "Cumulative Proportion") +
      theme(
        rect = element_rect(fill = "transparent"),
        text = element_text(size = 17), # Increase font size for axis and legend text
        axis.title = element_text(size = 15), # Increase font size for axis titles
        axis.text = element_text(size = 14), # Increase font size for axis tick labels
        legend.title = element_text(size = 17, face = "bold"), # Increase font size for legend title
        legend.text = element_text(size = 16)
        )
    
    plot <- plot + coord_cartesian(xlim = c(0,1))
    
    return(plot)
    
  } else {
    stop("Currently, the function only supports 'detailed' soc_level.")
  }
}

#################################################

graph_cdf_interactive_empweighted <- function(inputs, subset_matrix, testNames_list, titles) {
  
  # Extracting input parameters
  geo_level <- inputs$geo_level
  desired_skill_transferability <- inputs$desired_skill_transferability
  soc_level <- inputs$soc_level
  
  # Check if SOC level is detailed
  if (soc_level == 'detailed') {
    
    # Initialize a list to store graphs
    graph_list <- list()
    
    # Get unique AREA_TITLE values
    area_titles <- unique(subset_matrix$AREA_TITLE)
    cat("area_titles:")
    print(area_titles)
    
    # Loop through each AREA_TITLE
    for (area_title in area_titles) {
      # Filter data for the current AREA_TITLE
      filtered_data <- subset_matrix[subset_matrix$AREA_TITLE == area_title, ]
      
      # Compute the weighted CDFs for each test occupation
      cdf_data <- lapply(testNames_list, function(name) {
        data <- filtered_data[, c("AREA_TITLE", name, "TOT_EMP")]
        data <- data[complete.cases(data), ]
        data <- data[order(data[[name]]), ]
        total_weight <- sum(data$TOT_EMP)
        data$ecdf <- cumsum(data$TOT_EMP) / total_weight
        data$ecdf <- 1 - data$ecdf
        names(data)[2] <- "edge_weight"
        data$test_name <- name
        return(data)
      })
      
      cdf_data <- do.call(rbind, cdf_data)
      cdf_data <- cdf_data %>%
        filter(ecdf >= 0 & ecdf <= 1)

            titles_names <- titles %>% rename(test_name = Test)
      cdf_data <- left_join(cdf_data, titles_names, by = "test_name")
      
      # Create the plot for the current AREA_TITLE
      plot <- ggplot(cdf_data, aes(x = edge_weight, y = ecdf, color = Title)) + 
        geom_line(linewidth = 1.25, alpha = 0.75) + 
        scale_color_brewer(name = "Test Occupations", palette = "Dark2", labels = scales::label_wrap(26)) +
        geom_vline(xintercept = desired_skill_transferability, linetype = "dashed", color = "#063970") +
        labs(x = "Skill Similarity", y = "Cumulative Proportion") +
        theme(
          rect = element_rect(fill = "transparent"),
          text = element_text(size = 17), # Increase font size for axis and legend text
          axis.title = element_text(size = 15), # Increase font size for axis titles
          axis.text = element_text(size = 14), # Increase font size for axis tick labels
          legend.title = element_text(size = 17, face = "bold"), # Increase font size for legend title
          legend.text = element_text(size = 16)
        )
      
      plot <- plot + coord_cartesian(xlim = c(0,1))
      
      # Debug: Print out a message to confirm the plot is about to be stored
      print(paste("Storing plot for AREA_TITLE:", area_title))
      
      # Add the plot to the list
      graph_list[[area_title]] <- plot
    }
    
    # Return the list of graphs
    return(graph_list)
  } else {
    stop("Currently, the function only supports 'detailed' soc_level.")
  }
}


#################################################


filter_skill_transferability <- function(inputs, subset_matrix, testVector) {
  
  desired_skill_transferability <- inputs$desired_skill_transferability
  
  testOccupations <- unique(testVector$OCC_CODE)

  # Convert the subset_matrix to a data frame
  subset_df <- as.data.frame(subset_matrix)

  # Replace values less than desired_skill_transferability with NA
  subset_df[testOccupations] <- lapply(subset_df[testOccupations], function(x) {
    x[x < desired_skill_transferability] <- NA
    return(x)
  })

  # Filter rows with NA values in all testOccupations columns
  filtered_df <- subset_df %>%
    filter(!apply(is.na(dplyr::select(., all_of(testOccupations))), 1, all))

  # Reorder columns before returning the filtered data frame
  filtered_df <- dplyr::select(filtered_df, AREA, AREA_TITLE, everything())

  return(filtered_df)
}


##########################
##########################

# SUPPLY CURVE FUNCTIONS

##########################

# Define function to compute highest wage percentile met for each occupation for given wage and wage frequency
highest_percentile <- function(df, wage, hourly_or_annual) {
  percentiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  if (hourly_or_annual == "hourly") {
    wage_prefix <- "H"
  } else if (hourly_or_annual == "annual") {
    wage_prefix <- "A"
  } else {
    stop("Please specify 'hourly_or_annual' as 'hourly' or 'annual'.")
  }

  unique_areas <- unique(df$AREA)

  # Create a list to store the results
  results_list <- list()

  for (area in unique_areas) {
    area_df <- df %>%
      filter(AREA == area)

    # Calculate the highest percentile met for each row in area_df using sapply
    area_results <- sapply(seq_len(nrow(area_df)), function(i) {
      row <- area_df[i, ]
      col_indices <- grep(paste0("^", wage_prefix, "_PCT"), colnames(row))
      percentiles_vals <- as.numeric(row[col_indices])

      if (all(is.na(percentiles_vals))) {
        return(NA)
      } else {
        percentiles_vals_no_na <- percentiles_vals
        percentiles_vals_no_na[is.na(percentiles_vals_no_na)] <- ifelse(hourly_or_annual == "hourly", -Inf, -Inf)

        if (wage >= max(percentiles_vals_no_na)) {
          return(1)
        } else if (wage < min(percentiles_vals_no_na)) {
          return(0)
        } else {
          satisfied_percentiles <- percentiles[which(percentiles_vals_no_na <= wage)]
          return(max(satisfied_percentiles))
        }
      }
    })

    area_occtitles <- area_df$OCC_CODE
    area_results_df <- data.frame(AREA = rep(area, nrow(area_df)), OCC_CODE = area_occtitles, percentile_met = area_results)

    # Append the data frame to the results_list
    results_list[[area]] <- area_results_df
  }

  # Combine the list of data frames into a single data frame
  results <- do.call(rbind, results_list)

  return(invisible(results))
}

#################################################

# Define function to calculate total labor supply for given wage and skill constraints
labor_supply <- function(df, highest_percentile, wage, testNames_list) {
  # Join the data frames outside the loop
  df <- left_join(df, highest_percentile, by = c("AREA", "OCC_CODE"))
  unique_areas <- unique(df$AREA)

  # Create a list to store the results
  result_list <- list()

  for (area in unique_areas) {
    # Subset the data frame to only include rows where the "AREA" column matches the current unique value
    area_df <- df %>%
      filter(AREA == area)

    # Calculate the total supply for each test name using sapply
    supply_list <- sapply(testNames_list, function(name) {
      emp <- area_df[, "TOT_EMP"]
      occ_weight <- area_df[, name]
      perc_wage <- area_df[, "percentile_met"]

      # Calculate supply for current column based on the occ_weight condition
      supply <- ifelse(is.na(occ_weight), 0, emp * perc_wage)

      total_supply <- sum(supply, na.rm = TRUE) # Calculate total supply for current column
      return(total_supply)
    })

    # Append the data frame to the result_list
    result_list[[area]] <- data.frame(AREA = area, TotalSupply = supply_list)
  }

  # Combine the list of data frames into a single data frame
  result_df <- do.call(rbind, result_list)

  return(result_df)
}


#################################################

# Define function to calculate labor supply and highest percentile met for given wage and skill constraints
labor_supply_and_percentile <- function(df, wage, testNames_list, hourly_or_annual) {
  perc_wage <- highest_percentile(df, wage, hourly_or_annual)
  supply_list <- labor_supply(df, perc_wage, wage, testNames_list)
  return(supply_list)
}

#################################################

# Define a function to iterate labor_supply over a range of wage values
iterate_labor_supply <- function(inputs, df, testNames_list) {
  
  wage_lower <- inputs$wage_lower
  wage_upper <- inputs$wage_upper
  wage_interval <- inputs$wage_interval
  hourly_or_annual <- inputs$hourly_or_annual
  wage_tested <- inputs$wage_tested
  geo_level <- inputs$geo_level
  
  # Create a vector of wage values to iterate over
  wage_vec <- seq(from = wage_lower, to = wage_upper, by = wage_interval)
  
  # Get the unique AREA_TITLE values
  area_title <- unique(df[, c("AREA", "AREA_TITLE")])
  
  # Create a parallel backend with available cores
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  # Replace the for loop with foreach
  supply_df <- foreach(wage = wage_vec, 
                       .combine = rbind, 
                       .export = c("labor_supply_and_percentile", "highest_percentile", "labor_supply"),
                       .packages = "dplyr") %dopar% {
                         wage <- round(wage, 2)
                         
                         # Iterate over tests using lapply
                         test_results <- lapply(testNames_list, function(test) {
                           supply_list_test <- labor_supply_and_percentile(df, wage, c(test), hourly_or_annual)
                           
                           # Iterate over area titles using lapply and return a list of data frames
                           area_results <- lapply(seq_len(nrow(area_title)), function(i) {
                             area <- area_title$AREA[i]
                             area_title_value <- area_title$AREA_TITLE[i]
                             
                             # Select the supply value for the current AREA
                             total_supply <- supply_list_test[supply_list_test$AREA == area, "TotalSupply"]
                             
                             # Check if the total_supply has a value and return a data frame
                             if (length(total_supply) > 0) {
                               data.frame(AREA = area, AREA_TITLE = area_title_value, wage = wage, Test = test, TotalSupply = total_supply)
                             } else {
                               NULL
                             }
                           })
                           
                           # Remove NULL elements and return a single data frame
                           do.call(rbind, area_results)
                         })
                         
                         # Combine the test results
                         do.call(rbind, test_results)
                       }
  
  # Stop the cluster
  stopCluster(cl)
  
  # Return the data frame of results
  return(supply_df)
}

#################################################

# Define function to calculate total labor supply for given wage and skill constraints
labor_supply_existing <- function(df, highest_percentile, wage) {
  # Join the data frames outside the loop
  df <- left_join(df, highest_percentile, by = c("AREA", "OCC_CODE"))
  unique_areas <- unique(df$AREA)
  unique_occ_codes <- unique(df$OCC_CODE) # create a list of unique occupation codes

  # Create a list to store the results
  result_list <- list()

  for (area in unique_areas) {
    # Subset the data frame to only include rows where the "AREA" column matches the current unique value
    area_df <- df %>%
      filter(AREA == area)

    # Calculate the total supply for each OCC_CODE using sapply
    supply_list <- sapply(unique_occ_codes, function(code) {
      emp <- area_df[area_df$OCC_CODE == code, "TOT_EMP"][[1]]
      perc_wage <- area_df[area_df$OCC_CODE == code, "percentile_met"][[1]]

      # Calculate supply for current column based on the occ_weight condition
      supply <- ifelse(is.na(perc_wage), 0, emp * perc_wage)

      total_supply <- sum(supply, na.rm = TRUE) # Calculate total supply for current column
      return(total_supply)
    })

    # Append the data frame to the result_list
    result_list[[area]] <- data.frame(AREA = area, OCC_CODE = unique_occ_codes, TotalSupply = supply_list)
  }

  # Combine the list of data frames into a single data frame
  result_df <- do.call(rbind, result_list)

  return(result_df)
}


#################################################

# Define function to calculate labor supply and highest percentile met for given wage and skill constraints
labor_supply_and_percentile_existing <- function(df, wage, hourly_or_annual) {
  perc_wage <- highest_percentile(df, wage, hourly_or_annual)
  supply_list <- labor_supply_existing(df, perc_wage, wage)
  return(supply_list)
}

#################################################

# Define a function to iterate labor_supply over a range of wage values
iterate_labor_supply_existing <- function(inputs, df) {
  
  wage_lower <- inputs$wage_lower
  wage_upper <- inputs$wage_upper
  wage_interval <- inputs$wage_interval
  hourly_or_annual <- inputs$hourly_or_annual
  geo_level <- inputs$geo_level
  
  # Create a vector of wage values to iterate over
  wage_vec <- seq(from = wage_lower, to = wage_upper, by = wage_interval)

  # Get the unique AREA_TITLE values
  area_title <- unique(df[, c("AREA", "AREA_TITLE")])

  # Get the unique OCC_CODE values
  unique_occ_codes <- unique(df$OCC_CODE)

  # Create a list to store the results
  supply_list <- list()

  # Iterate over each wage value and compute the labor supply and percentile
  for (wage in wage_vec) {
    wage <- round(wage, 2)

    # Iterate over occupations using lapply
    occ_results <- lapply(unique_occ_codes, function(code) {
      supply_list_occ <- labor_supply_and_percentile_existing(df, wage, hourly_or_annual)

      # Iterate over area titles using lapply and return a list of data frames
      area_results <- lapply(seq_len(nrow(area_title)), function(i) {
        area <- area_title$AREA[i]
        area_title_value <- area_title$AREA_TITLE[i]

        # Select the supply value for the current AREA
        total_supply <- supply_list_occ[supply_list_occ$AREA == area & supply_list_occ$OCC_CODE == code, "TotalSupply"]

        # Check if the total_supply has a value and return a data frame
        if (length(total_supply) > 0) {
          data.frame(AREA = area, AREA_TITLE = area_title_value, wage = wage, OCC_CODE = code, TotalSupply = total_supply)
        } else {
          NULL
        }
      })

      # Remove NULL elements and return a single data frame
      do.call(rbind, area_results)
    })

    # Append the occupation results to the supply_list
    supply_list <- append(supply_list, occ_results)
  }

  # Combine the list of data frames into a single data frame
  supply_df <- do.call(rbind, supply_list)

  # Return the data frame of results
  return(supply_df)
}

#################################################

graph_supply_curve_by_area_title <- function(inputs, supply_df) {
  
  # Extract inputs
  wage_lower <- inputs$wage_lower
  wage_upper <- inputs$wage_upper
  wage_tested <- inputs$wage_tested
  
  # Create lists to store the graphs and intersect values
  graph_list <- list()
  intersect_values_list <- list()
  
  # Get the unique AREA_TITLE values
  area_titles <- unique(supply_df$AREA_TITLE)
  
  # Loop through each AREA_TITLE and generate the graph
  for (area_title in area_titles) {
    # Filter the data for the current AREA_TITLE
    df_area <- supply_df %>%
      filter(AREA_TITLE == area_title)
    
    # Check if the filtered data frame is empty
    if (nrow(df_area) == 0 || all(df_area$TotalSupply == 0)) {
      next
    }
    
    # Calculate and store intersection values (TotalSupply) for each Title at wage_tested
    intersect_values <- df_area %>%
      group_by(Title) %>%
      summarize(intersect_value = approx(x = wage, y = TotalSupply, xout = wage_tested)$y, .groups = "drop") %>%
      distinct()
    
    # Store intersect values
    intersect_values_list[[area_title]] <- intersect_values

    median_intersect_value <- median(intersect_values$intersect_value)
    x_range <- max(df_area$TotalSupply) - min(df_area$TotalSupply)
    
    # Set the label for the wage line
    label <- paste0("Wage Tested: $", formatC(wage_tested, format = "f", big.mark = ",", digits = 2))
    
    # Create the plot common for all areas
    p <- ggplot(df_area, aes(x = TotalSupply, y = wage, color = Title)) +
      geom_line(linewidth = 1.25, alpha = 0.75) +
      scale_color_brewer(name = "Test Occupation", palette = "Dark2", labels = scales::label_wrap(26)) +
      geom_hline(yintercept = wage_tested, linetype = "dashed", color = "#4D4D4D", alpha = 0.75, linewidth = 0.5) +
      scale_linetype_manual(name = label, values = "dashed", labels = "") +
      scale_y_continuous(limits = c(wage_lower, wage_upper), labels = scales::dollar) +
      scale_x_continuous(labels = scales::comma) +
      labs(y = "Wage", x = "Labor Supply (Stock)") +
      theme(
        text = element_text(size = 17),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 17, face = "bold"),
        legend.text = element_text(size = 16),
        rect = element_rect(fill = "")
      )
    
    # Add area-specific title adjustments
    p <- if (area_title == "U.S.") {
      p + labs(x = "National Labor Supply (Stock)")
    } else {
      p + labs(x = "Regional Labor Supply (Stock)")
    }
    
    # Store the graph in the list using the AREA_TITLE as the key
    graph_list[[area_title]] <- p
  }
  return(list(graphs = graph_list, intersects = intersect_values_list)
)
}

#################################################

graph_supply_curve_by_area_title_existing <- function(inputs, supply_df) {
  
  # Extract inputs from the function parameters
  wage_lower <- inputs$wage_lower
  wage_upper <- inputs$wage_upper
  wage_tested <- inputs$wage_tested
  
  # Create a list to store the graphs
  graph_list <- list()
  
  # Get the unique AREA_TITLE values
  area_titles <- unique(supply_df$AREA_TITLE)
  
  # Loop through each AREA_TITLE and generate the graph
  for (area_title in area_titles) {
    # Filter the data for the current AREA_TITLE
    df_area <- supply_df %>%
      filter(AREA_TITLE == area_title)
    
    # Calculate the intersection of the supply curves using linear interpolation
    intersect_values <- df_area %>%
      group_by(Title) %>%
      summarize(intersect_value = approx(x = wage, y = TotalSupply, xout = wage_tested, rule = 2)$y, .groups = "drop") %>%
      distinct()
    
    median_intersect_value <- median(intersect_values$intersect_value)
    x_range <- max(df_area$TotalSupply) - min(df_area$TotalSupply)
    
    label <- paste0("Wage Tested: $", formatC(wage_tested, format = "f", big.mark = ",", digits = 2))
    
    # Create the plot for the current AREA_TITLE
    p <- ggplot(df_area, aes(x = TotalSupply, y = wage, color = Title)) +
      geom_line(linewidth = 1.25, alpha = 0.75) +
      scale_color_brewer(name = "Existing Occupation", palette = "Dark2", labels = scales::label_wrap(26)) +
      geom_hline(yintercept = wage_tested, linetype = "dashed", color = "#4D4D4D", alpha = 0.75, linewidth = 0.5) +
      scale_y_continuous(limits = c(wage_lower, wage_upper), labels = scales::dollar) +
      scale_x_continuous(labels = scales::comma) +
      labs(y = "Wage", x = ifelse(area_title == "U.S.", "National Labor Supply (Stock)", "Regional Labor Supply (Stock)")) +
      theme(
        rect = element_rect(fill = "transparent"),
        text = element_text(size = 17),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 17, face = "bold"),
        legend.text = element_text(size = 16)
      )
    
    # Store the graph in the list using the AREA_TITLE as the key
    graph_list[[area_title]] <- p
  }
  return(graph_list)
}


################################################################################
################################################################################
################################################################################
################################################################################

# OCCUPATIONAL MOBILITY

################################################################################

##########################

# SUPPLY CURVE FUNCTIONS

##########################

# Define function to compute highest wage percentile met for each occupation for given wage and wage frequency
highest_percentile_mobility <- function(df, wage, hourly_or_annual) {
  percentiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  if (hourly_or_annual == "hourly") {
    wage_prefix <- "H"
  } else if (hourly_or_annual == "annual") {
    wage_prefix <- "A"
  } else {
    stop("Please specify 'hourly_or_annual' as 'hourly' or 'annual'.")
  }

  unique_areas <- unique(df$AREA)

  # Create a list to store the results
  results_list <- list()

  for (area in unique_areas) {
    area_df <- df %>%
      filter(AREA == area)

    # Calculate the highest percentile met for each row in area_df using sapply
    area_results <- sapply(seq_len(nrow(area_df)), function(i) {
      row <- area_df[i, ]
      col_indices <- grep(paste0("^", wage_prefix, "_PCT"), colnames(row))
      percentiles_vals <- as.numeric(row[col_indices])

      if (all(is.na(percentiles_vals))) {
        return(NA)
      } else {
        percentiles_vals_no_na <- percentiles_vals
        percentiles_vals_no_na[is.na(percentiles_vals_no_na)] <- ifelse(hourly_or_annual == "hourly", -Inf, -Inf)

        if (wage >= max(percentiles_vals_no_na)) {
          return(1)
        } else if (wage < min(percentiles_vals_no_na)) {
          return(0)
        } else {
          satisfied_percentiles <- percentiles[which(percentiles_vals_no_na <= wage)]
          return(max(satisfied_percentiles))
        }
      }
    })

    area_occtitles <- area_df$OCC_CODE
    area_results_df <- data.frame(AREA = rep(area, nrow(area_df)), OCC_CODE = area_occtitles, percentile_met = area_results)

    # Append the data frame to the results_list
    results_list[[area]] <- area_results_df
  }

  # Combine the list of data frames into a single data frame
  if (length(results_list) == 0 || all(sapply(results_list, is.null))) {
    results <- data.frame(AREA = character(), OCC_CODE = character(), percentile_met = numeric())  # Adjust to match expected structure
  } else {
    results <- do.call(rbind, results_list)
  }
  
  return(invisible(results))
}

#################################################

# Define function to calculate total labor supply for given wage and skill constraints
labor_supply_mobility <- function(df, highest_percentile, wage, testNames_list) {
  
  # Ensure only existing columns are used
  existing_columns <- colnames(df)
  filtered_testNames_list <- testNames_list[sapply(testNames_list, function(test) {
    paste0(test, "_skill") %in% existing_columns && paste0(test, "_mobility") %in% existing_columns
  })]
  
  # Continue with function logic using filtered_testNames_list
  df <- left_join(df, highest_percentile, by = c("AREA", "OCC_CODE"))
  unique_areas <- unique(df$AREA)

  # Create a list to store the results
  result_list <- list()

  for (area in unique_areas) {
    # Subset the data frame to only include rows where the "AREA" column matches the current unique value
    area_df <- df %>%
      filter(AREA == area)

    # Calculate the total supply for each test name using sapply
    supply_list <- sapply(testNames_list, function(name) {
      emp <- area_df[, "TOT_EMP"]
      occ_weight <- area_df[, paste0(name,"_skill")]
      mob_rate <- area_df[,  paste0(name,"_mobility")]
      perc_wage <- area_df[, "percentile_met"]

      # Calculate supply for current column based on the occ_weight condition
      supply <- ifelse(is.na(occ_weight), 0, emp * perc_wage * mob_rate)

      total_supply <- sum(supply, na.rm = TRUE) # Calculate total supply for current column
      return(total_supply)
    })

    # Append the data frame to the result_list
    result_list[[area]] <- data.frame(AREA = area, TotalSupply = supply_list)
  }

  # Combine the list of data frames into a single data frame
  result_df <- do.call(rbind, result_list)

  return(result_df)
}


#################################################

# Define function to calculate labor supply and highest percentile met for given wage and skill constraints
labor_supply_and_percentile_mobility <- function(df, wage, testNames_list, hourly_or_annual) {
  perc_wage <- highest_percentile_mobility(df, wage, hourly_or_annual)
  supply_list <- labor_supply_mobility(df, perc_wage, wage, testNames_list)
  return(supply_list)
}

#################################################

# Define a function to iterate labor_supply over a range of wage values
iterate_labor_supply_mobility <- function(inputs, df, testNames_list) {
  
  # Adjustments to ensure both _skill and _mobility columns exist
  valid_tests <- sapply(testNames_list, function(test) {
    skill_col_exists <- paste0(test, "_skill") %in% colnames(df)
    mobility_col_exists <- paste0(test, "_mobility") %in% colnames(df)
    skill_col_exists && mobility_col_exists
  })
  
  # Filter testNames_list based on valid_tests
  testNames_list <- testNames_list[valid_tests]
  
  wage_lower <- inputs$wage_lower
  wage_upper <- inputs$wage_upper
  wage_interval <- inputs$wage_interval
  hourly_or_annual <- inputs$hourly_or_annual
  wage_tested <- inputs$wage_tested
  geo_level <- inputs$geo_level
  
  # Create a vector of wage values to iterate over
  wage_vec <- seq(from = wage_lower, to = wage_upper, by = wage_interval)

  # Get the unique AREA_TITLE values
  area_title <- unique(df[, c("AREA", "AREA_TITLE")])

  # Create a list to store the results
  supply_list <- list()

  # Iterate over each wage value and compute the labor supply and percentile
  for (wage in wage_vec) {
    wage <- round(wage, 2)

    # Iterate over tests using lapply
    test_results <- lapply(testNames_list, function(test) {
      supply_list_test <- labor_supply_and_percentile_mobility(df, wage, c(test), hourly_or_annual)

      # Iterate over area titles using lapply and return a list of data frames
      area_results <- lapply(seq_len(nrow(area_title)), function(i) {
        area <- area_title$AREA[i]
        area_title_value <- area_title$AREA_TITLE[i]

        # Select the supply value for the current AREA
        total_supply <- supply_list_test[supply_list_test$AREA == area, "TotalSupply"]

        # Check if the total_supply has a value and return a data frame
        if (length(total_supply) > 0) {
          data.frame(AREA = area, AREA_TITLE = area_title_value, wage = wage, Test = test, TotalSupply = total_supply)
        } else {
          NULL
        }
      })

      # Remove NULL elements and return a single data frame
      do.call(rbind, area_results)
    })

    # Append the test results to the supply_list
    supply_list <- append(supply_list, test_results)
  }

  # Combine the list of data frames into a single data frame
  supply_df <- do.call(rbind, supply_list)

  # Return the data frame of results
  return(supply_df)
}


#################################################

graph_supply_curve_by_area_title_mobility <- function(supply_df, wage_lower, wage_upper, wage_tested) {
  # Create a list to store the graphs
  graph_list <- list()

  # Get the unique AREA_TITLE values
  area_titles <- unique(supply_df$AREA_TITLE)

  x_max <- max(supply_df$TotalSupply, na.rm = TRUE)

  # Loop through each AREA_TITLE and generate the graph
  for (area_title in area_titles) {
    # Filter the data for the current AREA_TITLE
    df_area <- supply_df %>%
      filter(AREA_TITLE == area_title) %>%
      group_by(Title) %>%
      filter(!all(TotalSupply == 0)) %>%
      ungroup()

    # If after filtering, the df_area is empty, skip to the next iteration
    if (nrow(df_area) == 0) {
      next
    }

    # Calculate the intersection of the supply curves for each test and AREA_TITLE
    intersect_values <- df_area %>%
      filter(wage == wage_tested) %>%
      group_by(Test) %>%
      summarize(intersect_value = dplyr::first(TotalSupply), .groups = "drop") %>%
      distinct()

    median_intersect_value <- median(intersect_values$intersect_value)
    x_range <- max(df_area$TotalSupply) - min(df_area$TotalSupply)

    # Create the plot
    p <- ggplot(df_area, aes(x = TotalSupply, y = wage, color = Test)) +
      geom_line(linewidth = 1.1, alpha = 0.75) +
      ggplot2::scale_color_brewer(name = "Existing Occupation", palette = "Dark2", labels = scales::label_wrap(26)) +
      scale_linetype_manual(name = label, values = "dashed", labels = "") +
      scale_y_continuous(limits = c(wage_lower, wage_upper), labels = scales::dollar) +
      scale_x_continuous(labels = scales::comma, limits = c(0, x_max)) + # Set the x-axis limits
      labs(y = "Wage", x = "Regional Labor Supply (Flow)") +
      theme(
        text = element_text(size = 17), # Increase font size for axis and legend text
        axis.title = element_text(size = 15), # Increase font size for axis titles
        axis.text = element_text(size = 14), # Increase font size for axis tick labels
        legend.title = element_text(size = 17, face = "bold"), # Increase font size for legend title
        legend.text = element_text(size = 16)
      ) # Increase font size for legend text

    # Store the graph in the list using the AREA_TITLE as the key
    graph_list[[area_title]] <- p
  }
  return(graph_list)
}


#################################################


graph_supply_curve_by_area_mobilityANDskill_tool <- function(inputs, supply_df) {
  # Extract inputs from the function parameters
  wage_lower <- inputs$wage_lower
  wage_upper <- inputs$wage_upper
  wage_tested <- inputs$wage_tested
  
  # Create lists to store the graphs and intersect values
  graph_list <- list()
  intersect_values_list <- list()  # New list for storing intersect values
  
  # Get the unique AREA_TITLE values from the supply dataframe
  unique_area_titles <- unique(supply_df$AREA_TITLE)
  
  # Loop through each unique AREA_TITLE to create individual graphs
  for (area_title in unique_area_titles) {
    # Filter the data for the current AREA_TITLE
    df_area <- supply_df %>%
      filter(AREA_TITLE == area_title)
    
    # Skip plotting if all TotalSupply values for the current area are zero
    if (all(df_area$TotalSupply == 0)) {
      next
    }
    
    # Calculate the intersection of the supply curves for each Title and source within the current AREA_TITLE using linear interpolation
    intersect_values <- df_area %>%
      group_by(Title, source) %>%
      summarize(intersect_value = approx(x = wage, y = TotalSupply, xout = wage_tested, rule = 2)$y, .groups = 'drop') %>%
      distinct()
    
    # Store the intersection values for the current AREA_TITLE
    intersect_values_list[[area_title]] <- intersect_values
    
    # Prepare the plot (plotting code remains unchanged)
    p <- ggplot(df_area, aes(x = TotalSupply, y = wage, color = Title)) +
      geom_line(aes(linetype = source), linewidth = 1.1, alpha = 0.75) +
      scale_color_brewer(name = "Test Occupation", palette = "Dark2", labels = scales::label_wrap(26)) +
      scale_linetype_manual(name = "Estimation Method", values = c("Skill Similarity" = "dashed", "Occupational Mobility" = "solid")) +
      geom_hline(yintercept = wage_tested, linetype = "dashed", color = "#4D4D4D", alpha = 0.75, linewidth = 0.5) +
      scale_y_continuous(limits = c(wage_lower, wage_upper), labels = scales::dollar) +
      scale_x_continuous(labels = scales::comma) +
      labs(y = "Wage", x = ifelse(area_title == "U.S.", "National Labor Stock & Flow", "Regional Labor Stock & Flow")) +
      theme(
        rect = element_rect(fill = "transparent"),
        text = element_text(size = 17),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 14)
      )
    
    # Store the generated graph in the graph_list using AREA_TITLE as the key
    graph_list[[area_title]] <- p
  }
  
  # Return both the graphs and the intersection values
  return(list(graphs = graph_list, intersects = intersect_values_list))
}


#################################################
