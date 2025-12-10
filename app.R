library(shiny)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(jsonlite)
library(glue)
library(stringr) # Added for text wrapping

# --- UI ---
ui <- fluidPage(
  titlePanel("Advanced Dynamic PRISMA Flow Generator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Global Settings"),
      textInput("chart_title", "Chart Title", value = "Study Identification and Inclusion Flow"),
      numericInput("n_total", "Total Records Identified (e.g., ClinicalTrials.gov)", value = 9743),
      
      hr(),
      h4("2. Processing Stages"),
      p("Define your flow steps (e.g., 'Pre-Screening', 'Screening')."),
      
      # This UI component is generated dynamically from the server
      uiOutput("stages_ui"),
      
      div(style="margin-top: 10px;",
          actionButton("add_stage_btn", "Add New Stage", icon = icon("layer-group"), class = "btn-primary")
      ),
      
      hr(),
      h4("3. Manual Additions"),
      p("Studies added from other sources (bypass screening)."),
      uiOutput("additions_ui"),
      
      hr(),
      h4("4. Data Management"),
      div(style="display:flex; gap:10px; margin-bottom: 10px;",
          downloadButton("save_json", "Save Project"),
          fileInput("load_json", NULL, buttonLabel = "Load Project", accept = ".json")
      ),
      h4("Export Chart"),
      div(style="display:flex; gap:10px;",
          downloadButton("exp_png", "Download PNG"),
          downloadButton("exp_pdf", "Download PDF")
      )
    ),
    
    mainPanel(
      h3("Flow Chart Visualization"),
      div(style = "border: 1px solid #ddd; padding: 20px; border-radius: 4px; background: white; overflow: auto;",
          grVizOutput("prisma_plot", height = "900px")
      ),
      br(),
      wellPanel(
        h4("Detailed Summary"),
        tableOutput("summary_table")
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # --- 1. Robust Data Model ---
  id_counter <- reactiveVal(2) 
  
  data <- reactiveValues(
    stages = list(
      list(id = 1, name = "Pre-Screening", exclusions = data.frame(reason = character(), count = numeric(), stringsAsFactors=FALSE)),
      list(id = 2, name = "Screening", exclusions = data.frame(reason = character(), count = numeric(), stringsAsFactors=FALSE))
    ),
    # Modified structure to include 'details' column
    additions = data.frame(reason = character(), count = numeric(), details = character(), stringsAsFactors=FALSE)
  )
  
  # --- Helper: Text Wrapper ---
  # Wraps text to a specific width to create "margins" and new lines
  wrap_txt <- function(txt, width = 45, sep = "\\n") {
    if (is.null(txt) || is.na(txt) || txt == "") return("")
    # str_wrap breaks the text, paste collapses it with graphviz newline (\n)
    # We use \l instead of \n for left alignment if desired, but \n centers it. 
    # For this specific look (left aligned lists), we handle alignment in the graph code.
    paste(str_wrap(txt, width = width), collapse = sep)
  }
  
  # --- 2. Centralized "Add Exclusion" Logic ---
  observeEvent(input$add_excl_trigger, {
    target_id <- input$add_excl_trigger
    r <- input[[paste0("excl_rsn_", target_id)]]
    c <- input[[paste0("excl_cnt_", target_id)]]
    
    if (!is.null(c) && c > 0 && r != "") {
      idx <- which(sapply(data$stages, function(x) x$id) == target_id)
      if(length(idx) > 0) {
        data$stages[[idx]]$exclusions <- rbind(data$stages[[idx]]$exclusions, 
                                               data.frame(reason=r, count=c, stringsAsFactors=FALSE))
        updateTextInput(session, paste0("excl_rsn_", target_id), value="")
        updateNumericInput(session, paste0("excl_cnt_", target_id), value=0)
      }
    }
  })
  
  # --- 3. Stage Management ---
  observeEvent(input$add_stage_btn, {
    new_id <- id_counter() + 1
    id_counter(new_id)
    new_stage <- list(
      id = new_id, 
      name = paste("New Stage", new_id), 
      exclusions = data.frame(reason = character(), count = numeric(), stringsAsFactors=FALSE)
    )
    data$stages[[length(data$stages) + 1]] <- new_stage
  })
  
  observe({
    lapply(seq_along(data$stages), function(i) {
      s_id <- data$stages[[i]]$id
      inp_val <- input[[paste0("stage_name_", s_id)]]
      if(!is.null(inp_val) && inp_val != data$stages[[i]]$name) {
        data$stages[[i]]$name <- inp_val
      }
    })
  })
  
  # --- 4. UI Rendering ---
  output$stages_ui <- renderUI({
    stage_panels <- lapply(data$stages, function(stage) {
      i <- stage$id 
      wellPanel(style = "background-color: #fcfcfc; border-left: 5px solid #337ab7;",
                div(style="display:flex; justify-content:space-between; align-items:center;",
                    textInput(paste0("stage_name_", i), label = NULL, value = stage$name, placeholder = "Stage Name"),
                    tags$button(class = "btn btn-danger btn-xs", icon("trash"),
                                onclick = sprintf("Shiny.setInputValue('del_stage_trigger', %d, {priority: 'event'})", i))
                ),
                div(style="display:flex; gap:5px; align-items:flex-end;",
                    div(style="flex-grow:1;", textInput(paste0("excl_rsn_", i), "Exclusion Reason", placeholder="e.g. Wrong Date")),
                    div(style="width:70px;", numericInput(paste0("excl_cnt_", i), "Count", value = 0, min=0)),
                    tags$button(class = "btn btn-default", type = "button", icon("plus"),
                                onclick = sprintf("Shiny.setInputValue('add_excl_trigger', %d, {priority: 'event'})", i))
                ),
                tags$ul(style="padding-left: 15px; margin-top: 5px;",
                        if(nrow(stage$exclusions) > 0) {
                          lapply(1:nrow(stage$exclusions), function(r) {
                            row <- stage$exclusions[r,]
                            tags$li(tags$b(row$count), " - ", row$reason,
                                    tags$a(icon("times"), href="#", style="color:red; margin-left:5px;",
                                           onclick = sprintf("Shiny.setInputValue('del_excl_trigger', '%d_%d', {priority: 'event'})", i, r)))
                          })
                        } else { tags$li("No exclusions yet.", style="color:grey; list-style:none;") }
                )
      )
    })
    do.call(tagList, stage_panels)
  })
  
  # --- 5. Deletion Logic ---
  observeEvent(input$del_excl_trigger, {
    parts <- strsplit(input$del_excl_trigger, "_")[[1]]
    stage_id <- as.numeric(parts[1])
    row_idx <- as.numeric(parts[2])
    idx <- which(sapply(data$stages, function(x) x$id) == stage_id)
    if(length(idx) > 0) {
      data$stages[[idx]]$exclusions <- data$stages[[idx]]$exclusions[-row_idx, , drop=FALSE]
    }
  })
  
  observeEvent(input$del_stage_trigger, {
    stage_id <- input$del_stage_trigger
    showModal(modalDialog(
      title = "Confirm Deletion", "Are you sure you want to delete this stage?",
      footer = tagList(modalButton("Cancel"), actionButton("confirm_del_stage", "Delete", class="btn-danger",
                                                           onclick = sprintf("Shiny.setInputValue('confirm_del_stage_id', %d)", stage_id)))
    ))
  })
  
  observeEvent(input$confirm_del_stage_id, {
    removeModal()
    idx <- which(sapply(data$stages, function(x) x$id) == input$confirm_del_stage_id)
    if(length(idx) > 0) data$stages[[idx]] <- NULL
  })
  
  # --- 6. Manual Additions Logic (Updated for Details) ---
  output$additions_ui <- renderUI({
    tagList(
      div(style="margin-bottom: 5px;",
          textInput("add_rsn", "Category / Source", placeholder="e.g. Include IO based Therapy"),
          textAreaInput("add_details", "Details / IDs (Optional)", placeholder="NCT06206096\nNCT04194359", rows=3)
      ),
      div(style="display:flex; gap:5px; align-items:flex-end;",
          div(style="width:100px;", numericInput("add_cnt", "Count", value = 0, min=0)),
          div(style="flex-grow:1;", actionButton("add_manual_btn", "Add Entry", icon = icon("plus"), class="btn-info btn-block"))
      ),
      tags$ul(style="padding-left: 15px; margin-top: 15px;",
              if(nrow(data$additions) > 0) {
                lapply(1:nrow(data$additions), function(r) {
                  row <- data$additions[r,]
                  tags$li(
                    tags$b(row$count), " - ", row$reason,
                    tags$a(icon("times"), href="#", style="color:red; margin-left:5px;",
                           onclick = sprintf("Shiny.setInputValue('del_add_trigger', %d, {priority: 'event'})", r)),
                    if(row$details != "") tags$pre(style="font-size:10px; margin-top:2px; background:#eee;", row$details)
                  )
                })
              }
      )
    )
  })
  
  observeEvent(input$add_manual_btn, {
    if(input$add_cnt > 0 && input$add_rsn != "") {
      data$additions <- rbind(data$additions, data.frame(
        reason = input$add_rsn, 
        count = input$add_cnt, 
        details = input$add_details, # Store details
        stringsAsFactors=FALSE
      ))
      updateTextInput(session, "add_rsn", value="")
      updateNumericInput(session, "add_cnt", value=0)
      updateTextAreaInput(session, "add_details", value="")
    }
  })
  
  observeEvent(input$del_add_trigger, {
    data$additions <- data$additions[-as.numeric(input$del_add_trigger), , drop=FALSE]
  })
  
  # --- 7. Calculation ---
  calc_flow <- reactive({
    current_count <- if(is.na(input$n_total)) 0 else input$n_total
    flow_steps <- list()
    flow_steps[[1]] <- list(type="start", name="Identification", count=current_count)
    
    for(i in seq_along(data$stages)) {
      stg <- data$stages[[i]]
      excl_sum <- sum(stg$exclusions$count)
      flow_steps[[length(flow_steps)+1]] <- list(type = "exclusion", stage_name = stg$name, reasons = stg$exclusions, removed = excl_sum, parent_count = current_count)
      current_count <- current_count - excl_sum
      flow_steps[[length(flow_steps)+1]] <- list(type = "node", name = paste("Post", stg$name), count = current_count)
    }
    
    add_sum <- sum(data$additions$count)
    if(add_sum > 0) {
      flow_steps[[length(flow_steps)+1]] <- list(type = "addition", reasons = data$additions, added = add_sum)
      current_count <- current_count + add_sum
    }
    list(steps = flow_steps, final = current_count)
  })
  
  # --- 8. Graph Code Generation (Updated for Layout & Formatting) ---
  # --- 8. Graph Code Generation ---
  # --- 8. Graph Code Generation ---
  graph_code <- reactive({
    f <- calc_flow()
    steps <- f$steps
    
    # 1. Prepare Title Text (Centered Newlines)
    # wrap_txt with sep="\\n" ensures lines break and center
    title_text <- wrap_txt(input$chart_title, width=50, sep="\\n")
    
    # 2. Define Global Graph Attributes
    # labelloc="t" -> Places label at the Top
    # labeljust="c" -> Centers the label
    # size="8.27,11.69" -> A4 Portrait dimensions (in inches)
    # ratio="compress" -> Scales graph to fit within the size without distorting aspect ratio
    dot <- paste0("digraph prisma { \n graph [layout = dot, rankdir = TB, splines=ortho, nodesep=0.5, ranksep=0.5, fontname = \"Helvetica\",
                   labelloc=\"t\", labeljust=\"c\", fontsize=20, margin=0.5,
                   size=\"8.27,11.69\", ratio=\"compress\",
                   label=\"", title_text, "\"]; \n 
                   node [shape = box, style = filled, fillcolor = White, width=4.0, fontname = \"Helvetica\"]; \n")
    
    # 3. Start Node (Title is now handled by 'graph', so we start directly with node_0)
    prev_node <- "node_0"
    dot <- paste0(dot, "node_0 [label='Records Identified\\n(n = ", steps[[1]]$count, ")']; \n")
    
    node_idx <- 1
    
    for(s in steps) {
      if(s$type == "exclusion") {
        ex_id <- paste0("excl_", node_idx)
        
        # Build Exclusion Label
        if(nrow(s$reasons) > 0) {
          # Header (Centered \n)
          lbl_head <- paste0("Excluded in ", s$stage_name, "\\n(n = ", s$removed, ")")
          
          # Body (Left Aligned \l)
          lbl_body <- paste(
            sapply(1:nrow(s$reasons), function(x) {
              rsn <- s$reasons$reason[x]
              cnt <- s$reasons$count[x]
              # Format: Reason (n=X)
              paste0(wrap_txt(rsn, 50, sep="\\l"), " (n=", cnt, ")")
            }), 
            collapse="\\l" 
          )
          
          # Combine: Header + \n\n + Body + \l
          label_txt <- paste0(lbl_head, "\\n\\n", lbl_body, "\\l") 
        } else {
          label_txt <- paste0("Excluded in ", s$stage_name, "\\n(n = 0)")
        }
        
        dot <- paste0(dot, ex_id, " [label='", label_txt, "', fillcolor='white']; \n")
      }
      
      if(s$type == "node") {
        next_id <- paste0("node_", node_idx)
        lbl <- paste0("Records remaining\\n(n = ", s$count, ")")
        if(grepl("Screening", s$name, ignore.case=TRUE)) lbl <- paste0("Records screened\\n(n = ", s$count, ")")
        if(grepl("Eligibility", s$name, ignore.case=TRUE)) lbl <- paste0("Reports assessed for eligibility\\n(n = ", s$count, ")")
        
        dot <- paste0(dot, next_id, " [label='", lbl, "']; \n")
        dot <- paste0(dot, prev_node, " -> ", next_id, "; \n")
        
        # Exclusion Edge and Ranking
        ex_prev_id <- paste0("excl_", node_idx)
        dot <- paste0(dot, prev_node, " -> ", ex_prev_id, " [minlen=2]; \n")
        dot <- paste0(dot, "{ rank = same; ", prev_node, "; ", ex_prev_id, "; } \n")
        
        prev_node <- next_id
        node_idx <- node_idx + 1
      }
      
      if(s$type == "addition") {
        add_id <- "add_node"
        
        # Build Addition Label with Details
        if(nrow(s$reasons) > 0) {
          header_txt <- paste0("Additional Trials Requested\\n(n = ", s$added, ")")
          
          body_txt <- paste(sapply(1:nrow(s$reasons), function(x) {
            row <- s$reasons[x,]
            cat_line <- paste0("- ", row$reason, " (", row$count, ")")
            det_line <- ""
            if(!is.na(row$details) && row$details != "") {
              det_splits <- strsplit(row$details, "\n")[[1]]
              det_clean <- paste0("    ", det_splits) 
              det_line <- paste(det_clean, collapse="\\l") 
              det_line <- paste0("\\l", det_line)
            }
            paste0(cat_line, det_line)
          }), collapse="\\l") 
          
          label_txt <- paste0(header_txt, "\\n\\l", body_txt, "\\l")
        } else {
          label_txt <- "Additional Records"
        }
        
        dot <- paste0(dot, add_id, " [label='", label_txt, "', width=4.5]; \n")
        
        final_id <- "final_included"
        dot <- paste0(dot, final_id, " [label='Studies Included in Review\\n(n = ", f$final, ")', style='filled', fillcolor='#E0F7FA']; \n")
        
        dot <- paste0(dot, prev_node, " -> ", final_id, "; \n")
        dot <- paste0(dot, add_id, " -> ", final_id, "; \n")
        dot <- paste0(dot, "{ rank = same; ", prev_node, "; ", add_id, "; } \n")
        prev_node <- final_id 
      }
    }
    
    if(!any(sapply(steps, function(x) x$type == "addition"))) {
      dot <- paste0(dot, prev_node, " [fillcolor='#E0F7FA', label='Studies Included in Review\\n(n = ", f$final, ")']; \n")
    }
    
    dot <- paste0(dot, "}")
    dot
  })
  
  output$summary_table <- renderTable({
    f <- calc_flow()
    df_list <- lapply(f$steps, function(s) {
      if(s$type == "start") return(data.frame(Event = "Start", Change = "", Result = s$count))
      if(s$type == "exclusion") return(data.frame(Event = paste("Excluded in", s$stage_name), Change = paste0("-", s$removed), Result = ""))
      if(s$type == "node") return(data.frame(Event = "Remaining", Change = "", Result = s$count))
      if(s$type == "addition") return(data.frame(Event = "Manual Additions", Change = paste0("+", s$added), Result = ""))
    })
    do.call(rbind, df_list)
  })
  
  output$prisma_plot <- renderGrViz({ grViz(graph_code()) })
  
  output$exp_png <- downloadHandler(filename = "prisma_flow.png", content = function(file) { rsvg_png(charToRaw(export_svg(grViz(graph_code()))), file) })
  output$exp_pdf <- downloadHandler(
    filename = "prisma_flow.pdf", 
    content = function(file) { 
      # Because we added size=A4 to the DOT code, rsvg_pdf will respect that aspect ratio
      rsvg_pdf(charToRaw(export_svg(grViz(graph_code()))), file) 
    }
  )  
  output$save_json <- downloadHandler(
    filename = function() { paste0("prisma_project_", Sys.Date(), ".json") },
    content = function(file) { write_json(list(n_total = input$n_total, stages = data$stages, additions = data$additions), file, auto_unbox = TRUE) }
  )
  
  observeEvent(input$load_json, {
    req(input$load_json)
    tryCatch({
      imp <- read_json(input$load_json$datapath, simplifyVector = TRUE)
      updateNumericInput(session, "n_total", value = imp$n_total)
      
      restored_stages <- list()
      if(is.data.frame(imp$stages)) {
        for(i in 1:nrow(imp$stages)) {
          stg <- imp$stages[i,]
          excl_df <- stg$exclusions
          if(is.list(excl_df) && !is.data.frame(excl_df)) excl_df <- excl_df[[1]]
          if(is.null(excl_df)) excl_df <- data.frame(reason=character(), count=numeric())
          restored_stages[[length(restored_stages)+1]] <- list(id = stg$id, name = stg$name, exclusions = excl_df)
        }
      } else { restored_stages <- imp$stages }
      data$stages <- restored_stages
      
      # Restore Additions (handling new 'details' column for backwards compatibility)
      if(!is.null(imp$additions)) {
        adds <- imp$additions
        if(!"details" %in% colnames(adds)) adds$details <- "" 
        data$additions <- adds
      } else {
        data$additions <- data.frame(reason=character(), count=numeric(), details=character())
      }
      
      max_id <- if(length(restored_stages) > 0) max(sapply(restored_stages, function(x) x$id)) else 0
      id_counter(max(2, max_id))
    }, error = function(e) { showNotification("Error loading JSON.", type="error") })
  })
}

shinyApp(ui, server)