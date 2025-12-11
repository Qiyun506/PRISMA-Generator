library(shiny)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(jsonlite)
library(glue)
library(stringr)

# --- UI ---
ui <- fluidPage(
  titlePanel("Advanced Dynamic PRISMA Flow Generator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Global Settings"),
      textInput("chart_title", "Chart Title", value = "Study Identification and Inclusion Flow\nfor Colorectal Cancer Trials"),
      
      # --- FOOTNOTE INPUT ---
      textAreaInput("chart_footnote", "Footnote", value = "Note: Database search included Medline and Embase.\n*Duplicate removal performed automatically.", rows = 3),
      
      numericInput("n_total", "Total Records Identified", value = 9743),
      checkboxInput("show_side_labels", "Show Side Labels (Identification/Screening/Included)", value = TRUE),
      
      hr(),
      h4("2. Processing Stages"),
      p("Define your flow steps (e.g., 'Pre-Screening', 'Screening')."),
      uiOutput("stages_ui"),
      div(style="margin-top: 10px;",
          actionButton("add_stage_btn", "Add New Stage", icon = icon("layer-group"), class = "btn-primary")
      ),
      
      hr(),
      h4("3. Manual Additions"),
      p("Studies added from other sources."),
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
  
  # --- 1. Data Model ---
  id_counter <- reactiveVal(2) 
  data <- reactiveValues(
    stages = list(
      list(id = 1, name = "Pre-Screening", exclusions = data.frame(reason = character(), count = numeric(), stringsAsFactors=FALSE)),
      list(id = 2, name = "Screening", exclusions = data.frame(reason = character(), count = numeric(), stringsAsFactors=FALSE))
    ),
    additions = data.frame(reason = character(), count = numeric(), details = character(), stringsAsFactors=FALSE)
  )
  
  # --- Helper: Text Wrapper ---
  wrap_txt <- function(txt, width = 45, sep = "\\l") {
    if (is.null(txt) || is.na(txt) || txt == "") return("")
    
    # 1. Wrap the text (generates string with \n)
    wrapped <- str_wrap(txt, width = width)
    
    # 2. CRITICAL FIX: Replace standard \n (Center) with \l (Left)
    # This forces every wrapped line to snap to the left.
    gsub("\n", sep, wrapped, fixed = TRUE)
  }
  
  # --- 2. Exclusion Logic ---
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
    new_stage <- list(id = new_id, name = paste("New Stage", new_id), 
                      exclusions = data.frame(reason = character(), count = numeric(), stringsAsFactors=FALSE))
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
  
  # --- 4. UI Rendering (Stages) ---
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
  
  # --- 6. Manual Additions Logic ---
  # --- 6. Manual Additions Logic ---
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
                  
                  # SAFETY CHECK: Ensure details is text and not NA
                  det_text <- as.character(row$details)
                  has_details <- !is.na(det_text) && det_text != ""
                  
                  tags$li(
                    tags$b(row$count), " - ", row$reason,
                    tags$a(icon("times"), href="#", style="color:red; margin-left:5px;",
                           onclick = sprintf("Shiny.setInputValue('del_add_trigger', %d, {priority: 'event'})", r)),
                    
                    # Only render the grey box if we have valid details
                    if(has_details) {
                      tags$pre(style="font-size:10px; margin-top:2px; background:#eee;", det_text)
                    }
                  )
                })
              }
      )
    )
  })
  
  observeEvent(input$add_manual_btn, {
    if(input$add_cnt > 0 && input$add_rsn != "") {
      # Create the new row
      new_row <- data.frame(
        reason = input$add_rsn, 
        count = input$add_cnt, 
        details = input$add_details, 
        stringsAsFactors = FALSE # CRITICAL: Prevents text from becoming Factors
      )
      
      data$additions <- rbind(data$additions, new_row)
      
      # Clear inputs
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
  
  # --- 8. Graph Code Generation ---
  # --- 8. Graph Code Generation ---
  # --- 8. Graph Code Generation ---
  graph_code <- reactive({
    f <- calc_flow()
    steps <- f$steps
    
    title_text <- wrap_txt(input$chart_title, width=50, sep="\\n")
    raw_footnote <- input$chart_footnote
    
    # --- DOT SETTINGS ---
    # splines=ortho: Orthogonal edges (right angles)
    # nodesep=0.8: Good separation to prevent label overlap
    dot <- "digraph prisma { \n graph [layout = dot, rankdir = TB, splines=ortho, 
            nodesep=0.8, ranksep=0.5, fontname = \"Helvetica\",
            fontsize=12, margin=\"0.5\"]; \n 
            node [shape = box, style = filled, fillcolor = White, width=3.5, fontname = \"Helvetica\"]; \n"
    
    # --- 1. TITLE NODE (Anchored to Spine) ---
    # We remove the huge width. We give it 'group=main' to lock it to the center flow.
    dot <- paste0(dot, "main_title [label='", title_text, "', shape=box, style='filled,rounded', 
                  fillcolor='#FFC107', fontsize=18, width=5, height=1, group='main']; \n")
    
    # --- 2. SIDE LABELS (Independent Column) ---
    if(input$show_side_labels) {
      dot <- paste0(dot, "node [shape=box, style='filled,rounded', fillcolor='#90CAF9', width=1.5, fontsize=12, group='side']; \n")
      dot <- paste0(dot, "lbl_id [label='Identification']; \n")
      dot <- paste0(dot, "lbl_scr [label='Screening']; \n")
      dot <- paste0(dot, "lbl_inc [label='Included']; \n")
      
      # Link labels vertically so they stay in a column
      dot <- paste0(dot, "edge [style=invis, weight=100]; \n")
      dot <- paste0(dot, "lbl_id -> lbl_scr -> lbl_inc; \n")
      dot <- paste0(dot, "edge [style=solid, weight=1]; \n") # Reset defaults
    }
    
    # --- 3. MAIN FLOW (The Spine) ---
    # All nodes here get group='main' to align with Title
    dot <- paste0(dot, "node [shape=box, style=filled, fillcolor=White, width=3.5, fontsize=14, group='main']; \n") 
    
    prev_node <- "node_0"
    
    # Define First Node
    dot <- paste0(dot, "node_0 [label='Records Identified\\n(n = ", steps[[1]]$count, ")']; \n")
    
    # CRITICAL: Connect Title -> First Node with High Weight
    # This locks the Title directly above the first node.
    dot <- paste0(dot, "main_title -> node_0 [style=invis, weight=1000]; \n")
    
    # Align Label 1 with Node 0
    if(input$show_side_labels) {
      dot <- paste0(dot, "{ rank = same; lbl_id; node_0; } \n")
      # Invisible strut to keep distance between label and flow
      dot <- paste0(dot, "lbl_id -> node_0 [style=invis, minlen=2]; \n")
    }
    
    node_idx <- 1
    screening_label_placed <- FALSE
    
    for(s in steps) {
      
      # --- EXCLUSION (Branch Right) ---
      if(s$type == "exclusion") {
        ex_id <- paste0("excl_", node_idx)
        
        if(nrow(s$reasons) > 0) {
          lbl_head <- paste0("Excluded in ", s$stage_name, "\\n(n = ", s$removed, ")")
          lbl_body <- paste(sapply(1:nrow(s$reasons), function(x) {
            paste0(wrap_txt(s$reasons$reason[x], 40, sep="\\l"), " (n=", s$reasons$count[x], ")")
          }), collapse="\\l") 
          label_txt <- paste0(lbl_head, "\\n\\n", lbl_body, "\\l") 
        } else {
          label_txt <- paste0("Excluded in ", s$stage_name, "\\n(n = 0)")
        }
        
        # Exclusions do NOT have group='main', so they can float right
        dot <- paste0(dot, ex_id, " [label='", label_txt, "', fillcolor='white', group='exclusions']; \n")
      }
      
      # --- MAIN NODE (Continue Spine) ---
      if(s$type == "node") {
        next_id <- paste0("node_", node_idx)
        lbl <- paste0("Records remaining\\n(n = ", s$count, ")")
        
        dot <- paste0(dot, next_id, " [label='", lbl, "']; \n")
        
        # CRITICAL: High weight edge to keep the spine straight
        dot <- paste0(dot, prev_node, " -> ", next_id, " [weight=1000]; \n")
        
        # Link Exclusion (Low weight, branches off)
        ex_prev_id <- paste0("excl_", node_idx)
        dot <- paste0(dot, prev_node, " -> ", ex_prev_id, " [minlen=2]; \n")
        dot <- paste0(dot, "{ rank = same; ", prev_node, "; ", ex_prev_id, "; } \n")
        
        # Place Screening Side Label at the first screening step
        if(input$show_side_labels && !screening_label_placed) {
          dot <- paste0(dot, "{ rank = same; lbl_scr; ", next_id, "; } \n")
          dot <- paste0(dot, "lbl_scr -> ", next_id, " [style=invis, minlen=2]; \n")
          screening_label_placed <- TRUE
        }
        
        prev_node <- next_id
        node_idx <- node_idx + 1
      }
      
      # --- ADDITIONS ---
      # --- ADDITION NODE GENERATION ---
      if(s$type == "addition") {
        add_id <- "add_node"
        
        # FIX: Calculate the full label including details
        if(nrow(s$reasons) > 0) {
          # 1. Header
          header_txt <- paste0("Additional Trials\\n(n = ", s$added, ")")
          
          # 2. Body (Loop through reasons and details)
          body_txt <- paste(sapply(1:nrow(s$reasons), function(x) {
            row <- s$reasons[x,]
            
            # Extract details if they exist
            det <- ""
            if(!is.na(row$details) && row$details != "") {
              # Add indentation for the details
              det <- paste0("\\l    ", paste(strsplit(row$details, "\n")[[1]], collapse="\\l    "))
            }
            # Combine Category + Count + Details
            paste0("- ", row$reason, " (", row$count, ")", det)
          }), collapse="\\l")
          
          # 3. Combine
          label_txt <- paste0(header_txt, "\\n\\l", body_txt, "\\l")
        } else { 
          label_txt <- "Additional Records" 
        }
        
        # Draw the Node
        dot <- paste0(dot, add_id, " [label='", label_txt, "', width=3.5]; \n")
        
        final_id <- "final_included"
        # Assign Final Node to 'main' group to keep it aligned with the Title
        dot <- paste0(dot, final_id, " [label='Studies Included in Review\\n(n = ", f$final, ")', style='filled', fillcolor='#E0F7FA', group='main']; \n")
        
        # Connect everything
        dot <- paste0(dot, "{ rank = same; ", prev_node, "; ", add_id, "; } \n")
        dot <- paste0(dot, prev_node, " -> ", final_id, " [weight=1000]; \n") # Keep the spine straight
        dot <- paste0(dot, add_id, " -> ", final_id, "; \n")
        prev_node <- final_id 
      }
    }
    
    # --- FINAL CONNECTION ---
    if(!any(sapply(steps, function(x) x$type == "addition"))) {
      final_id <- "final_included"
      # Ensure Final Node is in group='main'
      dot <- paste0(dot, final_id, " [fillcolor='#E0F7FA', label='Studies Included in Review\\n(n = ", f$final, ")', group='main']; \n")
      
      # High weight to finish the spine
      dot <- paste0(dot, prev_node, " -> ", final_id, " [weight=1000]; \n") 
    }
    
    # --- ALIGN FINAL LABEL ---
    if(input$show_side_labels) {
      dot <- paste0(dot, "{ rank = same; lbl_inc; final_included; } \n")
      dot <- paste0(dot, "lbl_inc -> final_included [style=invis, minlen=2]; \n")
    }
    
    # --- FOOTNOTE ---
    if(raw_footnote != "") {
      fn_wrapped <- wrap_txt(raw_footnote, width=100, sep="<BR/>")
      lbl_html <- paste0(
        "<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>",
        "<TR><TD WIDTH='600' HEIGHT='10'></TD></TR>", 
        "<TR><TD BORDER='1' SIDES='T' ALIGN='LEFT' BALIGN='TOP' CELLPADDING='5'>",
        "<FONT POINT-SIZE='10'>", fn_wrapped, "</FONT>",
        "</TD></TR></TABLE>>"
      )
      dot <- paste0(dot, "node_footnote [label=", lbl_html, ", shape=plain, group='main']; \n")
      
      # Anchor footnote to the spine as well
      dot <- paste0(dot, final_id, " -> node_footnote [style=invis, weight=1000]; \n")
    }
    
    dot <- paste0(dot, "}")
    dot
  })
  
  # --- 9. Outputs ---
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
    content = function(file) { rsvg_pdf(charToRaw(export_svg(grViz(graph_code()))), file) }
  )  
  output$save_json <- downloadHandler(
    filename = function() { paste0("prisma_project_", Sys.Date(), ".json") },
    content = function(file) { write_json(list(n_total = input$n_total, stages = data$stages, additions = data$additions, footnote = input$chart_footnote), file, auto_unbox = TRUE) }
  )
  
  observeEvent(input$load_json, {
    req(input$load_json)
    tryCatch({
      imp <- read_json(input$load_json$datapath, simplifyVector = TRUE)
      updateNumericInput(session, "n_total", value = imp$n_total)
      if(!is.null(imp$footnote)) updateTextAreaInput(session, "chart_footnote", value = imp$footnote)
    }, error = function(e) { showNotification("Error loading JSON.", type="error") })
  })
}

shinyApp(ui, server)