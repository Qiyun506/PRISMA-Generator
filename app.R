library(shiny)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(jsonlite)
library(glue)
library(stringr)
library(htmltools)

# --- UI ---
ui <- fluidPage(
  titlePanel("Advanced Dynamic PRISMA Flow Generator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Global Settings"),
      textInput("chart_title", "Chart Title", value = "Study Identification and Inclusion Flow"),
      textAreaInput("chart_footnote", "Footnote", value = "Note: Database search included Medline and Embase.\n* duplicate records removed manually.", rows = 4),
      numericInput("n_total", "Total Records Identified", value = 100),
      checkboxInput("show_side_labels", "Show Side Labels", value = TRUE),
      
      hr(),
      h4("2. Processing Stages (Exclusions)"),
      uiOutput("stages_ui"),
      div(style="display: flex; justify-content: flex-end; margin-top: 10px;",
          actionButton("add_stage_btn", "Add New Stage", icon = icon("layer-group"), class = "btn-primary btn-sm")
      ),
      
      hr(),
      h4("3. Manual Additions (Inclusion)"),
      
      # --- FIX 3: Static Input Form (Prevents input breaking on refresh) ---
      div(style="display:flex; gap:5px;",
          div(style="flex-grow:1;", textInput("add_rsn", "Category", placeholder="e.g., Other sources")),
          # --- FIX 1: Align Count to Right ---
          div(style="width:100px; text-align: right;", numericInput("add_cnt", "Count", 0, min=0))
      ),
      textAreaInput("add_anno", "Annotation / Details", placeholder="Enter details... (Preserves formatting)", rows = 2),
      
      div(style="display: flex; justify-content: flex-end; width: 100%; margin-bottom: 15px;",
          actionButton("add_manual_btn", "Add Entry", icon = icon("plus"), class="btn-info btn-sm")
      ),
      
      # Dynamic List Output (Only the list refreshes now)
      uiOutput("additions_list_ui"),
      
      hr(),
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
  
  get_uid <- function() { paste0(sample(c(LETTERS, 0:9), 8, replace = TRUE), collapse = "") }
  id_counter <- reactiveVal(2) 
  
  data <- reactiveValues(
    stages = list(
      list(id = 1, name = "Pre-Screening", exclusions = data.frame(uid = get_uid(), reason = "Sample reason", count = 50, stringsAsFactors=FALSE)),
      list(id = 2, name = "Screening", exclusions = data.frame(uid = character(), reason = character(), count = numeric(), stringsAsFactors=FALSE))
    ),
    additions = data.frame(uid = character(), reason = character(), count = numeric(), details = character(), stringsAsFactors=FALSE)
  )
  
  # --- Logic: Add/Edit/Delete Exclusions ---
  observeEvent(input$add_excl_trigger, {
    req(input$add_excl_trigger)
    target_id <- as.numeric(input$add_excl_trigger$id)
    r_id <- paste0("excl_rsn_", target_id)
    c_id <- paste0("excl_cnt_", target_id)
    
    if (!is.null(input[[r_id]]) && input[[r_id]] != "") {
      idx <- which(sapply(data$stages, function(x) x$id) == target_id)
      new_row <- data.frame(uid = get_uid(), reason = input[[r_id]], count = ifelse(is.na(input[[c_id]]), 0, input[[c_id]]), stringsAsFactors = FALSE)
      data$stages[[idx]]$exclusions <- rbind(data$stages[[idx]]$exclusions, new_row)
      updateTextInput(session, r_id, value = ""); updateNumericInput(session, c_id, value = 0)
    }
  })
  
  observe({
    for (i in seq_along(data$stages)) {
      ex_df <- data$stages[[i]]$exclusions
      if (nrow(ex_df) > 0) {
        for (r in 1:nrow(ex_df)) {
          u <- ex_df$uid[r]
          if (!is.null(input[[paste0("edit_rsn_", u)]])) data$stages[[i]]$exclusions$reason[r] <- input[[paste0("edit_rsn_", u)]]
          if (!is.null(input[[paste0("edit_cnt_", u)]])) data$stages[[i]]$exclusions$count[r] <- ifelse(is.na(input[[paste0("edit_cnt_", u)]]), 0, input[[paste0("edit_cnt_", u)]])
        }
      }
    }
  })
  
  observeEvent(input$del_excl_trigger, {
    s_id <- as.numeric(input$del_excl_trigger$stage_id)
    r_idx <- as.numeric(input$del_excl_trigger$row_idx)
    idx <- which(sapply(data$stages, function(x) x$id) == s_id)
    if(length(idx) > 0) data$stages[[idx]]$exclusions <- data$stages[[idx]]$exclusions[-r_idx, , drop=FALSE]
  })
  
  # --- UI Rendering for Stages ---
  output$stages_ui <- renderUI({
    lapply(data$stages, function(stage) {
      s_id <- stage$id
      wellPanel(style = "background-color: #fcfcfc; border-left: 5px solid #337ab7; padding: 10px; margin-bottom: 10px;",
                div(style="display:flex; justify-content: space-between; align-items:center; margin-bottom: 10px;",
                    div(style="flex-grow: 1;", textInput(paste0("stage_name_", s_id), label = NULL, value = stage$name, width="100%")),
                    div(style="margin-left: 10px;", 
                        actionButton(paste0("del_stg_", s_id), "", icon = icon("trash"), class="btn-danger btn-xs", 
                                     onclick=sprintf("Shiny.setInputValue('del_stage_trigger', %d)", s_id)))
                ),
                div(style="display:flex; gap:5px; align-items:flex-end;",
                    div(style="flex-grow:1;", textInput(paste0("excl_rsn_", s_id), "Reason", placeholder = "Exclude for...")),
                    # --- FIX 2: Rename 'n' to 'Count' ---
                    div(style="width:70px;", numericInput(paste0("excl_cnt_", s_id), "Count", 0, min=0)),
                    tags$button(class = "btn btn-default", type = "button", icon("plus"),
                                onclick = sprintf("Shiny.setInputValue('add_excl_trigger', {id: %d, nonce: Math.random()}, {priority: 'event'})", s_id))
                ),
                tags$ul(style="list-style:none; padding:0; margin-top:10px;",
                        if(nrow(stage$exclusions) > 0) lapply(1:nrow(stage$exclusions), function(r) {
                          row <- stage$exclusions[r,]
                          tags$li(style="display:flex; gap:5px; margin-bottom:5px; align-items: center;",
                                  div(style="flex-grow:1;", textInput(paste0("edit_rsn_", row$uid), NULL, row$reason)),
                                  div(style="width:60px;", numericInput(paste0("edit_cnt_", row$uid), NULL, row$count)),
                                  tags$a(icon("trash"), href="#", style="color:red; margin-left: 5px;",
                                         onclick = sprintf("Shiny.setInputValue('del_excl_trigger', {stage_id: %d, row_idx: %d, nonce: Math.random()}, {priority: 'event'})", s_id, r))
                          )
                        })
                )
      )
    })
  })
  
  # --- Manual Additions UI List (Dynamic Only) ---
  output$additions_list_ui <- renderUI({
    tags$ul(style="padding-left:0; list-style:none;",
            if(nrow(data$additions) > 0) lapply(1:nrow(data$additions), function(r) {
              row <- data$additions[r,]
              tags$li(style="margin-bottom:10px; padding: 10px; border: 1px solid #eee; background: #f9f9f9; border-radius: 4px;",
                      div(style="display:flex; gap:10px; align-items: flex-start;",
                          div(style="flex-grow:1;",
                              div(style="display:flex; gap: 5px; margin-bottom: 5px;",
                                  textInput(paste0("edit_add_rsn_", row$uid), NULL, value=row$reason, width="100%", placeholder="Category"),
                                  div(style="width: 80px;", numericInput(paste0("edit_add_cnt_", row$uid), NULL, value=row$count, min=0))
                              ),
                              textAreaInput(paste0("edit_add_det_", row$uid), NULL, value=row$details, rows=2, placeholder="Details", resize="vertical")
                          ),
                          div(style="margin-left: auto; padding-left: 5px;",
                              tags$button(class = "btn btn-danger btn-sm", type = "button", icon("trash"),
                                          onclick = sprintf("Shiny.setInputValue('del_add_trigger', '%s', {priority:'event'})", row$uid))
                          )
                      )
              )
            })
    )
  })
  
  observeEvent(input$add_manual_btn, {
    if(input$add_cnt > 0 && input$add_rsn != "") {
      data$additions <- rbind(data$additions, 
                              data.frame(uid=get_uid(), reason=input$add_rsn, count=input$add_cnt, details=input$add_anno, stringsAsFactors=FALSE))
      updateTextInput(session, "add_rsn", value=""); updateNumericInput(session, "add_cnt", value=0); updateTextAreaInput(session, "add_anno", value="")
    }
  })
  
  observeEvent(input$del_add_trigger, {
    target_uid <- input$del_add_trigger
    data$additions <- data$additions[data$additions$uid != target_uid, , drop=FALSE]
  })
  
  observe({
    if (nrow(data$additions) > 0) {
      for (r in 1:nrow(data$additions)) {
        u <- data$additions$uid[r]
        val_rsn <- input[[paste0("edit_add_rsn_", u)]]; if (!is.null(val_rsn)) data$additions$reason[r] <- val_rsn
        val_cnt <- input[[paste0("edit_add_cnt_", u)]]; if (!is.null(val_cnt)) data$additions$count[r] <- ifelse(is.na(val_cnt), 0, val_cnt)
        val_det <- input[[paste0("edit_add_det_", u)]]; if (!is.null(val_det)) data$additions$details[r] <- val_det
      }
    }
  })
  
  # --- Graph Logic ---
  calc_flow <- reactive({
    current_count <- if(is.na(input$n_total)) 0 else input$n_total
    steps <- list()
    steps[[1]] <- list(type="start", name="Identification", count=current_count)
    for(i in seq_along(data$stages)) {
      stg <- data$stages[[i]]
      excl_sum <- sum(stg$exclusions$count, na.rm=TRUE)
      steps[[length(steps)+1]] <- list(type = "exclusion", stage_name = stg$name, reasons = stg$exclusions, removed = excl_sum)
      current_count <- current_count - excl_sum
      steps[[length(steps)+1]] <- list(type = "node", name = paste("Post", stg$name), count = current_count)
    }
    
    if(nrow(data$additions) > 0) {
      add_sum <- sum(data$additions$count, na.rm=TRUE)
      steps[[length(steps)+1]] <- list(type = "addition", reasons = data$additions, added = add_sum)
      current_count <- current_count + add_sum
    }
    
    list(steps = steps, final = current_count)
  })
  
  graph_code <- reactive({
    f <- calc_flow()
    steps <- f$steps
    title_text <- str_wrap(input$chart_title, width=50) %>% gsub("\n", "\\\\n", .)
    
    format_text <- function(txt, width=35) {
      if(is.null(txt) || txt == "") return("")
      lines <- unlist(strsplit(txt, "\n"))
      formatted_segments <- sapply(lines, function(l) {
        wrapped <- stringr::str_wrap(l, width = width)
        escaped <- htmltools::htmlEscape(wrapped)
        gsub("\n", "<BR ALIGN='LEFT'/>", escaped)
      })
      paste(formatted_segments, collapse = "<BR ALIGN='LEFT'/>")
    }
    
    dot <- "digraph prisma { 
      graph [layout = dot, rankdir = TB, splines=ortho, nodesep=1.0, ranksep=0.8, fontname = 'Helvetica', fontsize=12, newrank=true];
      node [shape = box, style = filled, fillcolor = White, width=3.5, fontname = 'Helvetica']; \n"
    
    dot <- paste0(dot, "main_title [label='", title_text, "', shape=box, style='filled,rounded', fillcolor='#FFC107', fontsize=16, width=5]; \n")
    
    if(input$show_side_labels) {
      dot <- paste0(dot, "node [shape=box, style='filled,rounded', fillcolor='#90CAF9', width=1.5, group=left_labels]; 
                   lbl_id [label='Identification']; 
                   lbl_scr [label='Screening']; 
                   lbl_inc [label='Included']; \n")
      dot <- paste0(dot, "edge [style=invis]; lbl_id -> lbl_scr -> lbl_inc; \n edge [style=solid]; \n")
    }
    
    dot <- paste0(dot, "node [shape=box, style=filled, fillcolor=White, width=3.5, fontsize=12, group=main]; \n") 
    prev_node <- "node_0"
    dot <- paste0(dot, "node_0 [label='Records Identified\\n(n = ", steps[[1]]$count, ")']; \n")
    dot <- paste0(dot, "main_title -> node_0 [style=invis]; \n")
    
    if(input$show_side_labels) { dot <- paste0(dot, "{ rank = same; lbl_id; node_0; } \n") }
    
    node_idx <- 1
    for(s in steps) {
      if(s$type == "exclusion") {
        ex_id <- paste0("excl_", node_idx)
        lbl_head <- paste0("<B>Excluded in ", s$stage_name, "</B><BR/>(n = ", s$removed, ")")
        
        lbl_body <- if(nrow(s$reasons) > 0) {
          paste0(paste(sapply(1:nrow(s$reasons), function(x) {
            rsn <- format_text(s$reasons$reason[x], width=35)
            paste0(rsn, " (n=", s$reasons$count[x], ")")
          }), collapse="<BR ALIGN='LEFT'/>"), "<BR ALIGN='LEFT'/>")
        } else ""
        
        full_lbl <- paste0("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0' CELLPADDING='4'><TR><TD ALIGN='CENTER'>", lbl_head, "</TD></TR><TR><TD ALIGN='LEFT'>", lbl_body, "</TD></TR></TABLE>>")
        dot <- paste0(dot, ex_id, " [label=", full_lbl, ", fillcolor='white', group=right_side, width=3]; \n")
      }
      
      if(s$type == "node") {
        next_id <- paste0("node_", node_idx)
        dot <- paste0(dot, next_id, " [label='Records remaining\\n(n = ", s$count, ")', group=main]; \n")
        dot <- paste0(dot, prev_node, " -> ", next_id, " [weight=1000]; \n")
        dot <- paste0(dot, "{ rank = same; ", prev_node, "; ", paste0("excl_", node_idx), "; } \n")
        dot <- paste0(dot, prev_node, " -> ", paste0("excl_", node_idx), " [minlen=2]; \n")
        
        if(input$show_side_labels && node_idx == floor(length(data$stages)/2) + 1) {
          dot <- paste0(dot, "{ rank = same; lbl_scr; ", next_id, "; } \n")
        }
        prev_node <- next_id
        node_idx <- node_idx + 1
      }
      
      if(s$type == "addition") {
        add_id <- "add_node"
        
        rows_html <- sapply(1:nrow(s$reasons), function(i) {
          rsn <- format_text(s$reasons$reason[i], width=35)
          cnt <- s$reasons$count[i]
          det <- format_text(s$reasons$details[i], width=35)
          
          reason_line <- paste0("<TR><TD ALIGN='LEFT'><B>", rsn, "</B> (n=", cnt, ")<BR ALIGN='LEFT'/></TD></TR>")
          detail_line <- if(nchar(det) > 0) paste0("<TR><TD ALIGN='LEFT' FONTSIZE='10' COLOR='#555555'>", det, "<BR ALIGN='LEFT'/></TD></TR>") else ""
          
          paste0(reason_line, detail_line)
        })
        
        add_lbl <- paste0("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0' CELLPADDING='4'>",
                          "<TR><TD ALIGN='CENTER'><B>Manual Additions (Total n=", s$added, ")</B></TD></TR>",
                          paste(rows_html, collapse=""),
                          "</TABLE>>")
        
        dot <- paste0(dot, add_id, " [label=", add_lbl, ", fillcolor='#F9F9F9', group=right_side, width=3]; \n")
        
        final_id <- "final_included"
        dot <- paste0(dot, final_id, " [label='Studies Included\\n(n = ", f$final, ")', style='filled', fillcolor='#E0F7FA', group=main]; \n")
        
        dot <- paste0(dot, prev_node, " -> ", final_id, " [weight=1000]; \n") 
        dot <- paste0(dot, "{ rank = same; ", final_id, "; ", add_id, "; } \n")
        dot <- paste0(dot, add_id, " -> ", final_id, ":e [dir=forward, constraint=false]; \n") 
        
        prev_node <- final_id 
      }
    }
    
    if(!any(sapply(steps, function(x) x$type == "addition"))) {
      final_id <- "final_included"
      dot <- paste0(dot, final_id, " [fillcolor='#E0F7FA', label='Studies Included\\n(n = ", f$final, ")', group=main]; \n")
      dot <- paste0(dot, prev_node, " -> ", final_id, " [weight=1000]; \n") 
    }
    
    if(input$show_side_labels) { dot <- paste0(dot, "{ rank = same; lbl_inc; final_included; } \n") }
    
    if (!is.null(input$chart_footnote) && input$chart_footnote != "") {
      fn_text <- format_text(input$chart_footnote, width=50)
      dot <- paste0(dot, "fn [label=<<TABLE BORDER='0' CELLPADDING='4'><TR><TD ALIGN='LEFT' BORDER='1' SIDES='T'>", fn_text, "<BR ALIGN='LEFT'/></TD></TR></TABLE>>, shape=plain]; \n")
      dot <- paste0(dot, "final_included -> fn [style=invis]; \n")
    }
    paste0(dot, "}")
  })
  
  output$prisma_plot <- renderGrViz({ grViz(graph_code()) })
  
  output$summary_table <- renderTable({
    f <- calc_flow()
    do.call(rbind, lapply(f$steps, function(s) {
      if(s$type == "start") data.frame(Event="Start", Change="", Result=s$count)
      else if(s$type == "exclusion") data.frame(Event=s$stage_name, Change=paste0("-", s$removed), Result="")
      else if(s$type == "node") data.frame(Event="Remaining", Change="", Result=s$count)
      else if(s$type == "addition") data.frame(Event="Additions", Change=paste0("+", s$added), Result="")
    }))
  })
  
  observeEvent(input$add_stage_btn, {
    new_id <- id_counter() + 1
    id_counter(new_id)
    data$stages[[length(data$stages) + 1]] <- list(id = new_id, name = paste("New Stage", new_id), exclusions = data.frame(uid=character(), reason = character(), count = numeric(), stringsAsFactors=FALSE))
  })
  observeEvent(input$del_stage_trigger, {
    s_id <- as.numeric(input$del_stage_trigger)
    idx <- which(sapply(data$stages, function(x) x$id) == s_id)
    if(length(idx) > 0) data$stages[[idx]] <- NULL
  })
  
  output$exp_png <- downloadHandler(filename = "prisma_flow.png", content = function(file) { rsvg_png(charToRaw(export_svg(grViz(graph_code()))), file) })
  output$exp_pdf <- downloadHandler(filename = "prisma_flow.pdf", content = function(file) { rsvg_pdf(charToRaw(export_svg(grViz(graph_code()))), file) })  
}

shinyApp(ui, server)