library(shiny)

# ---------------------------------------------
# Load and prepare puzzle data
# ---------------------------------------------

# Read the full CSV that contains all weekly puzzles
puzzles <- read.csv("weekly_puzzles.csv", stringsAsFactors = FALSE)

# Select the puzzle for the current week
current_week <- format(Sys.Date(), "%Y-%m-%d")
puz <- subset(puzzles, week == current_week)

# If no puzzle exists for this week, fall back to the most recent one
if (nrow(puz) == 0) {
  latest <- max(unique(puzzles$week))
  puz <- subset(puzzles, week == latest)
}

# Store the words to display as tile labels
tiles_base <- puz$word

# Build the grouping structure
# Each group gets its tile indices, category label, and explanation hint
groups <- lapply(split(puz, puz$group), function(df) {
  list(
    indices = which(tiles_base %in% df$word),
    category = df$category[1],
    hint = df$hint[1]
  )
})

# ---------------------------------------------
# UI
# ---------------------------------------------
ui <- fluidPage(
#  titlePanel("Linking Paths"),
  headerPanel("Linking Paths", windowTitle = "Linking Paths"),
  tags$p("Creator: Patricio Troncoso, The University of Edinburgh"),
  wellPanel(tags$p("This game is designed for you to find and link the concepts through their 'correct' paths.
                   There may be some conceptual overlap between groups of ideas and that is expected. 
                   Nevertheless, there are some groups of concepts that are more related to each other.")),
  wellPanel(tags$p(strong("Instructions: "), "Select four tiles of concepts that you think are related and click 'submit'.
                   There are four main overarching topics that we discussed in the Youth Studies lecture. 
                   Hopefully, you can find all four groups.")),
  wellPanel(
    tags$p(strong("These are the main ideas and/or categories of concepts to find this week:")),
    uiOutput("categories_list"),
    div(style = "margin-top: 8px;",
        actionButton("show_hints", "Show hints")
    ),
    uiOutput("hints_panel")   # appears after clicking the button
  ),
  uiOutput("tiles_grid"),
  hr(),
  actionButton("submit", "Submit Selection"),
  actionButton("shuffle", "Shuffle Tiles"),
  hr(),
  textOutput("message"),
  hr(),
  textOutput("found"),
  hr(),
  textOutput("attempts_left"),
  hr(),
  wellPanel(
    tags$p(
      strong("Privacy notice: "),
      "This teaching app does not ask for or store personal information. ",
      "Your selections are processed only in your browser session. ",
      "The hosting provider (shinyapps.io) may record basic technical logs ",
      "(e.g., IP address and browser type) to operate the service. ",
      "Please do not enter or upload any personal data."
    )
  ),
  wellPanel(
    tags$p(
      strong("Credits and Acknowledgements: "),
      "Built by Patricio Troncoso, University of Edinburgh. ",
      "This game was inspired by the NY Times Connections game, but it is in no way related to it.",
      "This game was created for educational purposes only.",
      "Credit also goes to: ", tags$a(href="https://deanattali.com/blog/shiny-game-lightsout/","Dean Attali"), 
      " for inspiration to code this game in RShiny.",
      "Parts of this app were drafted with assistance from ELM (the University of Edinburgh’s GPT-4-based AI service). ",
      "All code and content were reviewed and tested by the author."
    )
  ),
  
  wellPanel(
    tags$p(
      "The public repository of this app is: ", tags$a(href="https://github.com/patroncos/linking_paths"," here") )
  )
  
)

# ---------------------------------------------
# Server logic
# ---------------------------------------------
server <- function(input, output, session) {
  
  # Reactive values for game state
  shuffled <- reactiveVal(sample(seq_along(tiles_base)))   # Random order of tiles
  selected <- reactiveVal(integer(0))                      # Positions of currently selected tiles
  solved_tiles <- reactiveVal(integer(0))                  # Tile indices already solved
  found_groups <- reactiveVal(list())                      # Record of solved groups
  attempts <- reactiveVal(10)                               # Number of attempts left
  game_over <- reactiveVal(FALSE)                          # Game over state flag

  format_correct_groups <- function(groups, tiles_base) {
    paste(
      lapply(groups, function(g) {
        words <- tiles_base[g$indices]
        paste0(
          g$category, ": ",
          paste(words, collapse = ", ")
        )
      }),
      collapse = "\n\n"
    )
  }
  
  correct_answers_text <- reactiveVal("")
  
  # ---------------------------------------------
  # Render the grid of tiles
  # ---------------------------------------------
  output$tiles_grid <- renderUI({
    cur_sel <- selected()
    solved <- solved_tiles()
    s <- shuffled()
    is_over <- game_over()
    
    fluidRow(
      lapply(seq_along(s), function(pos) {
        tile_index <- s[pos]
        label <- tiles_base[tile_index]
        
        # Determine visual state of each tile
        is_solved <- tile_index %in% solved
        is_selected <- pos %in% cur_sel
        
        # Basic color logic for solved, selected, or free tiles
        bg <- if (is_solved) "#d7f7d7" else if (is_selected) "#cfe8ff" else "#ffffff"
        
        disabled <- is_solved || is_over
        
        column(
          width = 3,
          actionButton(
            inputId = paste0("tile_", pos),
            label = label,
            style = paste0(
              "width:100%; background-color:", bg,
              "; border:1px solid #888;",
              if (disabled) "opacity:0.6; pointer-events:none;" else ""
            )
          )
        )
      })
    )
  })
  
  # ---------------------------------------------
  # Tile selection handling
  # ---------------------------------------------
  lapply(seq_along(tiles_base), function(pos) {
    local({
      p <- pos
      observeEvent(input[[paste0("tile_", p)]], {
        if (game_over()) return()
        
        s <- shuffled()
        tile_index <- s[p]
        
        # Do not allow selecting tiles already solved
        if (tile_index %in% solved_tiles()) return()
        
        # Toggle selection state
        cur <- selected()
        if (p %in% cur) {
          selected(setdiff(cur, p))
        } else {
          selected(c(cur, p))
        }
      }, ignoreInit = TRUE)
    })
  })
  
  # ---------------------------------------------
  # Submit button logic
  # ---------------------------------------------
  observeEvent(input$submit, {
    if (game_over()) return()
    
    s <- shuffled()
    sel <- selected()
    
    # Must select exactly four tiles
    if (length(sel) != 4) {
      output$message <- renderText("Select exactly four tiles before submitting")
      return()
    }
    
    originals <- s[sel]
    
    matched <- NULL
    matched_category <- NULL
    matched_hint <- NULL
    
    # Check if the submitted four tiles match any group
    for (g in groups) {
      if (setequal(originals, g$indices)) {
        matched <- g$indices
        matched_category <- g$category
        matched_hint <- g$hint
        break
      }
    }
    
    # ---------------------------------------------
    # Correct group submission
    # ---------------------------------------------
    if (!is.null(matched)) {
      
      # Mark these tiles as solved
      solved_tiles(unique(c(solved_tiles(), matched)))
      
      # Record the solved group
      found_groups(
        append(found_groups(), list(list(
          indices = matched,
          category = matched_category,
          hint = matched_hint
        )))
      )
      
      output$message <- renderText(
        paste("Correct group. Category:", matched_category)
      )
      
      # Check if all groups are now solved
      if (length(found_groups()) == length(groups)) {
        game_over(TRUE)
        output$message <- renderText("All groups solved. Well done, you're a Linking Paths master!.")
      }
      
    } else {
      # Incorrect submission
      s <- shuffled()
      sel <- selected()
      originals <- s[sel]
      
      # Are we one away? (exactly 3 tiles match any group)
      one_away <- any(vapply(groups, function(g) {
        sum(originals %in% g$indices) == 3
      }, logical(1)))
      
      # Decrement attempts safely and branch on game over
      new_attempts <- attempts() - 1
      attempts(new_attempts)
      
      if (new_attempts <= 0) {
        game_over(TRUE)
        solved_tiles(unique(unlist(lapply(groups, function(g) g$indices))))
        correct_answers_text(format_correct_groups(groups, tiles_base))
        output$message <- renderText(
          paste(
            "Better luck next time.",
            "Here are the correct groupings:\n\n",
            correct_answers_text()
          )
        )
      } else {
        if (one_away) {
          output$message <- renderText("One away!")
        } else {
          output$message <- renderText("Incorrect group. Try again.")
        }
      }
    }
    
    
    # Reset selection after submission
    selected(integer(0))
  })
  
  
  # Toggle for showing/hiding hints
  show_hints <- reactiveVal(FALSE)
  
  observeEvent(input$show_hints, {
    show_hints(!show_hints())
    shiny::updateActionButton(
      session, "show_hints",
      label = if (show_hints()) "Hide hints" else "Show hints"
    )
  })
  
  # Render one fixed example word per category
  output$hints_panel <- renderUI({
    if (!show_hints()) return(NULL)
    
    cats <- vapply(groups, function(g) g$category, character(1))
    
    # Pick a stable example word: alphabetically first within each group
    example_words <- vapply(groups, function(g) {
      words <- tiles_base[g$indices]
      words[order(tolower(words))][1]
    }, character(1))
    
    solved_cats <- vapply(found_groups(), function(x) x$category, character(1))
    
    tags$div(
      tags$hr(),
      tags$p(strong("Hints (one per category):")),
      tags$ul(
        lapply(seq_along(cats), function(i) {
          cat_i <- cats[i]
          ex_i  <- example_words[i]
          is_solved <- cat_i %in% solved_cats
          style <- if (is_solved) "color:#6c757d;" else ""  # grey out solved
          tags$li(tags$span(style = style, paste0(cat_i, ": e.g., ", ex_i)))
        })
      )
    )
  })
  
  
  # ---------------------------------------------
  # Shuffle tiles button
  # ---------------------------------------------
  observeEvent(input$shuffle, {
    shuffled(sample(seq_along(tiles_base)))
    game_over(FALSE)
    output$message <- renderText("")
  })
  
  # Informational outputs
  output$found <- renderText({
    paste("Groups found:", length(found_groups()))
  })
  
  output$attempts_left <- renderText({
    paste("Attempts remaining:", attempts())
  })

  # tick correct categories        
  
  output$categories_list <- renderUI({
    cats <- vapply(groups, function(g) g$category, character(1))
    cats <- unique(cats)
    solved_cats <- vapply(found_groups(), function(x) x$category, character(1))
    
    tags$ul(
      lapply(cats, function(cat) {
        is_solved <- cat %in% solved_cats
        if (is_solved) {
          tags$li(tags$span(style = "color:#2e7d32; font-weight:600;", "\u2713 ", cat))
        } else {
          tags$li(cat)
        }
      })
    )
  })
  }

# Launch the app
shinyApp(ui, server)
