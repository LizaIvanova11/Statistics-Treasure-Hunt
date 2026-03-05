library(shiny)
library(bslib)
library(png)
library(grid)



# ------------------------------
# QUESTIONS
# ------------------------------
questions <- list(
  level1 = list(
    list(
      question = " A pirate wants to store the number of jewels found each day. Which R code creates a vector?",
      options = c(
        "jewels <- c(12,15,9,20)",
        "jewels = (12 15 9 20)",
        "gold <- vector(12, 15, 9, 20)",
        "gold <- list(12 - 15 - 9 - 20)"
      ),
      correct = 1,
      coordinate = c(5, 10)
    ),
    list(
      question = "Which of these items is categorical?",
      options = c("Treasure weight in kilograms", "Distance in miles", "Ship type", "Time spent exploring"),
      correct = 3,
      coordinate = c(12, 15)
    ),
    list(
      question = "In a ship’s research log, which symbol represents adding a comment to R code?",
      options = c("/", ".", "#", "s²"),
      correct = 3,
      coordinate = c(8, 20)
    ),
    list(
      question = "If you want to show how a collection of gold amounts is distributed, which plot should you draw?",
      options = c("Bar chart", "Scatterplot", "Histogram", "Pie chart"),
      correct = 3,
      coordinate = c(18, 8)
    ),
    list(
      question = "If a crew’s treasure amounts have a large standard deviation, what does that indicate?",
      options = c(
        "The amount is more spread out",
        "The amount is closer together",
        "The mean is larger",
        "There are fewer data points"
      ),
      correct = 1,
      coordinate = c(22, 22)
    )
  ),
  level2 = list(
    list(
      question = "A pirate wants to know how many rows are in treasure_map. Which function should they use?",
      options = c("rows(treasure_map)", "nrow(treasure_map)", "count(treasure_map)", "length(treasure_map)"),
      correct = 2,
      coordinate = c(10, 5)
    ),
    list(
      question = "If a treasure chart has a long tail stretching to the right, how would you describe its shape?",
      options = c("Symmetric", "Left-skewed", "Right-skewed", "Uniform"),
      correct = 3,
      coordinate = c(20, 12)
    ),
    list(
      question = "Which statistic is most affected when a crew discovers one unusually large treasure chest?",
      options = c("Median", "Mode", "Mean", "Interquartile range"),
      correct = 3,
      coordinate = c(6, 18)
    ),
    list(
      question = "If the correlation between two voyage measurements is r = 0.80, how strong is their relationship?",
      options = c("Weak positive relationship", "Strong positive relationship", "Weak negative relationship", "Strong negative relationship"),
      correct = 2,
      coordinate = c(15, 22)
    ),
    list(
      question = "Which plot best shows how two measurements from a voyage change together?",
      options = c("Bar chart", "Scatterplot", "Boxplot", "Histogram"),
      correct = 2,
      coordinate = c(23, 7)
    )
  ),
  level3 = list(
    list(
      question = "A navigator calculates a 95% confidence interval for the true weight of a jewel, what does this mean?",
      options = c(
        "95% of the weight lies within the interval",
        "95% chance the true weight mean lies in the interval",
        "The method is 95% right",
        "95% of the mean lies in the interval"
      ),
      correct = 2,
      coordinate = c(5, 22)
    ),
    list(
      question = "Which test is used to examine whether two crew-related categories are associated?",
      options = c("ANOVA", "Chi-square test", "Regression", "Independent t-test"),
      correct = 2,
      coordinate = c(12, 4)
    ),
    list(
      question = "In a voyage’s regression model, what does the slope tell you about the relationship between X and Y?",
      options = c("Predicted value of Y when X = 0", "Change in Y for each unit increase in X", "Strength of correlation", "Variance explained"),
      correct = 2,
      coordinate = c(18, 20)
    ),
    list(
      question = "A crew's model has R² = 0.64, what does this value indicate?",
      options = c("64% of the sample is explained", "64% of variance in Y is explained by X", "Correlation is 0.64", "The model is perfect"),
      correct = 2,
      coordinate = c(22, 10)
    ),
    
    list( question = "The captain wants to load the psyuntr package, which command is correct?", 
          options = c( "library(psychotools)", "library(psyuntr)", "library(psych)", "library(psyuntur)" ), 
          correct = 2,
          coordinate = c(6, 6)
    )
  )
)


# ------------------------------
# HELPERS
# ------------------------------
smooth_path <- function(start, end, steps = 18) {
  x_seq <- seq(start[1], end[1], length.out = steps)
  y_seq <- seq(start[2], end[2], length.out = steps)
  data.frame(x = x_seq, y = y_seq)
}

show_level_complete <- function(level) {
  title_text <- switch(
    as.character(level),
    "1" = "🎉 Level 1 Complete!",
    "2" = "🎉 Level 2 Complete!"
  )
  body_text <- switch(
    as.character(level),
    "1" = "Great job explorer! Press any key to sail to Level 2.",
    "2" = "You’re sailing strong! Press any key to face the Captain’s Challenge."
  )
  showModal(modalDialog(
    title = title_text,
    div(
      style = "font-size: 20px; text-align: center; padding: 10px;",
      body_text
    ),
    easyClose = FALSE,
    footer = NULL
  ))
}

# ------------------------------
# UI
ui <- page_fillable(
  theme = bs_theme(bootswatch = "minty"),
  
  # Quiet background music
  tags$audio(
    id = "bgm",
    src = "background.mp3",
    type = "audio/mp3",
    autoplay = TRUE,
    loop = TRUE
  ),
  
  tags$script(HTML("
    // Start muted to satisfy autoplay rules
    document.addEventListener('DOMContentLoaded', function() {
      var audio = document.getElementById('bgm');
      if (audio) {
        audio.muted = true;
        audio.volume = 0.15;
      }
    });

    // Unmute on first user interaction
    document.addEventListener('click', function() {
      var audio = document.getElementById('bgm');
      if (audio) {
        audio.muted = false;
      }
    }, { once: true });
  ")),  
  
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=IM+Fell+English+SC&display=swap",
      rel = "stylesheet"
    )
  ),      
  
  tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.6.0/dist/confetti.browser.min.js"),
  
  
 
  tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.6.0/dist/confetti.browser.min.js"),
  
  # Audio
  tags$audio(id = "treasure_sound", src = "win.wav", type = "audio/wav"),
  tags$audio(id = "level_complete_sound", src = "levelup.wav", type = "audio/wav"),
  
  tags$style("
    :root {
      --seaweed-green: #9bbf9b;
      --seaweed-dark: #6f8f6f;
      --seaweed-light: #b7d8b7;
    }
    
    body, html {
      height: 100%;
      margin: 0;
      padding: 0;
      font-family: 'IM Fell English SC', serif;
      font-size: 18px;
      background-color: var(--seaweed-green);
      color: #1a1a1a;
      overflow: hidden;
    }
    
    .card {
      background-color: var(--seaweed-dark);
      border: 1.5px solid #4f6f4f;
      box-shadow: 0 0 10px rgba(0,0,0,0.25);
      color: #1a1a1a;
    }
    
    .card-header {
      background-color: var(--seaweed-light);
      color: #1a1a1a;
      font-weight: bold;
      text-align: center;
      padding: 6px 10px;
    }
    
    .btn, .btn-primary, .btn-success {
      background-color: var(--seaweed-light) !important;
      border-color: #4f6f4f !important;
      color: #1a1a1a !important;
      padding: 4px 10px;
      font-size: 14px;
    }
    .btn:hover {
      background-color: #c5e3c5 !important;
    }
    
    #splash-overlay {
      position: fixed;
      inset: 0;
      z-index: 9998;
      display: flex;
      align-items: center;
      justify-content: center;
      background: rgba(80, 110, 80, 0.75);
      backdrop-filter: blur(4px);
      opacity: 1;
      transition: opacity 1.2s ease-out;
    }
    #splash-overlay.fade-out {
      opacity: 0;
      pointer-events: none;
    }
    
    #splash-content {
      text-align: center;
      color: #1a1a1a;
      padding: 20px 30px;
      border-radius: 16px;
      background: rgba(230, 240, 230, 0.95);
      box-shadow: 0 0 25px rgba(0,0,0,0.4);
      max-width: 600px;
    }
    #splash-title {
      font-size: 32px;
      margin-bottom: 8px;
    }
    #splash-subtitle {
      font-size: 16px;
      margin-bottom: 16px;
    }
    #press-key {
      font-size: 16px;
      color: #2f3f2f;
      animation: pulse 1.8s ease-in-out infinite;
    }
    @keyframes pulse {
      0% { opacity: 0.4; }
      50% { opacity: 1; }
      100% { opacity: 0.4; }
    }
    
    #treasure-chest {
      position: fixed;
      left: 50%;
      top: 50%;
      width: 0px;
      height: 0px;
      opacity: 0;
      transform: translate(-50%, -50%) scale(0.1);
      transition: all 1.2s ease-out;
      z-index: 9999;
      pointer-events: none;
    }
    #treasure-chest.show {
      width: 260px;
      height: 260px;
      opacity: 1;
      transform: translate(-50%, -50%) scale(1.1);
      filter: drop-shadow(0 0 25px gold);
    }
    #treasure-chest.fade-out {
      opacity: 0;
      transform: translate(-50%, -50%) scale(0.1);
      transition: all 1s ease-out;
    }
    
    #feedback-box {
      font-size: 14px;
      font-weight: bold;
      text-align: center;
      padding: 6px;
      border-radius: 8px;
      opacity: 0;
      transition: opacity 0.6s ease-in-out;
      margin-top: 6px;
      background-color: transparent;
    }
    #feedback-box.show {
      opacity: 1;
    }
    
    h1, h2, h3, h4, h5, h6,
    label, p, div, button, input, .btn {
      font-family: 'IM Fell English SC', serif !important;
    }
    
    h3, h4, h5, p {
      margin-top: 4px;
      margin-bottom: 4px;
    }
    
    #thankyou-overlay {
      position: fixed;
      inset: 0;
      z-index: 9997;
      display: none;
      align-items: center;
      justify-content: center;
      background: rgba(80, 110, 80, 0.9);
      color: #1a1a1a;
      text-align: center;
      padding: 20px;
    }
    
    #thankyou-content {
      max-width: 600px;
      background: rgba(235, 245, 235, 0.95);
      padding: 20px 30px;
      border-radius: 16px;
      box-shadow: 0 0 25px rgba(0,0,0,0.4);
      font-size: 20px;
    }
    
    #main-row {
      display: flex;
      gap: 16px;
      justify-content: center;
      align-items: flex-start;
      padding: 10px 16px 0 16px;
      box-sizing: border-box;
      height: calc(100vh - 70px);
    }
    
    #question-panel {
      width: 320px;
    }
    
    #map-panel {
      flex-grow: 1;
      max-width: 720px;
    }
    
    #treasure_map {
      max-height: 350px;
    }
  "),
  
  tags$img(id = "treasure-chest", src = "chestfinal.png"),
  
  tags$script("
    Shiny.addCustomMessageHandler('play-sound', function(message) {
      var audio = document.getElementById('treasure_sound');
      if (audio) { audio.play(); }
    });
    
    Shiny.addCustomMessageHandler('play-level-complete', function(message) {
      var audio = document.getElementById('level_complete_sound');
      if (audio) { audio.play(); }
    });
    
    Shiny.addCustomMessageHandler('confetti', function(message) {
      confetti({
        particleCount: 200,
        spread: 70,
        origin: { y: 0.6 }
      });
    });
    
    Shiny.addCustomMessageHandler('show-treasure', function(message) {
      var chest = document.getElementById('treasure-chest');
      if (chest) { 
        chest.classList.remove('fade-out');
        chest.classList.add('show'); 
      }
    });
    
    Shiny.addCustomMessageHandler('hide-treasure', function(message) {
      var chest = document.getElementById('treasure-chest');
      if (chest) { 
        chest.classList.remove('show');
        chest.classList.add('fade-out'); 
      }
    });
    
    Shiny.addCustomMessageHandler('show-feedback', function(type) {
      var box = document.getElementById('feedback-box');
      if (!box) return;
      
      if (type === 'correct') {
        box.style.backgroundColor = '#d4edda';
        box.style.color = '#155724';
        box.innerText = 'Correct! Great job!';
      } else {
        box.style.backgroundColor = '#f8d7da';
        box.style.color = '#721c24';
        box.innerText = 'Not quite right — try again!';
      }
      
      box.classList.add('show');
      setTimeout(function() {
        box.classList.remove('show');
      }, 1500);
    });
    
    Shiny.addCustomMessageHandler('fade-splash', function(message) {
      var overlay = document.getElementById('splash-overlay');
      if (!overlay) return;
      overlay.classList.add('fade-out');
      setTimeout(function() {
        overlay.style.display = 'none';
      }, 1200);
    });
    
    Shiny.addCustomMessageHandler('play-bg-music', function(message) {
      var bg = document.getElementById('bg_music');
      if (bg) { bg.play(); }
    });
    
    Shiny.addCustomMessageHandler('show-thankyou', function(message) {
      var ty = document.getElementById('thankyou-overlay');
      if (ty) { ty.style.display = 'flex'; }
    });
    
   window.addEventListener('keydown', function(e) {
  Shiny.setInputValue('key_press', Math.random(), {priority: 'event'});
});
  "),
  
  div(
    id = "splash-overlay",
    div(
      id = "splash-content",
      div(id = "splash-title", "🏴‍☠️ Statistics Treasure Hunt"),
      div(
        id = "splash-subtitle",
        "Answer questions correctly to guide your explorer across the map and uncover the treasure!"
      ),
      br(),
      div(id = "press-key", "Press any key to start")
    )
  ),
  
  div(
    id = "thankyou-overlay",
    div(
      id = "thankyou-content",
      div(style = "font-size: 26px; margin-bottom: 8px;", "⚓ Thank you for playing!"),
      div(
        "You’ve completed the Statistics Treasure Hunt. Refresh the page to play again."
      )
    )
  ),
  
  div(
    id = "app-content",
    br(),
    div(
      style = "text-align: center; margin-bottom: 4px;",
      h3(textOutput("level_header"))
    ),
    div(
      id = "main-row",
      
      div(
        id = "question-panel",
        card(
          card_body(
            conditionalPanel(
              condition = "output.show_question",
              h5(textOutput("question_number")),
              h5(textOutput("current_question")),
              radioButtons("answer", NULL, choices = list(), selected = character(0)),
              actionButton("submit", "Submit", class = "btn-primary"),
              div(id = "feedback-box")
            ),
            conditionalPanel(
              condition = "!output.show_question",
              div(
                style = "text-align: center;",
                h4("Welcome, Captain!"),
                p("Press any key on your keyboard to begin your voyage.")
              )
            )
          )
        )
      ),
      
      div(
        id = "map-panel",
        card(
          card_header("Treasure Map"),
          plotOutput("treasure_map", height = "350px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ------------------------------
  # Load pirate frames
  # ------------------------------
  pirate1 <- readPNG("www/pirate1final.png")
  pirate2 <- readPNG("www/pirate2final.png")
  pirate_frames <- list(pirate1, pirate2)
  
  # ------------------------------
  # Load ALL THREE MAPS
  # ------------------------------
  map_list <- list(
    level1 = rasterGrob(readPNG("www/map.png"),  width = unit(1, "npc"), height = unit(1, "npc")),
    level2 = rasterGrob(readPNG("www/map2.png"), width = unit(1, "npc"), height = unit(1, "npc")),
    level3 = rasterGrob(readPNG("www/map3.png"), width = unit(1, "npc"), height = unit(1, "npc"))
  )
  
  # ------------------------------
  # JS helper wrappers
  # ------------------------------
  play_treasure_sound   <- function() session$sendCustomMessage("play-sound", list())
  play_confetti         <- function() session$sendCustomMessage("confetti", list())
  show_treasure_chest   <- function() session$sendCustomMessage("show-treasure", list())
  hide_treasure_chest   <- function() session$sendCustomMessage("hide-treasure", list())
  play_bg_music         <- function() session$sendCustomMessage("play-bg-music", list())
  show_thankyou         <- function() session$sendCustomMessage("show-thankyou", list())
  play_level_complete   <- function() session$sendCustomMessage("play-level-complete", list())
  
  # ------------------------------
  # Reactive values
  # ------------------------------
  values <- reactiveValues(
    current_level = 0,
    current_question = 0,
    level_questions = list(),
    player_position = c(1, 1),
    path = data.frame(x = 1, y = 1),
    footprints = data.frame(x = numeric(0), y = numeric(0)),
    pirate_frame = 1,
    game_started = FALSE,
    animating = FALSE,
    anim_path = NULL,
    anim_index = 1,
    waiting_next_level = FALSE,
    final_screen = FALSE
  )
  
  # ------------------------------
  # Start game / next level / final screen
  # ------------------------------
  observeEvent(input$key_press, {
    
    # Start game from splash
    if (!values$game_started && !values$waiting_next_level && !values$final_screen) {
      session$sendCustomMessage("fade-splash", list())
      play_bg_music()
      
      values$current_level <- 1
      values$current_question <- 1
      values$level_questions <- questions$level1
      values$game_started <- TRUE
      values$player_position <- c(1, 1)
      values$footprints <- data.frame(x = numeric(0), y = numeric(0))
      
      updateRadioButtons(
        session, "answer",
        choices = setNames(values$level_questions[[1]]$options,
                           values$level_questions[[1]]$options),
        selected = character(0)
      )
      return()
    }
    
    # Move to next level
    if (isTRUE(values$waiting_next_level)) {
      values$waiting_next_level <- FALSE
      removeModal()
      
      if (values$current_level == 1) {
        values$current_level <- 2
        values$level_questions <- questions$level2
      } else if (values$current_level == 2) {
        values$current_level <- 3
        values$level_questions <- questions$level3
      }
      
      values$current_question <- 1
      values$player_position <- c(1, 1)
      values$footprints <- data.frame(x = numeric(0), y = numeric(0))
      
      updateRadioButtons(
        session, "answer",
        choices = setNames(values$level_questions[[1]]$options,
                           values$level_questions[[1]]$options),
        selected = character(0)
      )
      return()
    }
    
    # Final screen → thank you
    if (isTRUE(values$final_screen)) {
      values$final_screen <- FALSE
      removeModal()
      hide_treasure_chest()
      show_thankyou()
      return()
    }
  })
  
  # ------------------------------
  # UI text
  # ------------------------------
  output$show_question <- reactive(values$game_started)
  outputOptions(output, "show_question", suspendWhenHidden = FALSE)
  
  output$level_header <- renderText({
    if (!values$game_started) return("Ready to Start?")
    c(
      "🥈 Level 1: Silver Hunt",
      "🥇 Level 2: Golden Voyage",
      "🔥 Boss Level: The Captain’s Challenge"
    )[values$current_level]
  })
  
  output$question_number <- renderText({
    if (!values$game_started) return("")
    
    total <- length(values$level_questions)
    shown_q <- min(values$current_question, total)
    
    paste("Question", shown_q, "of", total)
  })
  
  
  output$current_question <- renderText({
    if (!values$game_started) return("")
    if (values$current_question > length(values$level_questions)) return("")
    values$level_questions[[values$current_question]]$question
  })
  
  # ------------------------------
  # Submit answer
  # ------------------------------
  observeEvent(input$submit, {
    if (!values$game_started) return()
    if (values$current_question > length(values$level_questions)) return()
    
    q <- values$level_questions[[values$current_question]]
    correct_answer <- q$options[q$correct]
    
    # Wrong answer
    if (is.null(input$answer) || input$answer != correct_answer) {
      session$sendCustomMessage("show-feedback", "incorrect")
      return()
    }
    
    # Correct answer
    session$sendCustomMessage("show-feedback", "correct")
    
    # Move pirate
    new_coord <- q$coordinate
    values$anim_path <- smooth_path(values$player_position, new_coord, steps = 18)
    values$anim_index <- 1
    values$animating <- TRUE
    
    values$current_question <- values$current_question + 1
    
    # Level complete
    if (values$current_question > length(values$level_questions)) {
      
      if (values$current_level == 1) {
        play_confetti()
        play_level_complete()
        show_level_complete(1)
        values$waiting_next_level <- TRUE
        return()
      }
      
      if (values$current_level == 2) {
        play_confetti()
        play_level_complete()
        show_level_complete(2)
        values$waiting_next_level <- TRUE
        return()
      }
      
      if (values$current_level == 3) {
        play_confetti()
        play_treasure_sound()
        show_treasure_chest()
        
        showModal(modalDialog(
          title = "🏆 Well Done, Captain!",
          div(
            style = "font-size: 20px; text-align: center; padding: 10px;",
            "You’ve found the treasure! Press any key to exit."
          ),
          easyClose = FALSE,
          footer = NULL
        ))
        
        values$final_screen <- TRUE
        values$animating <- FALSE
        return()
      }
    }
    
    # Load next question
    if (values$current_question <= length(values$level_questions)) {
      updateRadioButtons(
        session, "answer",
        choices = setNames(values$level_questions[[values$current_question]]$options,
                           values$level_questions[[values$current_question]]$options),
        selected = character(0)
      )
    }
  })
  
  # ------------------------------
  # Animation loop
  # ------------------------------
  observe({
    invalidateLater(25, session)
    
    if (!isTRUE(values$animating)) return()
    if (is.null(values$anim_path) || nrow(values$anim_path) == 0) {
      values$animating <- FALSE
      return()
    }
    
    total_steps <- nrow(values$anim_path)
    progress <- values$anim_index / total_steps
    eased <- progress^0.7
    i <- round(eased * total_steps)
    if (i < 1) i <- 1
    if (i > total_steps) {
      values$animating <- FALSE
      return()
    }
    
    base_x <- values$anim_path$x[i]
    base_y <- values$anim_path$y[i]
    float_offset <- sin(i / 3) * 0.3
    values$player_position <- c(base_x, base_y + float_offset)
    
    values$footprints <- rbind(
      tail(values$footprints, 40),
      data.frame(x = values$player_position[1], y = values$player_position[2])
    )
    
    values$pirate_frame <- ifelse(i %% 2 == 0, 1, 2)
    values$anim_index <- values$anim_index + 1
  })
  
  # ------------------------------
  # Render map
  # ------------------------------
  output$treasure_map <- renderPlot({
    plot(1, type = "n",
         xlim = c(0, 25), ylim = c(0, 25),
         xlab = "", ylab = "",
         axes = FALSE, main = "")
    
    
    current_map <- switch(
      as.character(values$current_level),
      "1" = map_list$level1,
      "2" = map_list$level2,
      "3" = map_list$level3
    )
    
    grid::grid.draw(current_map)
    
    # footprints
    if (nrow(values$footprints) > 0) {
      points(values$footprints$x, values$footprints$y,
             pch = 16, col = rgb(0, 0, 0, 0.4), cex = 0.7)
    }
    
    # pirate
    pirate_img <- pirate_frames[[values$pirate_frame]]
    w <- 0.6
    h <- 1.8
    
    rasterImage(
      pirate_img,
      values$player_position[1] - w,
      values$player_position[2] - h,
      values$player_position[1] + w,
      values$player_position[2] + h
    )
  })
}
shinyApp(ui = ui, server = server)



