library(shiny)
library(shinyjqui)
library(shinyjs)
library(ggplot2)
library(dplyr)

# Avocado price csv
avocado <- read.csv("avocado.csv")
avg_region <- data.frame(avocado %>%
                           group_by(geography) %>%
                           summarize(avg = mean(average_price)))


# JS for centering draggables in their droppable
centerdrop <- JS('function(event, ui) {
                                            var $this = $(this);
                                            ui.draggable.position({
                                              my: "center",
                                              at: "center",
                                              of: $this,
                                              using: function(pos) {
                                                $(this).animate(pos, 200, "linear");
                                              }
                                            });
                                          }')

# Function to pop up a modal dialog with an iFrame
click_part <- function(part) {
  showModal(modalDialog(
    tags$iframe(src=paste("https://en.wikipedia.org/wiki/", part, sep=""), width="100%", height="100%"),
    footer=modalButton("Close"), size="l", easyClose=TRUE))
}

# Vector of bib entries for packages
packs <- c("ggplot2", "shiny", "shinyjqui", "shinyjs", "dplyr")
cites <- c(citation("ggplot2"), 
           citation("shiny"), 
           citation("shinyjqui"), 
           citation("shinyjs"),
           citation("dplyr"))

# Properly formats citation of a package
# Input: Package name as string
# Output: Citation as string
formatCites <- function(cit) {
  # Find the index of the given package and pull its bib entry
  ci <- cites[which(packs == cit)]
  # Pull the first and last names of all the authors
  authFirst <- sapply(strsplit(paste(ci$author), "\\s+"), "[[", 1)
  authLast <- sapply(strsplit(paste(ci$author), "\\s+"), "[[", 2)
  auths <- c()
  x <- 1
  # Append all authors to auth vector in the format LastName FirstInitial.
  repeat{
    auths <- append(auths, paste(authLast[x], " ",
                                 substring(authFirst[x], 1, 1), ".", sep=""))
    x <- x + 1
    if(x > length(authFirst)) break
  }
  # Collapse the auth vector to a single string
  auths <- paste0(auths, collapse=", ")
  # Paste together the bib chunks in the correct order
  paste(auths, " (", ci$year, "). ", em(paste(ci$title)), ". R package version ", packageVersion(cit), ", ", a(href=ci$url, ci$url, .noWS="after"), "."
        ,sep="")
}

# Dropboxes, some are initially hidden
# Dropboxes are outlined rectangles where parts can be dragged to
mbdrop <- jqui_droppable(div(id="mbdrop", p(""), class="dropspot", 
                             style="width:52%; height:65%; top:5%; left:46%; z-index:2;"), 
                         options = list(`accept` = "#mboard", `drop` = centerdrop))
ramdrop <- jqui_droppable(hidden(div(id="ramdrop", p(""), class="dropspot", 
                                     style="width:8.5%; height:31%; top:7.5%; left:81%; z-index:8;")),
                          options = list(`accept` = "#ram", `drop` = centerdrop))
psudrop <- jqui_droppable(div(id="psudrop", p(""), class="dropspot", 
                              style="width:35%; height:32%; top:65%; left:5%; z-index:4;"),
                          options = list(`accept` = "#psu", `drop` = centerdrop))
gpudrop <- jqui_droppable(hidden(div(id="gpudrop", p(""), class="dropspot", 
                                     style="width:22%; height:30%; top:37%; left:51%; z-index:6;")),
                          options = list(`accept` = "#gpu", `drop` = centerdrop))
cpudrop <- jqui_droppable(hidden(div(id="cpudrop", p(""), class="dropspot", 
                                     style="width:13.2%; height:14.4%; top:13.7%; left:62%; z-index:8;")),
                          options = list(`accept` = "#cpu", `drop` = centerdrop))
# Display the image of the empty pc case
caseimg <- img(id="case", src="casecrop.jpg", width="100%", style="z-index:1; position:relative; margin-right:5px")

ui <- fluidPage(
  tags$style(
    " 
    * {font-family: 'Lato';
        font-size: 15px; }
    "
  ),
  navbarPage(
    "Build a PC",
    
    # Tab for interactive PC building
    tabPanel("App",
    # Styling in CSS for various objects
             tags$style("
  .dropspot {
      border-style:solid;
      border-color:red;
      border-width:5px;
      margin-width:10px;
      position:absolute;
      padding-width:20px;
   }
  figure {
      display:inline-block;
      align:center;
      position:relative;
      border-style:solid;
      border-width:1px;
      border-color:black;
      padding-right:1px;
      padding-left:1px;
      margin-top:3px;
      margin-bottom:3px;
      margin-right:3px;
      margin-left:3px;
  }
  /* Makes figure captions look like hyperlinks */
  figcaption {
      text-align:center;
      text-decoration:underline;
      color:blue;
      cursor:pointer;
  }
  .aboutcap {
      text-decoration:none;
      color:black;
      cursor:default;
  
  } /* modal settings are for the popups on figcaption click */
  .modal-dialog, .modal-content {
      height: 98%;
  }

  .modal-body {
      height:88vh;
      overflow-y: scroll;
  }
  
  .app-inst {
      display:inline-block; 
      width:50%;
      text-align:center;
      margin-bottom:30px;
      font-size:20px;
      margin-left:10px;
  }"),
  # Loading in shinyjs for showing/hiding dropboxes
  useShinyjs(),
  
  # Case image and dropboxes
  column(6,
         uiOutput('dropboxes')),
  
  # Printing the parts to the right of the chassis
  column(6,
         uiOutput("figures"))),
  
  # Tab for information about the project
  tabPanel("About",
           column(4, h2("About this project"), hr(), 
                  p("Ever wanted to learn how to build a PC but were stopped for various reasons (financial, space, fear)? Now these things can't stop you!", span("*", style="color:red;", .noWS="before"), "I built this app to support those who are new to computers and may not be comfortable with the basics, such as 'what part goes where?' as well as 'what does this part even ", em("do?'")), 
                  p("To use the app, just drag and drop a part from the right side of the screen onto the red highlighted area on the computer that you think that part belongs in. If the part belongs there, the outline will disappear and the part will snap into place. If that is not the correct spot, the part will return itself back to the list"),
                  p("You can also click on the name of any part in the list or already placed on the computer to bring up the Wikipedia page containing more details."),
                  p("Getting more comfortable with the general layout and parts of a computer is the first step to building your own - and learning is always more fun through games!"),
                  p("*Can't stop you from learning", em("how,", style="font-size:10px; color:red;"), "that is. I unfortunately cannot remove these blocks from you actually building one.",
                      style="font-size:10px; color:red;"),
                  offset = 1),
           
           column(7,
                  tags$figure(img(src="pc1.jpg", alt="My PC.", width="100%"), 
                              tags$figcaption("My PC", class="aboutcap"), style="width:49%;"),
                  tags$figure(img(src="pc2.jpg", alt="My PC, lit up.", width="100%"), 
                              tags$figcaption("My PC, lit up", class="aboutcap"), style="width:49%; float:right;"))),
  
  # Tab for design process
  tabPanel("Design Process", 
           column(5,
                  h2("The Design Process"),
                  h4("How class resources influenced the design of the app"),
                  hr(),
                  p("The initial idea for creating this app was to merge two very different apps I have seen in my life - PC Building Simulator, and interactive science concept labs. I have distinct memories of visiting these web apps during my high school science classes to learn about physics, electricity, and more. Having a 'hands on' way of interacting with the topic allowed me to experiment and figure things out while also having fun (as much fun one can have in a high school science class!)."),
                  p("Since this project", em("was"), "created for my college course's half semester assignment, we also had activities in class to aid us in the design process. These included a wireframe and screen mockup (see right) as well as discussing who our audience is, what they would get out of using the app, etc."),
                  p("The book ", a(href="https://essenceofsoftware.com", "The Essence of Software"), "was a good resource for the design and development processes from here on out. Much of the focus was on concepts and their connections between each other."),
                  p("The main concept that the app focuses on is 'matching.' The higher order concept of this is 'dragging' and 'dropping.' These concepts are practiced at a young age with toys like ", a(href="https://www.amazon.com/Colorful-Preschool-Educational-Matching-Recognition/dp/B0895SGPBN", "shape puzzles."), "Another concept employed in the app is that of tabs. If you made it to this text though... congrats! You understand the concept of tabs - something widely used across internet. The final (conciously) employed concept is clickable links. There are links on this very tab, in the citations tab, as well as the links in the main part of the app popping up an info panel when clicked."),
                  p("The overall design of app didn't end up shifting much over the implementation phase of it. The only thing to really change was the introduction of labels on the PC parts, which was feedback I got from a peer who tested the app for me. The labels ended up being a great thing to add since image tags in HTML are fine with hyperlinks, but don't play nice with trying to run a command on click. This also improves visual cues for the user, since the blue underlined text is a common sign of the link concept being at play - whereas the image being clickable is a bit harder to convey."),
                  p("Aside from the main interactible feature of the app, some of the tabs changed dramatically. The original intention for the plots page was to create graphs displaying the trend in computer hardware brands over time, using the", a(href="https://store.steampowered.com/hwsurvey/Steam-Hardware-Software-Survey-Welcome-to-Steam", "Steam Survey."), "However, this quickly became difficult data to collect without a direct CSV of the results; as well as the fact that Steam only pubishes the overview and not the full dataset. The resulting avocado graphs are a fun way to visualize price differences in various locations, while also fulfilling the project requirement of having", code("ggplot2"), "graphs."),
                  p("Overall, I am proud of the application that was produced. I learned a lot about HTML, CSS, JavaScript, and R along the way, while also producing a functioning app that does not break copyright to have published online (looking at you, Pokemon Battle Simulator project in C!)"),
                  offset = 1),
           
           column(6,
                  div(
                    tags$iframe(src="Wireframe.pdf", width="60%", height="400px", style="display:inline-block;", alt="An outline of how the app will function."),
                    img(src="1.png", width="60%", alt="A mockup of an open pc case and a list of pc parts."),
                    img(src="2.png", width="60%", alt="A mockup of the pop-up feature when clicking on a pc part."),
                    img(src="3.png", width="60%", alt="A mockup of a page with detailed information on a pc part."), style="margin-left:50px;"))),
  
  # Tab for avocados
  tabPanel("Plots", 
           column(3,
                  h2("Why can't I afford to build a pc for real?"),
                  p("Because you're eating too much organic avocado toast. The graph below shows the average price for avocados over 4 years. Cut out the little green guy, eat a few chemicals (what could it hurt??), and you'll be able to afford a top-tier PC in no time.", "In the graph to the right, you'll see whether you need to move or not to afford your avocado addiction."),
                  span("Note: The above is all sarcasm. A requirement of the project was to include graphs based on a dataset.", style="color:red; font-size:10px;"),
                  plotOutput("plot1"),
                  offset = 1),
           column(8,
                  plotOutput("plot2"))),
  
  
  # Tab for citations
  tabPanel("Acknowledgements", 
           tags$style( # Set lists to use bullets
             "ol { list-style:disc; } 
                ul { list-style:disc; }"),
           column(7,
                  h2("Citations and Acknowledgements"),
                  hr(),
                  p("Many thanks also go out to those on", a(href="https://stackoverflow.com/", "StackOverflow"), "who were brave enough to ask questions, and those nice enough to answer them comprehensibly."),
                  h3("Photo Credits"),
                  p("All images either taken by me or pulled from the public domain through", 
                    a(href="https://openclipart.org/", "OpenClipArt,"), 
                    a(href="https://pixabay.com/", "PixaBay,"), "or",
                    a(href="https://commons.wikimedia.org/wiki/Main_Page",
                      "Wikimedia Commons.")),
                  h3("Avocado Dataset"),
                  tags$ol(
                    tags$li(p("Kornev, Timofei. (2021, February). Avocado Prices (2020), Version 2. Retrieved March 29, 2023 from ", a(href="https://www.kaggle.com/datasets/timmate/avocado-prices-2020", "https://www.kaggle.com/datasets/timmate/avocado-prices-2020", .noWS="after"), ".")),
                    tags$li(p("\"Category Data: Totals by PLU.\"", em("Hass Avocado Board", .noWS="after"), ", 2023,", a(href="https://hassavocadoboard.com/category-data/", "https://hassavocadoboard.com/category-data/"), "Retrieved March 29, 2023."))),
                  p("The noted dataset was updated with data through early 2023 by me, using the HAB's published data."),
                  h3("Packages"),
                  tags$ol( 
                    tags$li(HTML(formatCites("shinyjs"))),
                    tags$li(HTML(formatCites("shinyjqui"))),
                    tags$li(HTML(formatCites("ggplot2"))),
                    tags$li(HTML(formatCites("shiny"))),
                    tags$li(HTML(formatCites("dplyr")))),
                  h3("Additional Resources"),
                  tags$ul(
                    tags$li("My peers, class mentor, and professor"),
                    tags$li("Jackson, Daniel.", em("The Essence of Software: Why Concepts Matter for Great Design."), "Princeton University Press, 2021."),
                    tags$li(p("Moran, Tom.", em("System Design."), "Dimensions of Evaluation for User-System Performance,", a(href="http://www.chilton-computing.org.uk/acd/literature/books/mi/p04.htm#c4p4", "http://www.chilton-computing.org.uk/acd/literature/books/mi/p04.htm#c4p4")))
                  ),
                  offset = 1))
  
  )
  
)


server <- function(input, output, session) {
  # Images of PC parts
  mboard <- div(img(id="mboard", src="motherboard.png", width="100%", style="z-index:3", title="Motherboard", alt="A motherboard for a PC."), style="display:inline-block;")
  psu <- div(img(id="psu", src="psu.png", width="100%", style="z-index:5", title="Power Supply Unit (PSU)", alt="A computer power supply."), style="display:inline-block;")
  gpu <- div(img(id="gpu", src="gpu.png", width="100%", style="z-index:7", title="Graphics Processing Unit (GPU)", alt="A Graphics Processing Unit (GPU)."), style="display:inline-block;")
  ram <- div(img(id="ram", src="ram.png", width="80%", style="z-index:9", title="Random Access Memory (RAM)", alt="A stick of RAM."), style="display:inline-block")
  cpu <- div(img(id="cpu", src="cpu.png", width="100%", style="z-index:10", title="Central Processing Unit (CPU)", alt="A computer CPU."), style="display:inline-block;")
  
  # Outputting PC parts as figures with captions
  output$figures <- renderUI({
    div(
      tags$figure(style="width:45%; float:left;", tags$figcaption(id = "mblink", "Motherboard"), mboard),
      div(class="app-inst", "Drag and drop any pc part to a", span("red outlined box", style="color:red; font-size:20px;"), "on the left. The part will stick if that is where it belongs!"),
      tags$figure(style="width:8%; padding-left:7px", tags$figcaption(id="ramlink", "RAM"), ram),
      tags$figure(style="width:43%;", tags$figcaption(id="psulink", "Power Supply"), psu),
      tags$figure(style="width:11%;", tags$figcaption(id="cpulink", "CPU"), cpu),
      tags$figure(style="width:28%;", tags$figcaption(id="gpulink", "GPU"), gpu),
      
      # Was having issues with the app loading images without the draggable tag on them
      # and refreshing seemed to fix the issue, so added this
      div("If the parts aren't draggable, press", br(), "this button to refresh the page: ", style="color:red; font-size:15px;", br()),
      actionButton("refresh", "Refresh"))
  })
  
  # Make the images draggable
  jqui_draggable(ui = "#ram,#mboard,#cpu,#gpu,#psu",
                 # Return the part to the list if dropped on a non-accepting droppable
                 options = list(`revert` = "invalid"))
  
  # Refresh button; Build a PC tab
  observeEvent(input$refresh, {
    session$reload()
  })
  
  # Event handlers for when a draggable is dropped in an accepting droppable
  observeEvent(input$mbdrop_drop, {
    jqui_draggable("#mboard", operation = "disable") # Disable dragging once dropped
    # Hide motherboard dropbox, show dropboxes on the motherboard
    shinyjs::hide(id = "mbdrop")
    shinyjs::show(id = "ramdrop")
    shinyjs::show(id = "cpudrop")
    shinyjs::show(id = "gpudrop")
  })
  observeEvent(input$ramdrop_drop, {
    jqui_draggable("#ram", operation = "disable")
    shinyjs::hide(id = "ramdrop")
  })
  observeEvent(input$psudrop_drop, {
    jqui_draggable("#psu", operation = "disable")
    shinyjs::hide(id = "psudrop")
  })
  observeEvent(input$gpudrop_drop, {
    jqui_draggable("#gpu", operation = "disable")
    shinyjs::hide(id = "gpudrop")
  })
  observeEvent(input$cpudrop_drop, {
    jqui_draggable("#cpu", operation = "disable")
    shinyjs::hide(id = "cpudrop")
  })
  
  # Render the case image and dropboxes relative to it
  output$dropboxes <- renderUI({
    div(caseimg, mbdrop, ramdrop, psudrop, gpudrop, cpudrop, style="position:absolute;
        top:0px; left:100px;")
  })
  
  # Run click_part if a figure caption is clicked
  onclick("mblink", click_part("Motherboard"))
  onclick("ramlink", click_part("Random-access_memory"))
  onclick("psulink", click_part("power_supply_unit_(computer)"))
  onclick("cpulink", click_part("central_processing_unit"))
  onclick("gpulink", click_part("graphics_processing_unit"))
  
  # Output organic vs conventional avocado price graph
  output$plot1 <- renderPlot({
    ggplot(avocado, aes(year, average_price, fill=type, width=.8)) + 
      # side by side bars using "dodge", change from count to mean
      geom_bar(position="dodge", stat="summary", fun="mean") +
      # zoom in on the frame where price is between 1 and 1.8
      coord_cartesian(ylim=c(1, 1.8)) +
      scale_fill_manual(values=c("#1A5276", "#F5B041")) + 
      xlab("") +
      ylab("") +
      ggtitle("Average Price of an Avocado\n(in U.S. dollars)") +
      theme(plot.title=element_text(hjust=0.5), # title size
            legend.position=c(.5,.9), # place legend center top
            legend.key.size=unit(2, "mm"), # legend size
            legend.box.margin = margin(5, 20, 5, 20))
  }, width=390, res=90) # size of output
  
  # Geography based avocado price bar graph
  output$plot2 <- renderPlot({
    # Use reorder to order geography based on price
    ggplot(avg_region, aes(reorder(geography, avg), avg, width=.75, fill=avg)) +
      geom_col(show.legend=FALSE) +
      coord_flip(ylim=c(1,1.8)) + # flip to horizontal plot & zoom view
      # Set color of bar based on x axis (now price)
      scale_fill_gradient2(low="#2ECC71", mid="#F1C40F", high="#E74C3C", midpoint=1.4) +
      xlab("") +
      ylab("")
  }, height=680, res=100)
}

# Run the application 
shinyApp(ui = ui, server = server)
