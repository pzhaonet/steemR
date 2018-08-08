#' UI for the Shiny app `sfollow()`
#'
#' @return A UI function
sfollow_ui <- function(){
  fluidPage(
    ### header
    wellPanel(
      h4("Type an ID and clock 'Go':"),
      textInput('fr_id', label = '', value = 'dapeng'),
      actionButton('fr_go', 'Go!')
    ),

    ### follower vs following
    wellPanel(
      h4("Results"),
	  # wordclouds for the followers
          plotOutput('fr_plot'),
          p(),
          fluidRow(
            column(4, sliderInput("fr_slider_followers",
                                  "Followers Top:",
                                  min = 1,
                                  max = 100,
                                  value = c(1, 30)
            )),
            column(4, sliderInput("fr_slider_posts",
                                  "Posts Top:",
                                  min = 1,
                                  max = 100,
                                  value = c(1, 30)
            )),
            column(4, sliderInput("fr_slider_vests",
                                  "Vests Top:",
                                  min = 1,
                                  max = 100,
                                  value = c(1, 30)
            ))
          ),
          hr(),
		  # Ven diagram for the followers and followings
          wellPanel(
            plotOutput('fr_venplot')
          ),
	  
      hr(),
      strong("Following but not followers"),
      uiOutput('fr_follower'),
      p(),
      hr(),
      p(),
      strong("Follower but not following"),
      uiOutput('fr_following')
    ),

    ### follower report
    wellPanel(
      h4("Data of his/her followers"),
      dataTableOutput('fr_dtfer')
    ),

    ### following report
    wellPanel(
      h4("Data of his/her following"),
      dataTableOutput('fr_dtfing')
    )
  )
}


#' Server for the Shiny app sfollow display and analysis
#'
#' @param input The input of the server.
#' @param output The output of the server.
#' @param session The session of the server.
#' @return A server function
sfollow_server <- function(input, output, session) {
  ### input the ID
  myid <- eventReactive(input$fr_go, {
    ifelse(substr(input$fr_id, 1, 1) == '@',
           gsub('@', '', input$fr_id),
           input$fr_id)
  })

  ### Get the followers of the given ID
  fer <- eventReactive(input$fr_go,
                       {gfollower(id = myid())}
  )

  ### Get the following of the given ID
  fing <- eventReactive(input$fr_go,
                        {
                          fing <- gfollowing(id = myid())
                          fing$Action <- ifelse(fing$What == 'Follow', 'follow', 'un')
                          fing
                        }
  )

  ### Get the list of follower vs following
  mylist <- eventReactive(input$fr_go, {
    myferfing <- gfollow(id = myid())
    myfer <- unique(myferfing$followers)
    myfing <- unique(myferfing$following)
    finginfer <- myfing %in% myfer
    ferinfing <- myfer %in% myfing
    mylist <- list(not_follower = myfing[!finginfer][order(myfing[!finginfer])],
                   not_following = myfer[!ferinfing][order(myfer[!ferinfing])],
                   followers = myfer,
                   following = myfing)
    mylist
  })

  ### create the html code for the report of follower vs following
  output$fr_follower <- renderUI({
    HTML(idlink(mylist()[[1]]))
  })

  output$fr_following <- renderUI({
    HTML(idlink(mylist()[[2]]))
  })

  ### create the slide bars for the wordclouds
    observe({
    slmax <- nrow(fer())
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session,
                      "fr_slider_followers",
                      value = c(1, ifelse(slmax > 36, 36, slmax)),
                      min = 1,
                      max = slmax,
                      step = 1)
    updateSliderInput(session,
                      "fr_slider_posts",
                      value = c(1, ifelse(slmax > 36, 36, slmax)),
                      min = 1,
                      max = slmax,
                      step = 1)
    updateSliderInput(session,
                      "fr_slider_vests",
                      value = c(1, ifelse(slmax > 36, 36, slmax)),
                      min = 1,
                      max = slmax,
                      step = 1)
  })

  ### create the Venn diagram for the followers and followings
  output$fr_venplot <- renderPlot({
    VennDiagram::draw.pairwise.venn(area1 = length(mylist()$followers),
                                    area2 = length(mylist()$following),
                                    cross.area = sum(mylist()$following %in% mylist()$followers),
                                    category = c("Followers", "Following"),
                                    fill = c("red", "blue"),
                                    alpha=c(0.2,0.2),
                                    ext.text = TRUE,
                                    ext.percent = c(0.1,0.1,0.1),
                                    ext.length = 0.6,
                                    label.col = rep("gray10",3),
                                    lwd = 0,
                                    cex = 2,
                                    fontface = rep("bold",3),
                                    fontfamily = rep("sans",3),
                                    cat.cex = 1.5,
                                    cat.fontface = rep("plain",2),
                                    cat.fontfamily = rep("sans",2),
                                    cat.pos = c(0, 0),
                                    print.mode = c("percent","raw")
    )
  })
  
    ### create the wordclouds for the followers
  output$fr_plot <- renderPlot({
    if (nrow(fer()) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, 'No data for wordclouds!', col = 'red', cex = 2)
    } else {
      datatime =  format(Sys.time(), format = '%Y-%m-%d %H:%M:%S %Z')
      par(mar = c(0,0,0,0))
      layout(mat = matrix(
        c(1, 1, 1, 2, 2, 2, 3, 4, 5, 6, 6, 6),
        nrow = 4,
        byrow = TRUE
        ),
        heights = c(2, 2, 20, 2))
      plot(1,
           type = "n",
           axes = FALSE,
           xlab = "",
           ylab = "")
      text(1, 1,
           paste0('@', myid(), "'s followers"),
           col = 'steelblue',
           cex = 3)
      plot(1,
           type = "n",
           axes = FALSE,
           xlab = "",
           ylab = "")
      text(1, 1, datatime, cex = 2)
      freq <- 'vests'
      myfer <- fer()
      slr <- input$fr_slider_followers

      colplot <- c('Followers', 'Posts','vests')
      sliderplot <- list(input$fr_slider_followers,
                         input$fr_slider_posts,
                         input$fr_slider_vests)
      for (i in 1:3){
        wcplot <- myfer[rev(order(myfer[, colplot[i]])), ][sliderplot[[i]][1]: sliderplot[[i]][2], ]
        wordcloud::wordcloud(wcplot$Account,
                             freq = wcplot[, colplot[i]],
                             colors = RColorBrewer::l(4, "Dark2"),
                             scale = c(4, 0.6))
        legend('top',
               legend = paste0(colplot[i],
                               ' Top ',
                               sliderplot[[i]][1],
                               '--',
                               sliderplot[[i]][2]),
               bty = 'n',
               cex = 2)
      }
      plot(1,
           type = "n",
           axes = FALSE,
           xlab = "",
           ylab = "")
      text(1, 1,
           'made by steemr',
           col = 'steelblue',
           cex = 2)
    }
  })
  
  ### Create the table of the followers
  output$fr_dtfer = renderDataTable({
    if (nrow(fer()) == 0) {
      data.frame(Account = NA,
                 Followers = NA,
                 Posts = NA,
                 vests = NA)
    } else {
      fer()[, c('Account', 'Followers', 'Posts', 'vests')]
    }
  },
  options = list(lengthMenu = c(10, 30, 50, 100),
                 pageLength = 10))

  ### Create the table of the following
  output$fr_dtfing = renderDataTable({
    fing()[, c(1, 2, 5)]
  },
  options = list(lengthMenu = c(10, 30, 50, 100, 1000),
                 pageLength = 10)
  )

}


#' A shiny app to display and anaylize the followers a given ID.
#' sfollow means shiny app for followers.
#'
#' @return a shinyapp which can be displayed in a web browser.
#' @export
#' @example sfollow()
sfollow <- function(){
  return(shiny::shinyApp(ui = sfollow_ui, server = sfollow_server))
}
