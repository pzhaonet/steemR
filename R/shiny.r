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

  ### Get the follwer of the given ID
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
