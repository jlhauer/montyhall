#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Contestant Selects a Door.
#' 
#' @description
#' `select_door()` selects a door for the player's first guess.
#' 
#' @details
#' The function creates a vector of three elements- door1, 
#' door2, and door3 and uses sample () to select one random 
#' door and store this door in the variable 'a.pick'.
#' 
#' @param... no arguments are used by the function.
#' 
#' @return One of the three elements (a door) is randomly returned as 
#' the output.
#' 
#' @examples
#' select_door()
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host Opens Goat Door.
#' 
#' @description
#' The host reveals (opens) a new door with a goat behind it to the contestant. 
#' 
#' @details
#' The host must reveal a door that is not a car and not a current contestant 
#' selection.if the contestant selects the car on the first guess the host can 
#' open either door, but if the contestant selects a goat the host only has one 
#' option.
#' 
#' @param game A vector of length 3 representing the contents of the doors
#' (either "car" or "goat").
#' @param a.pick An integer representing the door that the contestant has 
#' selected (a value between 1 and 3).
#' 
#' @return This function returns an element (door number) as the output.
#' 
#' @examples
#' open_goat_door()
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Contestant is Given the Option to Change Door Selection.
#' 
#' @description
#' The contestant makes a final pick on their door by either
#' staying with their original door selection or switching.
#' 
#' @details
#' The contestant can choose to change from their initial selection to the other 
#' door that is still closed. The function will represent 
#' the game-playing strategy as the argument stay=TRUE or stay=FALSE.
#' 
#' @param stay A logical value indicating whether the contestant wants to stay 
#' with their original door selection (TRUE) or switch to another door (FALSE). 
#' The default value is TRUE.
#' @param opened.door An integer representing the door that was opened to reveal 
#' a goat (a value between 1 and 3).
#' @param a.pick An integer representing the door that the contestant has 
#' selected (a value between 1 and 3).

#' @return The final selected door (a door number between 1-3) is returned.
#' 
#' @examples
#' change_door()
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine Winner or Loser.
#' 
#' @description
#' This function reveals if the contestant has correctly selected the door with 
#' the car behind it, winning the game. Or if the contestant looses.
#' 
#' @details
#' For this function, in order to determine if the player has won we use 
#' information about the door they have selected and the game set-up 
#' to evaluate if they win the car (win) or the goat (lose).
#' 
#' @param final.pick An integer representing the door that the 
#' contestant has finally selected (a value between 1 and 3).
#' @param game A vector of length 3 representing the contents of the doors 
#' (either "car" or "goat").
#' 
#' @return The results of the game, Win or Lose, is returned.
#' 
#' @examples
#' determine_winner()
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play a Simulation of a Single, Entire Game.
#' 
#' @description
#' Package the game functions together into a single play_game() function which 
#' executes each step of a single game in order.
#' 
#' @details
#' We can use the play_game () to run a full simulation to examine the 
#' effectiveness of both strategies Stay and switch) by playing the game over 
#' and over and looking at which strategy yields higher returns.
#' 
#' @param ... no arguments are used by the simulation.
#' 
#' @return This simulation will return the results of a single game, Win or Lose.
#' 
#' @examples
#' play_game()
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Monty Hall Simulation
#' 
#' @description
#' A function that simulates 'n' number of Monty Hall game plays and 
#' returns the outcome of each game. 
#' 
#' @details
#' We use the play_game function to simulate one game, and use the dplyr library 
#' to bind the outcome of each game into a single data frame. 
#' The function then creates outcome proportions and a results data frame.
#' 
#' @param ... no arguments are used by the simulation.
#' 
#' @return a table of the outcome proportions and 
#' returns the results data frame.
#' 
#' @examples
#' play_n_games()
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
