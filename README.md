Pokka poker server
==================

It's a lightweight and very beta poker server enviroment builded with Erlang/OTP.

TODO
----

 * Define the protocol
 * Do error handling
 * kill the test detached processes
 * Implement the game
    * ~~Join a table (with username)~~
    * ~~Start a game when at least 2 players joined~~
    * ~~Deal pocket cards~~
    * Create an order for the current game (who's turn is it?)
    * Determine blinds order
    * Implemt pot
    * Ask player for action (with possibilities)
    * Process player actions
     * BET
     * FOLD
     * RAISE
     * CHECK
    * Deal turn
    * Deal river
    * Determine when the game is over
    * Dertemine who has won the game
    * Implement players credit
