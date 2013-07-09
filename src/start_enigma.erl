%%%-------------------------------------------------------------------------------------
%%% @author Ivan Iacono <ivan.iacono@gmail.com>, Corrado Santoro <santoro@dmi.unict.it>
%%% @copyright (C) 2013, Ivan Iacono, Corrado Santoro
%%% @doc This module includes the instructions to running and stopping the application.
%%% @end
%%%-------------------------------------------------------------------------------------

-module(start_enigma).

-export([start/0, stop/0]).

%% Runs the application.
start() ->
    application:start(enigma).

%% Stops the application.
stop() ->
    init:stop().
