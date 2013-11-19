% Abe Friesen
% 5171207
% j7p7
%
% ...
% 
% 


%% INCLUDE 'iolib.h'.

:- write('Clue Solver loaded.'),nl,nl.
:- write('Please type start. to begin the program and'),nl.
:- write('then follow the instructions at each prompt.'),nl.
:- write('Have Fun!'),nl,nl.


start :- 
	write('Please enter in the players playing in the correct order of play.'),nl,
	write('Each player should be seperated by a space. When all players have'),nl,
	write('been entered, press enter. DO NOT PUT A PERIOD AT THE END!'),nl,
	read_line_to_codes(user_input,String),
	atom_codes(C,String),
	atomic_list_concat(L, ' ', C),
	addPlayers(L),nl,
	cardPrompt.

cardPrompt :-
	write('Please enter the name of the player you are playing as.'),nl,
	read_line_to_codes(user_input,PlayerName),
	atom_codes(Code,PlayerName),nl,
	write('Please enter in the cards you have in your hand. Each card should be followed'),nl,
	write('by a space. When all cards have been entered, press enter. NO PERIOD AT END!'),nl,
	read_line_to_codes(user_input,String),
	atom_codes(C,String),
	atomic_list_concat(L, ' ', C),
	addOurCards(Code,L).

% Weapons
:- dynamic weapon/1.

weapon(knife).
weapon(candlestick).
weapon(revolver).
weapon(rope).
weapon(lead_pipe).
weapon(wrench).


% Locations
:- dynamic location/1.

location(kitchen).
location(ballroom).
location(conservatory).
location(billiard_room).
location(library).
location(study).
location(hall).
location(lounge).
location(dining_room).


% Suspects
:- dynamic suspect/1.

suspect(miss_scarlett).
suspect(mrs_white).
suspect(colonel_mustard).
suspect(reverend_green).
suspect(mrs_peacock).
suspect(professor_plum).


% Possible Locations
:- dynamic possibleLocation/1.


% Possible Weapons
:- dynamic possibleWeapon/1.


% Possible Suspects
:- dynamic possibleSuspect/1.


% Players currently playing
:- dynamic player/1.


% Used to store information about which player has which cards
:- dynamic hasCard/2.


% Used to store information about which player doesn't have which cards
:- dynamic notHaveCard/2.


% Used to determine potential cards players have
:- dynamic mightHaveCard/2.


% Used to determine who's turn it is next
:- dynamic whosTurn/2.


% Used to determine if an accusation is possible or list all possible
% permutations of accusations.
accusation(X,Y,Z) :- suspect(X),location(Y),weapon(Z).


% Wrapper function to initialize first player.
addPlayers([H | T]) :- addPlayersHelper(H,[H | T]).


% Used to setup the number of players and the order.
addPlayersHelper(FP,[P]) :- assert(player(P)),assert(whosTurn(P,FP)).
addPlayersHelper(FP,[H, N | T]) :-
	assert(player(H)),
	assert(whosTurn(H,N)),
	addPlayersHelper(FP,[N | T]).


% Used to show the contents of the knowledge base at any point in time.
showKB :- listing(weapon),listing(location),listing(suspect).


% Used to add information to the database when a player makes a suggestion.
% Not sure if this is actually helpful or not...
suggestion(X,Y,Z) :- 
	assert(possibleSuspect(X)),
	assert(possibleWeapon(Y)),
	assert(possibleLocation(Z)).


% Used to add a fact representing a card a player has
addCard(Player,Card) :-
	assert(hasCard(Player,Card)),
	removeFromKB(Card).


% Used to add the cards in our hand to the knowledge base
addOurCards(Player,[]) :- retract(suspect(Player)).
addOurCards(Player,[H | T]) :- 
	addCard(Player,H),
	addOurCards(Player,T).


% Used to remove a card from the knowledge base.
removeFromKB(Card) :- retract(location(Card)).
removeFromKB(Card) :- retract(weapon(Card)).
removeFromKB(Card) :- retract(suspect(Card)).


% Used to show all the cards a player has
showCards(Player,Card) :- hasCard(Player,Card),removeFromKB(Card).


% Used to determine when a player doesn't have a certain cards because
% they couldn't refute a suggestion from the player to their right
couldntRefute(Player,Suspect,Location,Weapon) :-
	assert(notHaveCard(Player,Suspect)),
	assert(notHaveCard(Player,Location)),
	assert(notHaveCard(Player,Weapon)),
	retract(mightHaveCard(Player,Suspect)),
	retract(mightHaveCard(Player,Location)),
	retract(mightHaveCard(Player,Weapon)).


% Used to add possible cards of a players hand. This occurs when
% a player on the left shows a card to a player on their right.
% Player variable refers to the player showing the card, while the
% 3 card variables are those made by the suggestion. So we know
% the player on the left has to have 1 of the 3 cards.
showedCard(Player,Suspect,Location,Weapon) :-
	assert(mightHaveCard(Player,Suspect)),
	assert(mightHaveCard(Player,Location)),
	assert(mightHaveCard(Player,Weapon)).


% Used to determine who's turn it is next
nextTurn(Player,X) :- whosTurn(Player,X).


% Used to remove cards from the knowledge base when another
% player makes an incorrect accusation.
wrongAccusation(Suspect,Location,Weapon) :-
	removeFromKB(Suspect),
	removeFromKB(Location),
	removeFromKB(Weapon).
