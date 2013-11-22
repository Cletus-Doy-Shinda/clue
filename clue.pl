% CLUE
% ------------------
% Abe Friesen
% 5171207
% j7p7
%
% Erik Shadwick
% 40112104
% z3u7


% Message to alert user that the program as loaded to instructions
% for the first thing they should do.
:- write('Clue Solver loaded.'),nl,nl.
:- write('Please type start. to begin the program and'),nl.
:- write('then follow the instructions at each prompt.'),nl.
:- write('Have Fun!'),nl,nl.


% Entry point for the program. Will handle setting up players
% and the turn order
start :- 
	write('Please enter in the players playing in the correct order of play.'),nl,
	write('Each player should be seperated by a space. When all players have'),nl,
	write('been entered, press enter. DO NOT PUT A PERIOD AT THE END!'),nl,
	read_line_to_codes(user_input,String),
	atom_codes(C,String),
	atomic_list_concat(L, ' ', C),
	addPlayers(L),nl,
	cardPrompt.

% Used by: start
% Prompts the user to enter in the player they are playing as and asks which
% cards they hold in their hand.
cardPrompt :-
	write('Please enter the name of the player you are playing as.'),nl,
	read_line_to_codes(user_input,PlayerName),
	atom_codes(Code,PlayerName),nl,
	write('Please enter in the cards you have in your hand. Each card should be followed'),nl,
	write('by a space. When all cards have been entered, press enter. NO PERIOD AT END!'),nl,
	read_line_to_codes(user_input,String),
	atom_codes(C,String),
	atomic_list_concat(L, ' ', C),
	assert(ourPlayer(Code)),
	addOurCards(Code,L),
        beginLooper.


% Facts about who and where we are.
:- dynamic ourPlayer/1.
:- dynamic ourLocation/1.


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
accusation([X,Y,Z]) :- suspect(X),location(Y),weapon(Z).


% Wrapper function to initialize first player so we
% can link the last player back to the first player.
addPlayers([H | T]) :- addPlayersHelper(H,[H | T]).


% Used to setup the number of players and the order.
addPlayersHelper(FP,[P]) :- assert(player(P)),assert(whosTurn(P,FP)).
addPlayersHelper(FP,[H, N | T]) :-
	assert(player(H)),
	assert(whosTurn(H,N)),
	addPlayersHelper(FP,[N | T]).


% Used to show the contents of the knowledge base. Will show
% the remaining weapons, locations and suspects.
showKB :- listing(weapon),listing(location),listing(suspect).


% Ask for a possible accusation (ie suggestion) involving some person, location,
% and weapon which are not yet known about.
suggestion([P, L, W]) :-
    findall(Acc, accusation(Acc), Suggs),
    find(Suggs, [P, L, W]).

% Used by: suggestion
% Find the first occurrance in a list of suggestions. This is used to match an
% accusation to the form of a suggestion we want to make.
find([X|_T], X) :- !.
find([_X|T], Y) :- find(T, Y).


% Used to add a fact representing a card a player has. This is Used
% when another player shows us a card.
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


% Used to show all the cards a player has that we know of.
showCards(Player,Card) :- hasCard(Player,Card).


% Used to determine when a player doesn't have a certain cards because they
% couldn't refute a suggestion from the player to their right.
couldntRefute(Player,Suspect,Location,Weapon) :-
    assert(notHaveCard(Player,Suspect)),
    assert(notHaveCard(Player,Location)),
    assert(notHaveCard(Player,Weapon)),
    forceRetract(mightHaveCard(Player,Suspect)),
    forceRetract(mightHaveCard(Player,Location)),
    forceRetract(mightHaveCard(Player,Weapon)).


% Retract a fact, and regardless of if that fact existed prior to this, evaluate
% to true. Retracting a non-existant fact normally results in a false.
forceRetract(Clause) :- retract(Clause) ; true.


% Used to add possible cards of a players hand. This occurs when
% a player on the left shows a card to a player on their right.
% Player variable refers to the player showing the card, while the
% 3 card variables are those made by the suggestion. So we know
% the player on the left has to have 1 of the 3 cards.
showedCard(Player,Suspect,Location,Weapon) :-
    assert(mightHaveCard(Player,Suspect)),
    assert(mightHaveCard(Player,Location)),
    assert(mightHaveCard(Player,Weapon)).


% Used to determine who's turn it is next.
nextTurn(Player,X) :- whosTurn(Player,X).


% Kick off the main I/O loop.
beginLooper :-
    findall(X, player(X), Players),
    last(Players, Last),
    looper(Last).


% The main I/O loop. This cycles through each player's turn, properly allowing
% information to be added as we go. This loops until an accusation can be made.
looper(PrevPlayer) :-
    nl, nl,
    nextTurn(PrevPlayer, CurPlayer),
    (  ourPlayer(CurPlayer)
    -> ourTurn
    ;  opponentTurn(CurPlayer)
    ), !,
    looper(CurPlayer).


% Handle I/O for our own turn. This is a fairly complex process, which can
% update our location, then either tell us to make and accusation or a
% suggestion. In the case of the latter, we are told what to suggest, and can
% input which player showed us a card as a result, if any.
ourTurn :-
    write('It is our turn!'), nl,
    checkLocationChange,
    accusation([Acc | AccTail]),
    (  AccTail = []
    -> tryToAccuse(Acc)
    ;  ( (ourLocation(L), !,
	  makeSuggestion(L))
       ; (write('You are not in a room, so can\'t make a suggestion.'), nl))
    ), !.


% Possibly update our own location. This tells us where we are, and then prompts
% us to see if we need to enter a new location.
checkLocationChange :-
    (  ourLocation(L)
    -> write('We are in the '), write(L), write('. ')
    ;  write('You are not in a room. ')
    ), !,
    write('Did we change locations? [yes/no]: '),
    read_line_to_codes(user_input, YN),
    atom_codes(C, YN),
    handleLocationChange(C), !.

% Used by: checkLocationChange.
% The result of saying 'yes' to changing our location.
handleLocationChange(yes) :-
    write('Where are we now? '),
    read_line_to_codes(user_input, String),
    forceRetract(ourLocation(_)),
    (  String = []
    -> true
    ;  atom_codes(L, String),
       assert(ourLocation(L))
    ).

% Used by: checkLocationChange.
% The result of saying 'no' to changing our location.
handleLocationChange(no).


% There was a single accusation left (a Player, Location, and Weapon). This
% alerts us to the fact that we need to make the accusation!
tryToAccuse([P, L, W]) :-
    write('You should make an accusation!'), nl,
    write('Accuse '), write(P), write(' in the '), write(L), write(' with the '), write(W), write('!'), nl,
    write('...'), read_line_to_codes(user_input, _),
    write('Were you right?! [yes/no]:  '), read_line_to_codes(user_input, YN), nl,
    atom_codes(C, YN),
    (  C = yes
    -> write('Congratulations!!')
    ;  write('We must have a bug... oops :(')
    ), nl, !.


% On our turn, we can make a suggestion (if the contents of the envelope are
% still fuzzy). This gets the next suggestion applicable to the room we are in,
% and then defers to asking which player shows us a card from it.
makeSuggestion(L) :-
    suggestion([P, L, W]),
    nl, write('Suggest '), write(P), write(' in the '), write(L), write(' with the '), write(W), nl,
    ourPlayer(Us),
    (cycleOurSuggestion(P, L, W, Us) ; true), !.


% Iterate the players in turn-order, asking if any of them could show us one
% of the cards from our suggestion. Our knowledge is updated for each player
% that fails to show us one of the cards, or can finally show us one of the
% three. This terminates when we see that, or we've come full-circle.
cycleOurSuggestion(P, L, W, PrevPlayer) :-
    nextTurn(PrevPlayer, CurPlayer),
    (  ourPlayer(CurPlayer)
    -> write('Done a full lap! No one could show us anything.'), nl
    ;  write('Which card could '), write(CurPlayer), write(' show you?	'),
       read_line_to_codes(user_input, String),
       (  String = []
       -> couldntRefute(CurPlayer, P, L, W),
	  cycleOurSuggestion(P, L, W, CurPlayer)
       ;  atom_codes(Card, String),
	  addCard(CurPlayer, Card)
       )
    ), !.


% Handle I/O for an opponent's turn. This means that we specify the suggestion
% they make (if any), and defer to seeing which player can show them a card.
opponentTurn(Them) :-
    write('It is '), write(Them), write('\'s turn.'), nl,
    write('What was their suggestion? Enter the player, then location, then weapon, separated'), nl,
    write('by spaces. If they didn\'t make a suggestion, then enter nothing.'), nl,
    write('  '), read_line_to_codes(user_input, String), nl,
    (  String = []
    -> true
    ;  atom_codes(C, String),
       atomic_list_concat([P, L, W], ' ', C),
       cycleTheirSuggestion(P, L, W, Them, Them)
    ), !.


% Iterate the players in turn-order (starting after the player whose turn it
% is). We note which players failed to show them a card from their suggestion,
% and which player could. This terminates when they see a card, or when no one
% could show them any cards.
cycleTheirSuggestion(P, L, W, Them, PrevPlayer) :-
    nextTurn(PrevPlayer, CurPlayer),
    (  Them = CurPlayer
    -> write('Done a full lap! No one could show them anything.'), nl
    ;  write('Could '), write(CurPlayer), write(' show them anything?  [yes/no]:  '),

       read_line_to_codes(user_input, String),
       atom_codes(C, String),
       (  C == yes
       -> showedCard(CurPlayer, P, L, W)
       ;  couldntRefute(P, L, W, CurPlayer),
	  cycleTheirSuggestion(P, L, W, Them, CurPlayer)
       )
    ), !.
