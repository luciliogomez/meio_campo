
% --- Validacoes ---
team_exists(X,R):-equipa(X,_,_,_),R is 1,!.
team_exists(X,R):-R is 0.

is_valid_player(NUM_TEAM,NUM_P,NOM_P,AGE,HEIGHT,WEIGHT,GEN,POS,GOLS,R):- team_exists(NUM_TEAM,A), A \= 1,format('~n --- FALHA: Equipa não existe. --- ~n'),R is 0,!.
is_valid_player(NUM_TEAM,NUM_P,NOM_P,AGE,HEIGHT,WEIGHT,GEN,POS,GOLS,R):- AGE >=45,format('~n --- FALHA: IDADE AVANCADA. --- ~n'),R is 0,!.
is_valid_player(NUM_TEAM,NUM_P,NOM_P,AGE,HEIGHT,WEIGHT,GEN,POS,GOLS,R):- R is 1,!.

is_valid_age(AGE):- AGE >= 10, AGE =< 45.
