
% --- Validacoes ---
team_exists(X,R):-equipa(X,_,_,_,_),R is 1,!.
team_exists(_,R):-R is 0.

jornada_exists(X,R):-jornada(X),R is 1,!.
jornada_exists(_,R):-R is 0.

mesma_equipa(E1,E2) :- E1 = E2.

jogo_repetido(EQUIPA1,EQUIPA2):- jogo(_,_,_,EQUIPA1,_,EQUIPA2,_,_);jogo(_,_,_,EQUIPA2,_,EQUIPA1,_,_).

adversario_invalido(J,EQUIPA1,EQUIPA2):- 
                                        jornada(J),!,
                                        (
                                            (jogo(J,_,_,EQUIPA1,_,_,_,_);jogo(J,_,_,EQUIPA2,_,_,_,_));
                                            (jogo(J,_,_,_,_,EQUIPA1,_,_);jogo(J,_,_,_,_,EQUIPA2,_,_))
                                        ).

is_camisola_valid(NUMERO_TEAM,CAMISOLA,R):- equipa(NUMERO_TEAM,_,_,_,_),
                                            jogador(NUM,CAM,_,_,_,_,_,_,_,NUMERO_TEAM),
                                            CAM = CAMISOLA,
                                            format('~n --- FALHA: NUMERO DE CAMISOLA DUPLICADO --- ~n --- NUMERO JA ATRIBUIDO A  OUTRO JOGADOR DA EQUIPA --- ~n'),
                                            R is 0,!.
is_camisola_valid(NUMERO_TEAM,CAMISOLA,R):-R is 1,!.

is_valid_genero(GEN,R):- GEN = 'M',R is 1,!.
is_valid_genero(GEN,R):- GEN = 'm',R is 1,!.
is_valid_genero(GEN,R):- GEN = 'F',R is 1,!.
is_valid_genero(GEN,R):- GEN = 'f',R is 1,!.
is_valid_genero(_,R):- format('~n --- ERRO: GENERO INVÁLIDO ~n '),R is 0,!.


is_valid_player(NUM_TEAM,_,_,_,_,_,_,_,_,R):- team_exists(NUM_TEAM,A), A \= 1,format('~n --- FALHA: Equipa não existe. --- ~n'),R is 0,!.
is_valid_player(_,_,_,AGE,_,_,_,_,_,R):- AGE >=45,format('~n --- FALHA: IDADE AVANCADA. --- ~n'),R is 0,!.
is_valid_player(_,_,_,_,_,_,_,_,_,R):- R is 1,!.

is_valid_age(AGE):- AGE >= 10, AGE =< 45.


is_valid_team(N,_,_,_,R):-team_exists(N,A), A = 1, format('~n --- FALHA: NUMERO DA EQUIPA DUPLICADA ---~n'),R is 0,!.
is_valid_team(_,NOME,_,_,R):-equipa(_,NO,_,_,_),NO = NOME, format('~n --- FALHA: NOME DE EQUIPA DUPLICADO ---~n'),R is 0,!.                
is_valid_team(_,_,_,_,R):-R is 1,!.                



is_valid_game(_,EQUIPA1,_,R):-team_exists(EQUIPA1,A),
                                        A \= 1,
                                        format('~n --- FALHA: EQUIPA [~W] NÃO ENCONTRADA. --- ~n',[EQUIPA1]),
                                        R is 0,!.

is_valid_game(_,_,EQUIPA2,R):-team_exists(EQUIPA2,A),
                                        A \= 1,
                                        format('~n --- FALHA: EQUIPA [~W] NÃO ENCONTRADA. --- ~n',[EQUIPA2]),
                                        R is 0,!.

is_valid_game(_,EQUIPA1,EQUIPA2,R):-mesma_equipa(EQUIPA2,EQUIPA1),
                                        format('~n --- FALHA: MESMA EQUIPA. --- ~n'),
                                        R is 0,!.


is_valid_game(_,EQUIPA1,EQUIPA2,R):-jogo_repetido(EQUIPA2,EQUIPA1),
                                        format('~n --- FALHA: JOGO REPETIDO. Estas equipas já se defrontaram --- ~n'),
                                        R is 0,!.
is_valid_game(JORNADA,EQUIPA1,EQUIPA2,R):-adversario_invalido(JORNADA,EQUIPA2,EQUIPA1),
                                        format('~n --- FALHA: ADVERSARIO INVALIDO.     DICA: Escolhe outro adversario ou crie uma nova jornada. --- ~n'),
                                        R is 0,!.


is_valid_game(JORNADA,_,_,R):-jornada_exists(JORNADA,A),
                                        A \= 1,
                                        format('~n --- FALHA: JORNADA NÃO ENCONTRADA. ~n     DICA: Informe outra jornada ou crie uma. --- ~n'),
                                        R is 0,!.

is_valid_game(_,_,_,R):- R is 1,!.