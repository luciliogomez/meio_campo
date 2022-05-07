
% --- Validacoes ---

% valida se a equipa existe
team_exists(X,R):-equipa(X,_,_,_,_),R is 1,!.
team_exists(_,R):-R is 0.


% valida se a jornada foi criada
jornada_exists(X,R):-jornada(X),R is 1,!.
jornada_exists(_,R):-R is 0.


% verifica se sao a mesma equipa
mesma_equipa(E1,E2) :- E1 = E2.


% verifica se as duas equipas ja se defrontaram
jogo_repetido(EQUIPA1,EQUIPA2):- jogo(_,_,_,EQUIPA1,_,EQUIPA2,_,_);jogo(_,_,_,EQUIPA2,_,EQUIPA1,_,_).


% verifica se o adversario ja tem jogo marcado com outra equipa nesta jornada
adversario_invalido(J,EQUIPA1,EQUIPA2):- 
                                        jornada(J),!,
                                        (
                                            (jogo(J,_,_,EQUIPA1,_,_,_,_);jogo(J,_,_,EQUIPA2,_,_,_,_));
                                            (jogo(J,_,_,_,_,EQUIPA1,_,_);jogo(J,_,_,_,_,EQUIPA2,_,_))
                                        ).


% verfica se há numeros de camisolas iguais na equipa
is_camisola_valid(NUMERO_TEAM,CAMISOLA,R):- equipa(NUMERO_TEAM,_,_,_,_),
                                            jogador(_,CAM,_,_,_,_,_,_,_,NUMERO_TEAM),
                                            CAM = CAMISOLA,
                                            format('~n --- FALHA: NUMERO JA ATRIBUIDO A  OUTRO JOGADOR DA EQUIPA --- ~n'),
                                            R is 0,!.
is_camisola_valid(_,_,R):-R is 1,!.


% valida o Genero
is_valid_genero(GEN,R):- GEN = 'M',R is 1,!.
is_valid_genero(GEN,R):- GEN = 'm',R is 1,!.
is_valid_genero(GEN,R):- GEN = 'F',R is 1,!.
is_valid_genero(GEN,R):- GEN = 'f',R is 1,!.
is_valid_genero(_,R):- format('~n --- ERRO: GENERO INVÁLIDO ~n '),R is 0,!.


% valida a criacao do jogador
is_valid_player(NUM_TEAM,_,_,_,_,_,_,_,_,R):- team_exists(NUM_TEAM,A), A \= 1,format('~n --- FALHA: Equipa não existe. --- ~n'),R is 0,!.
is_valid_player(_,_,_,AGE,_,_,_,_,_,R):- AGE >=45,format('~n --- FALHA: IDADE AVANCADA. --- ~n'),R is 0,!.
is_valid_player(_,_,_,_,_,_,_,_,_,R):- R is 1,!.

is_valid_age(AGE):- AGE >= 10, AGE =< 45.


% valida a criacao da equipa
is_valid_team(N,_,_,_,R):-team_exists(N,A), A = 1, format('~n --- FALHA: NUMERO DA EQUIPA DUPLICADA ---~n'),R is 0,!.
is_valid_team(_,NOME,_,_,R):-equipa(_,NO,_,_,_),NO = NOME, format('~n --- FALHA: NOME DE EQUIPA DUPLICADO ---~n'),R is 0,!.                
is_valid_team(_,_,_,_,R):-R is 1,!.                


% validando a marcacao do jogo

% valida se a equipa existe
is_valid_game(_,EQUIPA1,_,R):-team_exists(EQUIPA1,A),
                                        A \= 1,
                                        format('~n --- FALHA: EQUIPA [~W] NÃO ENCONTRADA. --- ~n',[EQUIPA1]),
                                        R is 0,!.

% valida se a equipa existe
is_valid_game(_,_,EQUIPA2,R):-team_exists(EQUIPA2,A),
                                        A \= 1,
                                        format('~n --- FALHA: EQUIPA [~W] NÃO ENCONTRADA. --- ~n',[EQUIPA2]),
                                        R is 0,!.

% valida se sao a mesma equipa
is_valid_game(_,EQUIPA1,EQUIPA2,R):-mesma_equipa(EQUIPA2,EQUIPA1),
                                        format('~n --- FALHA: MESMA EQUIPA. --- ~n'),
                                        R is 0,!.

% valida a repeticao do jogo
is_valid_game(_,EQUIPA1,EQUIPA2,R):-jogo_repetido(EQUIPA2,EQUIPA1),
                                        format('~n --- FALHA: JOGO REPETIDO. Estas equipas já se defrontaram --- ~n'),
                                        R is 0,!.

% valida se o adversario ja jogou na jornada
is_valid_game(JORNADA,EQUIPA1,EQUIPA2,R):-adversario_invalido(JORNADA,EQUIPA2,EQUIPA1),
                                        format('~n --- FALHA: ADVERSARIO INVALIDO.     DICA: Escolhe outro adversario ou crie uma nova jornada. --- ~n'),
                                        R is 0,!.

% valida se a equipa tem jogadores
is_valid_game(_,EQUIPA1,_,R):- team_has_players(EQUIPA1,H),H = 0,
                                        equipa(EQUIPA1,NO,_,_,_),
                                        format('~n --- FALHA: EQUIPA ~w NÃO TEM JOGADORES. --- ~n',[NO]),
                                        R is 0,!.

% valida se a equipa tem jogadores
is_valid_game(_,_,EQUIPA2,R):- team_has_players(EQUIPA2,H),H = 0,
                                        equipa(EQUIPA2,NO,_,_,_),
                                        format('~n --- FALHA: EQUIPA ~w NÃO TEM JOGADORES. --- ~n',[NO]),
                                        R is 0,!.

% valida se a equipa tem treinador
is_valid_game(_,EQUIPA1,_,R):- team_has_coach(EQUIPA1,H),H = 0,
                                        equipa(EQUIPA1,NO,_,_,_),
                                        format('~n --- FALHA: EQUIPA ~w NÃO TEM TREINADOR. --- ~n',[NO]),
                                        R is 0,!.
% valida se a equipa tem treinador
is_valid_game(_,_,EQUIPA2,R):- team_has_coach(EQUIPA2,H),H = 0,
                                        equipa(EQUIPA2,NO,_,_,_),
                                        format('~n --- FALHA: EQUIPA ~w NÃO TEM TREINADOR. --- ~n',[NO]),
                                        R is 0,!.
% valida se a jornada foi criada
is_valid_game(JORNADA,_,_,R):-jornada_exists(JORNADA,A),
                                        A \= 1,
                                        format('~n --- FALHA: JORNADA NÃO ENCONTRADA. ~n     DICA: Informe outra jornada ou crie uma. --- ~n'),
                                        R is 0,!.

is_valid_game(_,_,_,R):- R is 1,!.


% verifica se a equipa tem jogadores
team_has_players(TEAM,R):-equipa(TEAM,_,_,_,_), jogador(_,_,_,_,_,_,_,_,_,TEAM),R is 1,!.
team_has_players(_,R):-R is 0,!.

% verifica se a equipa tem treinadores
team_has_coach(TEAM,R):-equipa(TEAM,_,_,_,_), treinador(_,_,_,_,_,_,TEAM),R is 1,!.
team_has_coach(_,R):-R is 0,!.