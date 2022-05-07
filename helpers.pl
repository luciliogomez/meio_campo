
% Pedindo dados do utilizador
pergunta(Question,Answer):-
	format(Question),
	gets(Answer).

% input from user
gets(S):-
	read_line_to_codes(user_input,C),
	name(S,C).


% Salvando base de dados em disco
salva(Predicado,Arquivo):-
	tell(Arquivo),
	listing(Predicado),
	told.




%busca melhor MARCADOR

melhor_marcador:-
                jogador(_,_,NO,_,_,_,_,_,GOLOS,EQUIPA),
                best(B_EQ,B_JOGADOR,B_GOL),
                GOLOS > B_GOL,
                retract(best(B_EQ,B_JOGADOR,B_GOL)),
                assertz(best(EQUIPA,NO,GOLOS)),
                salva(best,'database/best.bd').

melhor_marcador:-write('').



buscaEquipa(NOME,E1,_,NUMERO):-equipa(E1,NOME,_,_,_),NUMERO is E1,!.
buscaEquipa(NOME,_,E2,NUMERO):-equipa(E2,NOME,_,_,_),NUMERO is E2,!.
buscaEquipa(_,_,_,NUMERO):-NUMERO is 0,!.

atribuirGolo(JORN,NUMJOGO,DATA,E1,GOL1,E2,GOL2,ESTADO,NUMEQUIPA,NUM_GOLS,R):-
                NUMEQUIPA = E1,
                jogo(JORN,NUMJOGO,DATA,E1,GOL1,E2,GOL2,ESTADO),
                NUM_GOLOS is GOL1 + NUM_GOLS,
                retract(jogo(JORN,NUMJOGO,DATA,E1,GOL1,E2,GOL2,ESTADO)),
                assertz(jogo(JORN,NUMJOGO,DATA,E1,NUM_GOLOS,E2,GOL2,ESTADO)),
                R is 1,!.

atribuirGolo(JORN,NUMJOGO,DATA,E1,GOL1,E2,GOL2,ESTADO,NUMEQUIPA,NUM_GOLS,R):-
                NUMEQUIPA = E2,
                jogo(JORN,NUMJOGO,DATA,E1,GOL1,E2,GOL2,ESTADO),
                NUM_GOLOS is GOL2 + NUM_GOLS,
                retract(jogo(JORN,NUMJOGO,DATA,E1,GOL1,E2,GOL2,ESTADO)),
                assertz(jogo(JORN,NUMJOGO,DATA,E1,GOL1,E2,NUM_GOLOS,ESTADO)),
                R is 1,!.

atribuirGolo(_,_,_,_,_,_,_,_,_,_,R):-
                R is 0,!.

atribuir_pontuacao(E1,GOL1,_,GOL2,R):- GOL1 > GOL2,
                                    adicionar_ponto(3,E1),
                                    maior_ponto(E1),
                                    R is 1,!.
atribuir_pontuacao(_,GOL1,E2,GOL2,R):- GOL1 < GOL2,
                                    adicionar_ponto(3,E2),
                                    maior_ponto(E2),
                                    R is 1,!.
atribuir_pontuacao(E1,GOL1,E2,GOL2,R):- GOL1 = GOL2,
                                    adicionar_ponto(1,E1),
                                    maior_ponto(E1),
                                    adicionar_ponto(1,E2),
                                    maior_ponto(E2),
                                    R is 1,!.
atribuir_pontuacao(_,_,_,_,R):-R is 0,!.

adicionar_ponto(PONTO,EQUIPA):- equipa(EQUIPA,NOM,FUN,TIT,PTS),
                                NEW_PT is PTS + PONTO,
                                retract(equipa(EQUIPA,NOM,FUN,TIT,PTS)),
                                assertz(equipa(EQUIPA,NOM,FUN,TIT,NEW_PT)),
                                !.


% busca equipa com maior pontuacao
maior_ponto(EQUIPA):- 
                    winner(EQ,PT),
                    equipa(EQUIPA,_,_,_,PTS),
                    PTS > PT,
                    retract(winner(EQ,PT)),
                    assertz(winner(EQUIPA,PTS)),
                    !.
maior_ponto(_):-!.
                    


atualiza_total_jornadas(TOTAL):-
                                N is TOTAL+1,
                                retract(total_jornadas(TOTAL)),
                                assertz(total_jornadas(N)).

atualiza_total_jogos(TOTAL):-
                                N is TOTAL+1,
                                retract(total_jogos(TOTAL)),
                                assertz(total_jogos(N)).


atualiza_total_equipas(TOTAL):-
                                N is TOTAL+1,
                                retract(total_equipas(TOTAL)),
                                assertz(total_equipas(N)).


adicionar_jornada(X):-
                    N is X+1,
                    assertz(jornada(N)).
