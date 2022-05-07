
% --- LISTAGENS ---
listarEquipas:- equipa(NU,NO,FU,TI,_),
                format('~n[ ~w - ~w - ~w - ~w ]~n',[NU,NO,FU,TI]),fail.

listarJogadores:- jogador(NUM_P,CAMISOLA,NOM_P,AGE_P,ALT_P,PESO_P,GEN_P,POS_P,GOL_P,TEAM), 
                equipa(TEAM,NOM_T,_,_,_),
                format('~n[ ~w - ~w - ~w - ~w - ~w - ~w - ~w - ~w - ~w - ~w]~n',[NUM_P,CAMISOLA,NOM_P,AGE_P,ALT_P,PESO_P,GEN_P,POS_P,GOL_P,NOM_T]),fail.
                    

listarTreinadores:- treinador(NUM_P,NOM_P,AGE_P,ALT_P,PESO_P,GEN_P,TEAM), 
                equipa(TEAM,NOM_T,_,_,_),
                format('~n[ ~w - ~w - ~w - ~w - ~w - ~w - ~w ]~n',[NUM_P,NOM_P,AGE_P,ALT_P,PESO_P,GEN_P,NOM_T]),fail.
                    
listarUmaEquipa(NUM_TEAM):-format('~n-----------------------~n'),
                            equipa(NUM_TEAM,NO,FU,TI,_),
                            format('NOME DA EQUIPA:  ~w ~n',[NO]),
                            format('ANO DE FUNDACAO: ~w ~n',[FU]),
                            format('QTD DE TITULOS:  ~w ~n',[TI]),
                            format('-----------------------~n'),fail.
listarUmaEquipa(NUM_TEAM):-treinador(_,NOM_T,_,_,_,_,NUM_TEAM),equipa(NUM_TEAM,_,_,_,_),
                            format('~n[TREINADOR:  ~w ]~n',[NOM_T]),fail.

listarUmaEquipa(NUM_TEAM):-format('~n[JOGADORES]~n'),
                            jogador(_,_,NOM_P,_,_,_,_,POS_P,_,NUM_TEAM), 
                            equipa(NUM_TEAM,_,_,_,_),
                            format('~n» ~w - ~w ~n',[NOM_P,POS_P]),fail.

listarUmaEquipa(_):-format('~n-----------------------~n'),
                            pergunta("~n PRESSIONE [ENTER]~n",_),!.

listar_jogos:-  jornada(J),
                format('~n~n [JORNADA ~w] ~n',[J]),
                jogo(J,_,_,EQUIPA1,_,EQUIPA2,_,_), 
                equipa(EQUIPA1,NOME1,_,_,_),
                equipa(EQUIPA2,NOME2,_,_,_),
                format('~n» ~w - ~w ~n',[NOME1,NOME2]),fail.

listar_jogos:-format('~n-----------------------~n'),
                            pergunta("~n PRESSIONE [ENTER]~n",_),!.


listar_jogos_marcados:-  jornada(J),
                format('~n~n [JORNADA ~w] ~n',[J]),
                jogo(J,NUM,_,EQUIPA1,_,EQUIPA2,_,E),
                E = 0, 
                equipa(EQUIPA1,NOME1,_,_,_),
                equipa(EQUIPA2,NOME2,_,_,_),
                format('~n ~w» [~w - ~w] ~n',[NUM,NOME1,NOME2]),fail.

listar_jogos_marcados:-format('~n-----------------------~n'),!.

listar_jogos_iniciados:-  jornada(J),
                format('~n~n [JORNADA ~w] ~n',[J]),
                jogo(J,NUM,_,EQUIPA1,_,EQUIPA2,_,E),
                E = 1, 
                equipa(EQUIPA1,NOME1,_,_,_),
                equipa(EQUIPA2,NOME2,_,_,_),
                format('~n ~w» [~w - ~w] ~n',[NUM,NOME1,NOME2]),fail.

listar_jogos_iniciados:-format('~n-----------------------~n'),!.


listar_jogos_terminados:-  jornada(J),
                format('~n~n [JORNADA ~w] ~n',[J]),
                jogo(J,NUM,_,EQUIPA1,GOL1,EQUIPA2,GOL2,E),
                E = 2, 
                equipa(EQUIPA1,NOME1,_,_,_),
                equipa(EQUIPA2,NOME2,_,_,_),
                format('~n ~w« ~w (~w) - (~w) ~w ~n',[NUM,NOME1,GOL1,GOL2,NOME2]),fail.

listar_jogos_terminados:-format('~n-----------------------~n'),
                            pergunta("~n PRESSIONE [ENTER]~n",_),!.


listar_jogadores_da_equipa(NUMERO_EQUIPA):-
                                        equipa(NUMERO_EQUIPA,NOM,_,_,_),
                                        format('~n Jogadores da equipa ~w~n',[NOM]),
                                        jogador(COD,_,NOME,_,_,_,_,_,_,NUMERO_EQUIPA),
                                        format('~n~w  ~w',[COD,NOME]),fail.
                                        
listar_jogadores_da_equipa(_):-format('~n-----------------------~n'),!.
                            

ver_pontuacao:- equipa(_,NO,_,_,PON),
                format('~n » ~w : ~w pts ~n',[NO,PON]),fail.

ver_pontuacao:- format('~n-------------~n'),!.
