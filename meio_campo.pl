% THE PROJECT STARTS HERE

start:- carregaDados,
        repeat,
        main_menu,
        pergunta("Digite a Opcao~n",R),
        open_menu(R,A),
        A=1,!.

open_menu(1,A):- repeat,
                game_menu,
                A is 0,!.
open_menu(2,A):- repeat,
                team_menu,
                A is 0,!.
open_menu(3,A):- repeat,
                statistics_menu,
                pergunta("Deseja Sair? [s/n]~n",R),
                R=s,A is 0,!.
open_menu(4,1).

open_menu(now,A):- repeat,
                gamenow_menu,
                pergunta("Digite a Opcao~n",R),
                R=5,A is 0, !.




team_menu:-
    repeat,
    format('~n*** EQUIPAS  *** ~n'),
    format('-------------~n'),
    format('1- CADASTRAR EQUIPA ~n'),
    format('2- CADASTRAR JOGADORES ~n'),
    format('3- CADASTRAR TREINADOR ~n'),
    format('4- LISTAR EQUIPAS ~n'),
    format('5- LISTAR JOGADORES ~n'),
    format('6- LISTAR TREINADORES ~n'),
    format('7- LISTAR UMA EQUIPA ~n'),
    format('8- VER PONTUACAO ~n'),
    format('9- SAIR ~n~n'),
    pergunta("Digite a Opcao~n",R),
    team_menu_option(R,A),
    A=1,!.

team_menu_option(9,1):-!.
team_menu_option(1,A):-format('~n *** CADASTRAR EQUIPA *** ~n'),
                pergunta("Numero Da Equipa:~n",NUMERO),
                pergunta("Nome Da Equipa:~n",NOME),
                pergunta("Data de Fundacao:~n",FUNDACAO),
                pergunta("Numero de titulos:~n",TITULOS),
                assertz(equipa(NUMERO,NOME,FUNDACAO,TITULOS)),
                total_equipas(TOTAL), N is TOTAL+1,
                retract(total_equipas(TOTAL)),
                assertz(total_equipas(N)),
                format('~n ---  EQUIPA ADICIONADA --- ~n'),
                salva(equipa,'equipas.bd'),
                salva(total_equipas,'total_equipas.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.

team_menu_option(2,A):-format('~n *** CADASTRAR JOGADOR *** ~n'),
                pergunta("Numero Da Equipa:~n",NUMERO_TEAM),
                team_exists(NUMERO_TEAM,E),E = 1,
                pergunta("Numero do Jogador:~n",NUMERO_PLAYER),
                pergunta("Nome:~n",NOME),
                pergunta("Idade:~n",IDADE),
                is_valid_age(IDADE),
                pergunta("Altura:~n",ALTURA),
                pergunta("Peso:~n",PESO),
                pergunta("Genero:~n",GENERO),
                pergunta("Posicao:~n",POSICAO),
                pergunta("Numero de Golos:~n",GOLOS),
                assertz(jogador(NUMERO_PLAYER,NOME,IDADE,ALTURA,PESO,GENERO,POSICAO,GOLOS,NUMERO_TEAM)),
                format('~n ---  JOGADOR ADICIONADO --- ~n'),
                salva(jogador,'jogadores.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.

team_menu_option(2,A):-format('~n --- FALHA AO CADASTRAR. --- ~n'),
                    pergunta("~n Pressione [Enter]",_),
                    A is 0,!.


team_menu_option(3,A):-format('~n *** CADASTRAR TREINADOR *** ~n'),
                pergunta("Numero Da Equipa:~n",NUMERO_TEAM),
                team_exists(NUMERO_TEAM,E),E = 1,
                pergunta("Codigo do Treinador:~n",CODE_COACH),
                pergunta("Nome:~n",NOME),
                pergunta("Idade:~n",IDADE),
                pergunta("Altura:~n",ALTURA),
                pergunta("Peso:~n",PESO),
                pergunta("Genero:~n",GENERO),
                assertz(treinador(CODE_COACH,NOME,IDADE,ALTURA,PESO,GENERO,NUMERO_TEAM)),
                format('~n ---  TREINADOR ADICIONADO --- ~n'),
                salva(treinador,'treinadores.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.

team_menu_option(3,A):-format('~n --- FALHA AO CADASTRAR. --- ~n'),
                    pergunta("~n Pressione [Enter]",_),
                    A is 0,!.

team_menu_option(4,A):-format('~n *** LISTA DE EQUIPAS *** ~n'),
                    listarEquipas,
                    A is 0,!.
team_menu_option(4,A):-pergunta(" ~nPressione [Enter]~n",_),
                    A is 0,!.

team_menu_option(5,A):-format('~n *** LISTA DE JOGADORES *** ~n'),
                    listarJogadores,
                    A is 0,!.
team_menu_option(5,A):-pergunta("~nPressione [Enter]~n",_),
                    A is 0,!.

team_menu_option(6,A):-format('~n    *** LISTA DE TREINADORES *** ~n'),
                    listarTreinadores,
                    A is 0,!.
team_menu_option(6,A):-pergunta("~nPressione [Enter]~n",_),
                    A is 0,!.

team_menu_option(7,A):-format('~n    *** LISTAR UMA EQUIPA *** ~n'),
                    pergunta("~n NUMERO DA EQUIPA:~n",NUMERO_TEAM),
                    team_exists(NUMERO_TEAM,R),
                    R = 1,
                    listarUmaEquipa(NUMERO_TEAM),
                    A is 0,!.
team_menu_option(7,A):-pergunta("~n --- EQUIPA NÃO ENCONTRADA --- ~n~n    Pressione [Enter]~n",_),
                    A is 0,!.

statistics_menu:-
    format('~n*** ESTATISTICA  *** ~n'),
    format('-------------~n'),
    format('1- TABELA CLASSIFICATIVA ~n'),
    format('2- VENCEDOR ~n'),
    format('3- MELHOR MARCADOR ~n'),
    format('4- SAIR ~n~n').


main_menu:-
    format('~n*** MENU  *** ~n'),
    format('-------------~n'),
    format('1- JOGO ~n'),
    format('2- EQUIPAS ~n'),
    format('3- ESTATISTICAS ~n'),
    format('4- SAIR ~n~n').

game_menu:-
    repeat,
    format('~n*** JOGOS  *** ~n'),
    format('-------------~n'),
    format('1- ADICIONAR JORNADA ~n'),
    format('2- CADASTRAR JOGOS ~n'),
    format('3- JOGO A DECORRER ~n'),
    format('4- LISTAR JOGOS ~n'),
    format('5- LISTAR TODOS RESULTADOS ~n'),
    format('0- SAIR ~n~n'),
    pergunta("Digite a Opcao~n",R),
    game_menu_option(R,A),
    A = 1,!.

game_menu_option(0,1):-!.

game_menu_option(1,A):-format('~n *** ADICIONAR JORNADA *** ~n'),
                pergunta("Pretende Adicionar uma nova jornada? [s/n]:~n",R),
                R = 's',
                total_jornadas(TOTAL),
                atualiza_total_jornadas(TOTAL),
                adicionar_jornada(TOTAL),
                format('~n ---  JORNADA ADICIONADA --- ~n'),
                salva(total_jornadas,'total_jornadas.bd'),
                salva(jornada,'jornadas.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.


game_menu_option(1,A):-format('~n --- IMPOSSÍVEL ADICIONAR JORNADA --- ~n'),
                        pergunta("~n Pressione [Enter]",_),
                        A is 0,!.


game_menu_option(2,A):-format('~n *** ADICIONAR JOGO *** ~n'),
                pergunta("Indique a Jornada:~n",JORNADA),
                pergunta("Numero da equipa anfitriã:~n",EQUIPA1),
                pergunta("Numero da equipa Visitante:~n",EQUIPA2),
                pergunta("Data do Jogo:~n",DATA_JOGO),
                is_valid_game(JORNADA,EQUIPA1,EQUIPA2,R),
                R = 1,
                total_jogos(TOTAL),
                NUMERO_DO_JOGO is (TOTAL+1),
                atualiza_total_jogos(TOTAL),
                assertz(jogo(JORNADA,NUMERO_DO_JOGO,DATA_JOGO,EQUIPA1,0,EQUIPA2,0,0)),
                format('~n ---  JOGO MARCADO --- ~n'),
                salva(total_jogos,'total_jogos.bd'),
                salva(jogo,'jogos.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.


game_menu_option(2,A):-
                        pergunta("~n Pressione [Enter]",_),
                        A is 0,!.

game_menu_option(3,A):-repeat,
                        gamenow_menu,
                        A is 0,!.

game_menu_option(4,A):-format('~n *** LISTA DE JOGOS *** ~n'),
                        format('-----------------------~n'),
                        listar_jogos,
                        A is 0,!.

game_menu_option(5,A):-format('~n *** RESULTADOS *** ~n'),
                        format('-----------------------~n'),
                        listar_jogos_terminados,
                        A is 0,!.




gamenow_menu:-
    repeat,
    format('~n*** JOGO A DECORRER  *** ~n'),
    format('-------------~n'),
    format('1- INICIAR JOGO ~n'),
    format('2- ATRIBUIR GOLOS ~n'),
    format('3- TERMINAR JOGO ~n'),
    format('0- VOLTAR ~n'),
    pergunta("Digite a Opcao~n",R),
    gamenow_menu_option(R,A),
    A = 1,!.

gamenow_menu_option(0,A):- A is 1,!.

gamenow_menu_option(1,A):-format('~n *** INICIAR UM JOGO *** ~n'),
                listar_jogos_marcados,
                pergunta("Indique o numero do Jogo:~n",NUMERO_DO_JOGO),
                jogo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,ESTADO),
                ESTADO = 0,
                retract(jogo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,ESTADO)),
                assertz(jogo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,1)),
                format('~n ---  JOGO INICIADO --- ~n'),
                salva(jogo,'jogos.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.

gamenow_menu_option(1,A):-format('~n FALHA AO INICIAR JOGO ~n'),A is 0,!.


gamenow_menu_option(3,A):-format('~n *** TERMINAR UM JOGO *** ~n'),
                listar_jogos_iniciados,
                pergunta("Indique o numero do Jogo:~n",NUMERO_DO_JOGO),
                jogo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,ESTADO),
                ESTADO = 1,
                retract(jogo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,ESTADO)),
                assertz(jogo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,2)),
                format('~n ---  JOGO TERMINADO --- ~n'),
                format('~n --- RESULTADO DO JOGO ~n'),
                equipa(EQUIPA1,NOME1,_,_),
                equipa(EQUIPA2,NOME2,_,_),
                format('~n» ~w ~w : ~w ~w «~n',[NOME1,GOLOS1,GOLOS2,NOME2]),
                salva(jogo,'jogos.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.

gamenow_menu_option(3,A):-format('~n FALHA AO TERMINAR JOGO ~n'),A is 0,!.


gamenow_menu_option(2,A):-format('~n *** ATRIBUIR GOLOS *** ~n'),
                listar_jogos_iniciados,
                pergunta("Indique o numero do Jogo:~n",NUMERO_DO_JOGO),
                jogo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,ESTADO),
                ESTADO = 1,
                pergunta("NOME DA EQUIPA:~n",NOME_EQUIPA),
                buscaEquipa(NOME_EQUIPA,EQUIPA1,EQUIPA2,NUMERO_EQUIPA),
                NUMERO_EQUIPA \= 0,
                listar_jogadores_da_equipa(NUMERO_EQUIPA),
                pergunta("Indique o numero do Jogador:~n",NUMERO_DO_JOGADOR),
                jogador(NUMERO_DO_JOGADOR,NOME_DO_JOGADOR,ALT,PES,GEN,IDA,POS,GOLS,NUMERO_EQUIPA),
                pergunta("Quantos Golos:~n",NUM_GOLS),
                atribuirGolo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,ESTADO,NUMERO_EQUIPA,NUM_GOLS,R),
                R = 1,
                QTD_GOLS is GOLS + NUM_GOLS,
                retract(jogador(NUMERO_DO_JOGADOR,NOME_DO_JOGADOR,ALT,PES,GEN,IDA,POS,GOLS,NUMERO_EQUIPA)),
                assertz(jogador(NUMERO_DO_JOGADOR,NOME_DO_JOGADOR,ALT,PES,GEN,IDA,POS,QTD_GOLS,NUMERO_EQUIPA)),
                format('~n ---  CONCLUIDO --- ~n'),
                salva(jogo,'jogos.bd'),
                salva(jogador,'jogadores.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.

gamenow_menu_option(2,A):-format('~n FALHA AO ATRIBUIR GOLOS ~n'),A is 0,!.

buscaEquipa(NOME,E1,_,NUMERO):-equipa(E1,NOME,_,_),NUMERO is E1,!.
buscaEquipa(NOME,_,E2,NUMERO):-equipa(E2,NOME,_,_),NUMERO is E2,!.
buscaEquipa(_,_,_,NUMERO):-NUMERO is 0,!.

atribuirGolo(JORN,NUMJOGO,DATA,E1,GOL1,E2,GOL2,ESTADO,NUMEQUIPA,NUM_GOLS,R):-
                NUMEQUIPA = E1,
                retract(jogo(JORN,NUMJOGO,DATA,E1,GOL1,E2,GOL2,ESTADO)),
                assertz(jogo(JORN,NUMJOGO,DATA,E1,NUM_GOLS,E2,GOL2,ESTADO)),
                R is 1,!.

atribuirGolo(JORN,NUMJOGO,DATA,E1,GOL1,E2,GOL2,ESTADO,NUMEQUIPA,NUM_GOLS,R):-
                NUMEQUIPA = E2,
                retract(jogo(JORN,NUMJOGO,DATA,E1,GOL1,E2,GOL2,ESTADO)),
                assertz(jogo(JORN,NUMJOGO,DATA,E1,GOL1,E2,NUM_GOLS,ESTADO)),
                R is 1,!.

atribuirGolo(_,_,_,_,_,_,_,_,_,_,R):-
                R is 0,!.

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

% --- LISTAGENS ---
listarEquipas:- equipa(NU,NO,FU,TI),
                format('~n[ ~w - ~w - ~w - ~w ]~n',[NU,NO,FU,TI]),fail.

listarJogadores:- jogador(NUM_P,NOM_P,AGE_P,ALT_P,PESO_P,GEN_P,POS_P,GOL_P,TEAM), 
                equipa(TEAM,NOM_T,_,_),
                format('~n[ ~w - ~w - ~w - ~w - ~w - ~w - ~w - ~w - ~w]~n',[NUM_P,NOM_P,AGE_P,ALT_P,PESO_P,GEN_P,POS_P,GOL_P,NOM_T]),fail.
                    

listarTreinadores:- treinador(NUM_P,NOM_P,AGE_P,ALT_P,PESO_P,GEN_P,TEAM), 
                equipa(TEAM,NOM_T,_,_),
                format('~n[ ~w - ~w - ~w - ~w - ~w - ~w - ~w ]~n',[NUM_P,NOM_P,AGE_P,ALT_P,PESO_P,GEN_P,NOM_T]),fail.
                    
listarUmaEquipa(NUM_TEAM):-format('~n-----------------------~n'),
                            equipa(NUM_TEAM,NO,FU,TI),
                            format('NOME DA EQUIPA:  ~w ~n',[NO]),
                            format('ANO DE FUNDACAO: ~w ~n',[FU]),
                            format('QTD DE TITULOS:  ~w ~n',[TI]),
                            format('-----------------------~n'),fail.
listarUmaEquipa(NUM_TEAM):-treinador(_,NOM_T,_,_,_,_,NUM_TEAM),equipa(NUM_TEAM,_,_,_),
                            format('~n[TREINADOR:  ~w ]~n',[NOM_T]),fail.

listarUmaEquipa(NUM_TEAM):-format('~n[JOGADORES]~n'),
                            jogador(_,NOM_P,_,_,_,_,POS_P,_,NUM_TEAM), 
                            equipa(NUM_TEAM,_,_,_),
                            format('~n» ~w - ~w ~n',[NOM_P,POS_P]),fail.

listarUmaEquipa(_):-format('~n-----------------------~n'),
                            pergunta("~n PRESSIONE [ENTER]~n",_),!.

listar_jogos:-  jornada(J),
                format('~n~n [JORNADA ~w] ~n',[J]),
                jogo(J,_,_,EQUIPA1,_,EQUIPA2,_,_), 
                equipa(EQUIPA1,NOME1,_,_),
                equipa(EQUIPA2,NOME2,_,_),
                format('~n» ~w - ~w ~n',[NOME1,NOME2]),fail.

listar_jogos:-format('~n-----------------------~n'),
                            pergunta("~n PRESSIONE [ENTER]~n",_),!.


listar_jogos_marcados:-  jornada(J),
                format('~n~n [JORNADA ~w] ~n',[J]),
                jogo(J,NUM,_,EQUIPA1,_,EQUIPA2,_,E),
                E = 0, 
                equipa(EQUIPA1,NOME1,_,_),
                equipa(EQUIPA2,NOME2,_,_),
                format('~n ~w» [~w - ~w] ~n',[NUM,NOME1,NOME2]),fail.

listar_jogos_marcados:-format('~n-----------------------~n'),!.

listar_jogos_iniciados:-  jornada(J),
                format('~n~n [JORNADA ~w] ~n',[J]),
                jogo(J,NUM,_,EQUIPA1,_,EQUIPA2,_,E),
                E = 1, 
                equipa(EQUIPA1,NOME1,_,_),
                equipa(EQUIPA2,NOME2,_,_),
                format('~n ~w» [~w - ~w] ~n',[NUM,NOME1,NOME2]),fail.

listar_jogos_iniciados:-format('~n-----------------------~n'),!.


listar_jogos_terminados:-  jornada(J),
                format('~n~n [JORNADA ~w] ~n',[J]),
                jogo(J,NUM,_,EQUIPA1,GOL1,EQUIPA2,GOL2,E),
                E = 2, 
                equipa(EQUIPA1,NOME1,_,_),
                equipa(EQUIPA2,NOME2,_,_),
                format('~n ~w« ~w (~w) - (~w) ~w ~n',[NUM,NOME1,GOL1,GOL2,NOME2]),fail.

listar_jogos_terminados:-format('~n-----------------------~n'),!.


listar_jogadores_da_equipa(NUMERO_EQUIPA):-
                                        equipa(NUMERO_EQUIPA,NOM,_,_),
                                        format('~n Jogadores da equipa ~w~n',[NOM]),
                                        jogador(COD,NOME,_,_,_,_,_,_,NUMERO_EQUIPA),
                                        format('~n~w ~w',[COD,NOME]),fail.
                                        
listar_jogadores_da_equipa(_):-format('~n-----------------------~n'),
                                pergunta('~nDigite [Enter]~n',_),!.
                            





carregaDados:- carrega('equipas.bd'),
                carrega('jogadores.bd'),
                carrega('treinadores.bd'),
                carrega('jogos.bd'),
                carrega('jornadas.bd'),
                carrega('total_jornadas.bd'),
                carrega('total_equipas.bd'),
                carrega('total_jogos.bd'),
                carrega('menus.pl'),
                carrega('helpers.pl'),
                carrega('validate.pl').
carrega(Arquivo):-
        exists_file(Arquivo),
        consult(Arquivo);true.

