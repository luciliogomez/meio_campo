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
                A is 0,!.
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
                total_equipas(TOTAL), N is TOTAL+1,
                pergunta("Nome Da Equipa:~n",NOME),
                pergunta("Data de Fundacao:~n",FUNDACAO),
                pergunta("Numero de titulos:~n",TITULOS),
                is_valid_team(N,NOME,FUNDACAO,TITULOS,R),
                R = 1,
                assertz(equipa(N,NOME,FUNDACAO,TITULOS,0)),
                retract(total_equipas(TOTAL)),
                assertz(total_equipas(N)),
                format('~n ---  EQUIPA ADICIONADA --- ~n'),
                salva(equipa,'database/equipas.bd'),
                salva(total_equipas,'database/total_equipas.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.
team_menu_option(1,A):-
                    pergunta("~n Pressione [Enter]",_),
                    A is 0,!.


team_menu_option(2,A):-format('~n *** CADASTRAR JOGADOR *** ~n'),
                pergunta("Numero Da Equipa:~n",NUMERO_TEAM),
                team_exists(NUMERO_TEAM,E),E = 1,
                total_jogadores(TOTAL), NUMERO_PLAYER is TOTAL+1,
                pergunta("Nome:~n",NOME),
                pergunta("Numero da camisola:~n",CAMISOLA),
                is_camisola_valid(NUMERO_TEAM,CAMISOLA,R),R = 1,
                pergunta("Idade:~n",IDADE),
                is_valid_age(IDADE),
                pergunta("Altura:~n",ALTURA),
                pergunta("Peso:~n",PESO),
                pergunta("Genero [M/F]:~n",GENERO),
                is_valid_genero(GENERO,R), R = 1,
                pergunta("Posicao:~n",POSICAO),
                assertz(jogador(NUMERO_PLAYER,CAMISOLA,NOME,IDADE,ALTURA,PESO,GENERO,POSICAO,0,NUMERO_TEAM)),
                format('~n ---  JOGADOR ADICIONADO --- ~n'),
                retract(total_jogadores(TOTAL)),
                assertz(total_jogadores(NUMERO_PLAYER)),
                salva(jogador,'database/jogadores.bd'),
                salva(total_jogadores,'database/total_jogadores.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.

team_menu_option(2,A):-format('~n --- FALHA AO CADASTRAR. --- ~n'),
                    pergunta("~n Pressione [Enter]",_),
                    A is 0,!.


team_menu_option(3,A):-format('~n *** CADASTRAR TREINADOR *** ~n'),
                pergunta("Numero Da Equipa:~n",NUMERO_TEAM),
                team_exists(NUMERO_TEAM,E),E = 1,
                total_treinadores(TOTAL), NUMERO_TREINADOR is TOTAL+1,
                pergunta("Nome:~n",NOME),
                pergunta("Idade:~n",IDADE),
                pergunta("Altura:~n",ALTURA),
                pergunta("Peso:~n",PESO),
                pergunta("Genero:~n",GENERO),
                assertz(treinador(NUMERO_TREINADOR,NOME,IDADE,ALTURA,PESO,GENERO,NUMERO_TEAM)),
                format('~n ---  TREINADOR ADICIONADO --- ~n'),
                retract(total_treinadores(TOTAL)),
                assertz(total_treinadores(NUMERO_TREINADOR)),
                salva(treinador,'database/treinadores.bd'),
                salva(total_treinadores,'database/total_treinadores.bd'),
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
team_menu_option(8,A):-format('~n *** PONTUACAO DAS EQUIPAS *** ~n'),
                    ver_pontuacao,
                    pergunta('Digite [Enter]',_),
                    A is 0,!.

statistics_menu:-
    repeat,
    format('~n*** ESTATISTICA  *** ~n'),
    format('-------------~n'),
    format('1- TABELA CLASSIFICATIVA ~n'),
    format('2- VENCEDOR ~n'),
    format('3- MELHOR MARCADOR ~n'),
    format('0- VOLTAR ~n~n'),
    pergunta('Digite a opcao: ~n',R),
    statistics_menu_option(R,A),
    A = 1,!.

statistics_menu_option(0,1):-!.
statistics_menu_option(1,A):-format('~n*** TABELA CLASSIFICATIVA ***~n'), 
                            ver_pontuacao,
                            pergunta('~nDigite [Enter]~n',_),
                            A is 0,!.

statistics_menu_option(2,A):-format('~n*** VENCEDOR DO CAMPEONATO ***~n'), 
                            is_campeonado_closed(R),
                            R = 1,
                            winner(EQUI,PTS),
                            equipa(EQUI,NOME,_,_,_),
                            format('~n~n~n~w com ~w pontos!!!~n~n',[NOME,PTS]),
                            pergunta('~nDigite [Enter]~n',_),
                            A is 0,!.

statistics_menu_option(2,A):- 
                            pergunta('~nDigite [Enter]~n',_),
                            A is 0,!.


statistics_menu_option(3,A):-format('~n*** MELHOR MARCADOR ***~n'), 
                            melhor_marcador,
                            best(TEAM,PLAYER,GOLS),
                            equipa(TEAM,NOME,_,_,_),
                            format('~n~n[~w ~w ~w]~n~n',[NOME,PLAYER,GOLS]),
                            pergunta('~nDigite [Enter]~n',_),
                            A is 0,!.

statistics_menu_option(3,A):- 
                            pergunta('~nDigite [Enter]~n',_),
                            A is 0,!.

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
                salva(total_jornadas,'database/total_jornadas.bd'),
                salva(jornada,'database/jornadas.bd'),
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
                salva(total_jogos,'database/total_jogos.bd'),
                salva(jogo,'database/jogos.bd'),
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
                salva(jogo,'database/jogos.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.

gamenow_menu_option(1,A):-format('~n FALHA AO INICIAR JOGO ~n'),
                                pergunta("~n Pressione [Enter]",_),A is 0,!.


gamenow_menu_option(3,A):-format('~n *** TERMINAR UM JOGO *** ~n'),
                listar_jogos_iniciados,
                pergunta("Indique o numero do Jogo:~n",NUMERO_DO_JOGO),
                jogo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,ESTADO),
                ESTADO = 1,
                retract(jogo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,ESTADO)),
                assertz(jogo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,2)),
                format('~n ---  JOGO TERMINADO --- ~n'),
                format('~n --- RESULTADO DO JOGO ~n'),
                equipa(EQUIPA1,NOME1,_,_,_),
                equipa(EQUIPA2,NOME2,_,_,_),
                format('~n» ~w ~w : ~w ~w «~n',[NOME1,GOLOS1,GOLOS2,NOME2]),
                atribuir_pontuacao(EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,R),
                R = 1,
                salva(jogo,'database/jogos.bd'),
                salva(equipa,'database/equipas.bd'),
                salva(winner,'database/winner.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.

gamenow_menu_option(3,A):-format('~n FALHA AO TERMINAR JOGO ~n'),
                                pergunta("~n Pressione [Enter]",_),A is 0,!.


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
                jogador(NUMERO_DO_JOGADOR,CAMISOLA,NOME_DO_JOGADOR,ALT,PES,GEN,IDA,POS,GOLS,NUMERO_EQUIPA),
                pergunta("Quantos Golos:~n",NUM_GOLS),
                atribuirGolo(JORNADA,NUMERO_DO_JOGO,DATA,EQUIPA1,GOLOS1,EQUIPA2,GOLOS2,ESTADO,NUMERO_EQUIPA,NUM_GOLS,R),
                R = 1,
                QTD_GOLS is GOLS + NUM_GOLS,
                retract(jogador(NUMERO_DO_JOGADOR,CAMISOLA,NOME_DO_JOGADOR,ALT,PES,GEN,IDA,POS,GOLS,NUMERO_EQUIPA)),
                assertz(jogador(NUMERO_DO_JOGADOR,CAMISOLA,NOME_DO_JOGADOR,ALT,PES,GEN,IDA,POS,QTD_GOLS,NUMERO_EQUIPA)),
                format('~n ---  CONCLUIDO --- ~n'),
                salva(jogo,'database/jogos.bd'),
                salva(jogador,'database/jogadores.bd'),
                pergunta("~n Pressione [Enter]",_),
                A is 0,!.

gamenow_menu_option(2,A):-format('~n FALHA AO ATRIBUIR GOLOS ~n'),
                        pergunta("~n Pressione [Enter]",_),A is 0,!.





carregaDados:- carrega('database/equipas.bd'),
                carrega('database/jogadores.bd'),
                carrega('database/treinadores.bd'),
                carrega('database/jogos.bd'),
                carrega('database/jornadas.bd'),
                carrega('database/total_jornadas.bd'),
                carrega('database/total_equipas.bd'),
                carrega('database/total_jogos.bd'),
                carrega('database/total_jogadores.bd'),
                carrega('database/total_treinadores.bd'),
                carrega('database/winner.bd'),
                carrega('database/best.bd'),
                carrega('helpers.pl'),
                carrega('listing.pl'),
                carrega('validate.pl').
carrega(Arquivo):-
        exists_file(Arquivo),
        consult(Arquivo);true.

