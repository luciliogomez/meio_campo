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
    format('6- LISTAR UM RESULTADO ~n~n'),
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


atualiza_total_jornadas(TOTAL):-
                                N is TOTAL+1,
                                retract(total_jornadas(TOTAL)),
                                assertz(total_jornadas(N)).

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

                            





carregaDados:- carrega('equipas.bd'),
                carrega('jogadores.bd'),
                carrega('treinadores.bd'),
                carrega('jogos.bd'),
                carrega('jornadas.bd'),
                carrega('total_jornadas.bd'),
                carrega('total_equipas.bd'),
                carrega('menus.pl'),
                carrega('helpers.pl'),
                carrega('validate.pl').
carrega(Arquivo):-
        exists_file(Arquivo),
        consult(Arquivo);true.

