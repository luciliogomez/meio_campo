% THE PROJECT STARTS HERE

start:- carregaDados,
        repeat,
        main_menu,
        pergunta("Digite a Opcao~n",R),
        open_menu(R,A),
        A=1,!.

open_menu(1,A):- repeat,
                game_menu,
                pergunta("Digite a Opcao~n",R),
                R=5,A is 0,!.
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
                format('~n ---  EQUIPA ADICIONADA --- ~n'),
                salva(equipa,'equipas.bd'),
                pergunta("~n Pressione [Enter]",B),
                A is 0,!.

team_menu_option(2,A):-format('~n *** CADASTRAR JOGADOR *** ~n'),
                pergunta("Numero Da Equipa:~n",NUMERO_TEAM),
                team_exists(NUMERO_TEAM),
                pergunta("Numero do Jogador:~n",NUMERO_PLAYER),
                pergunta("Nome:~n",NOME),
                pergunta("Idade:~n",IDADE),
                pergunta("Altura:~n",ALTURA),
                pergunta("Peso:~n",PESO),
                pergunta("Genero:~n",GENERO),
                pergunta("Posicao:~n",POSICAO),
                pergunta("Numero de Golos:~n",GOLOS),
                assertz(jogador(NUMERO_PLAYER,NOME,IDADE,ALTURA,PESO,GENERO,POSICAO,GOLOS,NUMERO_TEAM)),
                format('~n ---  JOGADOR ADICIONADO --- ~n'),
                salva(jogador,'jogadores.bd'),
                pergunta("~n Pressione [Enter]",B),
                A is 0,!.

team_menu_option(2,A):-format('~n --- FALHA AO CADASTRAR --- ~n'),
                    pergunta("~n Pressione [Enter]",B),
                    A is 0,!.


team_menu_option(4,A):-format('~n *** LISTA DE EQUIPAS *** ~n'),
                    listarEquipas,
                    A is 0,!.
team_menu_option(4,A):-pergunta("Pressione [Enter]",B),
                    A is 0,!.

team_menu_option(5,A):-format('~n *** LISTA DE JOGADORES *** ~n'),
                    listarJogadores,
                    A is 0,!.
team_menu_option(5,A):-pergunta("~nPressione [Enter]",B),
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
    format('~n*** JOGOS  *** ~n'),
    format('-------------~n'),
    format('1- CADASTRAR JOGOS ~n'),
    format('2- JOGO A DECORRER ~n'),
    format('3- LISTAR TODOS RESULTADOS ~n'),
    format('4- LISTAR UM RESULTADO ~n~n'),
    format('5- SAIR ~n~n').



% --- LISTAGENS ---
listarEquipas:- equipa(NU,NO,FU,TI),
                format('~n[ ~w - ~w - ~w - ~w ]~n',[NU,NO,FU,TI]),fail.

listarJogadores:- jogador(NUM_P,NOM_P,AGE_P,ALT_P,PESO_P,GEN_P,POS_P,GOL_P,TEAM), 
                equipa(TEAM,NOM_T,_,_),
                format('~n[ ~w - ~w - ~w - ~w - ~w - ~w - ~w - ~w - ~w]~n',[NUM_P,NOM_P,AGE_P,ALT_P,PESO_P,GEN_P,POS_P,GOL_P,NOM_T]),fail.
                    






carregaDados:- carrega('equipas.bd'),
                carrega('jogadores.bd'),
                carrega('treinadores.bd'),
                carrega('jogos.bd'),
                carrega('jornadas.bd'),
                carrega('menus.pl'),
                carrega('helpers.pl'),
                carrega('validate.pl').
carrega(Arquivo):-
        exists_file(Arquivo),
        consult(Arquivo);true.

