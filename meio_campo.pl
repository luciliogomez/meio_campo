% THE PROJECT STARTS HERE

start:- carrega('database.bd'),
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
                pergunta("Digite a Opcao~n",R),
                R=9,A is 0,!.
open_menu(3,A):- repeat,
                statistics_menu,
                pergunta("Deseja Sair? [s/n]~n",R),
                R=s,A is 0,!.
open_menu(4,1).

open_menu(now,A):- repeat,
                gamenow_menu,
                pergunta("Digite a Opcao~n",R),
                R=5,A is 0, !.

carrega(Arquivo):-
        exists_file(Arquivo),
        consult(Arquivo);true.

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

gamenow_menu:-
    format('~n*** JOGO A DECORRER  *** ~n'),
    format('-------------~n'),
    format('1- INICIAR ~n'),
    format('2- TERMINAR JOGO ~n'),
    format('3- ATRIBUIR GOLOS ~n'),
    format('4- ESTATISTICA DO JOGO~n~n'),
    format('5- SAIR~n~n').

team_menu:-
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
    format('9- SAIR ~n~n').

statistics_menu:-
    format('~n*** ESTATISTICA  *** ~n'),
    format('-------------~n'),
    format('1- TABELA CLASSIFICATIVA ~n'),
    format('2- VENCEDOR ~n'),
    format('3- MELHOR MARCADOR ~n'),
    format('4- SAIR ~n~n').



pergunta(Question,Answer):-
	format(Question),
	gets(Answer).


gets(S):-
	read_line_to_codes(user_input,C),
	name(S,C).