
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