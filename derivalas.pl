% Portik Ábel paim1949
% 523. csoport
%
% O.2.1.Szimbolikus deriválás egyszerûsítése Prolog-ban:
% Feladat, hogy írjátok meg az egyszerûsítés szabályait a polinomok osztályára.
%
% Hasznalat:
% d(Kif,DKif). <=> DKif <- Kif'


%----------------------
% derivalasi szabalyok
%----------------------


% x' = 1
der(x,1):-!.

% c' = 0
der(C,0):-
    atomic(C).

% (f+g)' = f' + g'
der(F+G,DF+DG):-
    der(F,DF),
    der(G,DG).

% (f-g)' = f' - g'
der(F-G,DF-DG):-
    der(F,DF),
    der(G,DG).


% (f*g)' = f'*g + f*g'
der(F*G,DF*G+F*DG):-
    der(F,DF),
    der(G,DG).

% (f^n)' = n*f^(n-1)*f'
der(F^N,N*F^N1*DF):-
    der(F,DF),
    N1 is N-1.

%-------------------
% egyszerusites
%-------------------

egysz(Pol,EgyszPol):-
    atalakit(Pol, LP),
    osszevon(LP,OsszevontLP),
    elhagy_nulla(OsszevontLP, NincsNullaLP),
    sort(2, @>=, NincsNullaLP, L),
    visszaalakit(L, EgyszPol).

%-----------
% derivalas
%-----------

d(F,DF):-
    der(F,Df),
    egysz(Df,DF),
    !.

%-----------------------------------------------------
% polinom listava alakitasa
% [a,n] alaku lista elemek
% a polinom = SUMMA(a*x^n), minden [a,n] lista elembol
%-----------------------------------------------------

atalakit(x,[[1,1]]):- !.
atalakit(-x,[[-1,1]]):- !.
atalakit(C, [[C,0]]):-
    atomic(C), !.
atalakit(A+B,Ossz):-
    atalakit(A,A1),
    atalakit(B,B1),
    append(A1,B1,Ossz), !.
atalakit(A-B,Kul):-
    atalakit(A,A1),
    atalakit(-B,B1),
    append(A1,B1,Kul), !.
atalakit(-(A),NegA):-
    atalakit((-1)*A,NegA), !.
atalakit(A*B,Szor):-
    atalakit(A,A1),
    atalakit(B,B1),
    szoroz(A1,B1,Szor), !.
atalakit(A^N,Hatv):-
    atalakit(A,A1),
    hatvanyoz(A1,N,Hatv), !.

%---------------------------------------------------------------------
% lista alaku polinomok szorzata
%
% egy elemu polinom * lista polinom:
% [a0,n0] * [[a1,n1]...[ar,nr]] -> [[a*a1,n+n1]...[a*ar,n+nr]]
%
% lista polinom * lista polinom:
% elso polinom elemeit sorra osszeszorozzuk a masodik polinommal
% kapott polinomok osszege (egymas utan fuzott lista pol.) -> eredmeny
%----------------------------------------------------------------------


szor1(_,[],[]):- !.
szor1([A0,N0],[[Ai,Ni]|Marad],[[A,N]|MaradSzor]):-
    A is A0*Ai,
    N is N0+Ni,
    szor1([A0,N0],Marad,MaradSzor).

szoroz([],_,[]):- !.
szoroz([H|Marad],ListaPol,P):-
    szor1(H,ListaPol,P1),
    szoroz(Marad,ListaPol,P2),
    append(P1,P2,P).

%----------------------------------------------------------------
% lista alaku polinom hatvanyozasa
%
% LP^N = LP^(N-1) * LP = ... = LP * LP * ... * LP
% rekurzivan szamolunk a polinom onmagaval valo osszeszorzasabol
% ---------------------------------------------------------------


hatvanyoz(_,0,[[1,0]]):- !.
hatvanyoz(LP,1,LP):- !.
hatvanyoz(LP,N,LPN):-
    N1 is N-1,
    hatvanyoz(LP,N1,LPN1),
    szoroz(LP,LPN1,LPN).

%----------------------------------------------------------------
% osszevonas
%
% [a,n] egyeduli n-ed foku tag -> marad a lista
%
% [a0,n] n-ed foku tag es van a listaban egy masik
% [ai,n] n-ed foku tag -> a ket tag helyett [a0+ai,n] a listaban
% ----------------------------------------------------------------

osszevon([],[]):- !.
osszevon([[A,N]|Marad],[[A,N]|MaradOsszevont]):-
    nincs_tobb_N(N,Marad),
    osszevon(Marad,MaradOsszevont), !.
osszevon([[A0,N]|Marad],MaradOsszevont):-
    append(Eleje,[[Ai,N]|Vege],Marad),
    A is A0+Ai,
    append(Eleje,[[A,N]|Vege],OsszevontN),
    osszevon(OsszevontN,MaradOsszevont).

nincs_tobb_N(_,[]):- !.
nincs_tobb_N(_,[[]]):- !.
nincs_tobb_N(N,[[_,N1]|Marad]):-
    N \= N1,
    nincs_tobb_N(N,Marad).


%------------------------------------------------------------------
% nullas egyutthatojuk tagok elhagyasa
%
% [0,n] -> 0*x^n = 0 =>
% => [0,n] tagok elhagyhatoak, mivel nem befolyasoljak az osszeget
% -----------------------------------------------------------------

elhagy_nulla([],[]):- !.
elhagy_nulla([[0,_]|Marad],Marad1):-
    elhagy_nulla(Marad,Marad1), !.
elhagy_nulla([[A,N]|Marad],[[A,N]|Marad1]):-
    elhagy_nulla(Marad,Marad1).

%---------------------------------------
% lista alaku polinom vissza alakitasa
%
% [a,n] -> a*x^n
% alaku tagok osszege
% --------------------------------------

% egy elemu listak:
visszaalakit([[A,0]],A):- !.

visszaalakit([[1,1]],x):- !.
visszaalakit([[-1,1]],-x):- !.

visszaalakit([[A,1]],A*x):- !.

visszaalakit([[1,N]],x^N):- !.
visszaalakit([[-1,N]],-x^N):- !.

visszaalakit([[A,N]],A*x^N):- !.

% tobb elem eseten:
%
% utolso tag levalasztasa
% +
% rekurzivan a lista elejet atalakitjuk

% pozitiv egyutthatonal:
visszaalakit(Pol,Eleje+Vege):-
    append(PolEleje,[[A,N]],Pol),
    A > 0,
    visszaalakit(PolEleje,Eleje),
    visszaalakit([[A,N]],Vege).
% negativ egyutthatonal:
visszaalakit(Pol,Eleje-Vege):-
    append(PolEleje,[[A,N]],Pol),
    A < 0,
    MinusA is (-1)*A,
    visszaalakit(PolEleje,Eleje),
    visszaalakit([[MinusA,N]],Vege).
