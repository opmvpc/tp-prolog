% temps pour traverser le pont
% temps(Personnage, Duree)

temps(bono,1).
temps(edge,2).
temps(adam,5).
temps(larry,10).

% alternatives pour monter sur le pont
% monte(NbPont, Bono, Edge, Adam, Larry)

% Pont vide et 1 personne monte
monter(0,1,0,0,0).
monter(0,0,1,0,0).
monter(0,0,0,1,0).
monter(0,0,0,0,1).

% Pont vide et 2 personnes montes
monter(0,1,1,0,0).
monter(0,1,0,1,0).
monter(0,1,0,0,1).
monter(0,0,1,1,0).
monter(0,0,1,0,1).
monter(0,0,0,1,1).

% 1 personne sur le pont et 1 personne monte
monter(1,1,0,0,0).
monter(1,0,1,0,0).
monter(1,0,0,1,0).
monter(1,0,0,0,1).

% alternateives pour descendre du pont
% descend(  BonoPont, EdgePont, AdamPont,
%           LarryPont, BonoDescend, EdgeDescend,
%           AdamDescend, LarryDescend)

% 1 personne sur le pont et 1 personne descend
descendre(1,0,0,0,1,0,0,0).
descendre(0,1,0,0,0,1,0,0).
descendre(0,0,1,0,0,0,1,0).
descendre(0,0,0,1,0,0,0,1).

% 2 personnes sur le pont et 1 descend
descendre(1,1,0,0,1,1,0,0).
descendre(1,1,0,0,0,1,0,0).
descendre(1,1,0,0,1,0,0,0).

descendre(1,0,1,0,1,0,1,0).
descendre(1,0,1,0,1,0,0,0).
descendre(1,0,1,0,0,0,1,0).

descendre(1,0,0,1,1,0,0,1).
descendre(1,0,0,1,1,0,0,0).
descendre(1,0,0,1,0,0,0,1).

descendre(0,1,1,0,0,1,1,0).
descendre(0,1,1,0,0,1,0,0).
descendre(0,1,1,0,0,0,1,0).

descendre(0,1,0,1,0,1,0,1).
descendre(0,1,0,1,0,1,0,0).
descendre(0,1,0,1,0,0,0,1).

descendre(0,0,1,1,0,0,1,1).
descendre(0,0,1,1,0,0,1,0).
descendre(0,0,1,1,0,0,0,1).

% Calcule le temps de la traversée

max_temps(I,J,K,L,T) :-
    L == 1,
    R1 is I + J,
    R2 is K + L,
    R3 is R1 + R2,
    R3 =< 2,
    temps(larry,T).
max_temps(I,J,K,L,T) :-
    L == 0,
    K == 1,
    R1 is I + J,
    R2 is K + L,
    R3 is R1 + R2,
    R3 =< 2,
    temps(adam,T).
max_temps(I,J,K,L,T) :-
    L == 0,
    K == 0,
    J == 1,
    R1 is I + J,
    R2 is K + L,
    R3 is R1 + R2,
    R3 =< 2,
    temps(edge,T).
max_temps(I,J,K,L,T) :-
    L == 0,
    K == 0,
    J == 0,
    I == 1,
    temps(bono,T).

% Verification du retard au concert

retard(T) :- T =< 17.

% transitions
% Monter sur le pont
transition( etat(Bg,Eg,Ag,Lg,Bd,Ed,Ad,Ld,Bp,Ep,Ap,Lp,gauche,T),
            etat(Bfg,Efg,Afg,Lfg,Bd,Ed,Ad,Ld,Bfp,Efp,Afp,Lfp,gauche,T),
            aller_pont(I,J,K,L) ) :-
                Nb1 is Bp + Ep,
                Nb2 is Ap + Lp,
                Nb is Nb1 + Nb2,
                monter(Nb,I,J,K,L),
                I =< Bg, J =< Eg, K =< Ag, L =< Lg,
                Bfg is Bg - I,
                Efg is Eg - J,
                Afg is Ag - K,
                Lfg is Lg - L,
                Bfp is Bp + I,
                Efp is Ep + J,
                Afp is Ap + K,
                Lfp is Lp + L.

transition( etat(Bg,Eg,Ag,Lg,Bd,Ed,Ad,Ld,Bp,Ep,Ap,Lp,droite,T),
            etat(Bg,Eg,Ag,Lg,Bfd,Efd,Afd,Lfd,Bfp,Efp,Afp,Lfp,droite,T),
            aller_pont(I,J,K,L) ) :-
                Nb1 is Bp + Ep,
                Nb2 is Ap + Lp,
                Nb is Nb1 + Nb2,
                monter(Nb,I,J,K,L),
                I =< Bd, J =< Ed, K =< Ad, L =< Ld,
                Bfd is Bd - I,
                Efd is Ed - J,
                Afd is Ad - K,
                Lfd is Ld - L,
                Bfp is Bp + I,
                Efp is Ep + J,
                Afp is Ap + K,
                Lfp is Lp + L.

% Descendre du pont
transition( etat(Bg,Eg,Ag,Lg,Bd,Ed,Ad,Ld,Bp,Ep,Ap,Lp,gauche,T),
            etat(Bfg,Efg,Afg,Lfg,Bd,Ed,Ad,Ld,Bfp,Efp,Afp,Lfp,gauche,T),
            quitter_pont(I,J,K,L) ) :-
                descendre(Bp,Ep,Ap,Lp,I,J,K,L),
                Bfg is Bg + I,
                Efg is Eg + J,
                Afg is Ag + K,
                Lfg is Lg + L,
                Bfp is Bp - I,
                Efp is Ep - J,
                Afp is Ap - K,
                Lfp is Lp - L.

transition( etat(Bg,Eg,Ag,Lg,Bd,Ed,Ad,Ld,Bp,Ep,Ap,Lp,droite,T),
            etat(Bg,Eg,Ag,Lg,Bfd,Efd,Afd,Lfd,Bfp,Efp,Afp,Lfp,droite,T),
            quitter_pont(I,J,K,L) ) :-
                descendre(Bp,Ep,Ap,Lp,I,J,K,L),
                Bfd is Bd + I,
                Efd is Ed + J,
                Afd is Ad + K,
                Lfd is Ld + L,
                Bfp is Bp - I,
                Efp is Ep - J,
                Afp is Ap - K,
                Lfp is Lp - L.

% Traverser le pont
transition( etat(Bg,Eg,Ag,Lg,Bd,Ed,Ad,Ld,Bp,Ep,Ap,Lp,gauche,T),
            etat(Bg,Eg,Ag,Lg,Bd,Ed,Ad,Ld,Bp,Ep,Ap,Lp,droite,Tf),
            traverser ) :-
                Nb1 is Bp + Ep,
                Nb2 is Ap + Lp,
                Nb is Nb1 + Nb2,
                1 =< Nb,
                Nb =< 2,
                max_temps(Bp,Ep,Ap,Lp,X),
                Tf is T + X,
                retard(Tf).

transition( etat(Bg,Eg,Ag,Lg,Bd,Ed,Ad,Ld,Bp,Ep,Ap,Lp,droite,T),
            etat(Bg,Eg,Ag,Lg,Bd,Ed,Ad,Ld,Bp,Ep,Ap,Lp,gauche,Tf),
            traverser ) :-
                Nb1 is Bp + Ep,
                Nb2 is Ap + Lp,
                Nb is Nb1 + Nb2,
                1 =< Nb,
                Nb =< 2,
                max_temps(Bp,Ep,Ap,Lp,X),
                Tf is T + X,
                retard(Tf).

% Proposition de résolution au problème

rech(Etat,Etat,[]).
rech(Initial,Final,[Op|Trans]) :-
         transition(Initial,Tmp,Op),
         rech(Tmp,Final,Trans).

m :- Transitions =
       [ aller_pont(1,1,0,0), traverser, quitter_pont(1,1,0,0),
         aller_pont(1,0,0,0), traverser, quitter_pont(1,0,0,0),
         aller_pont(0,0,1,1), traverser, quitter_pont(0,0,1,1),
         aller_pont(0,1,0,0), traverser, quitter_pont(0,1,0,0),
         aller_pont(1,1,0,0), traverser, quitter_pont(1,1,0,0) ],
     rech( etat(1,1,1,1,0,0,0,0,0,0,0,0,gauche,0),
           etat(0,0,0,0,1,1,1,1,0,0,0,0,droite,17),
           Transitions ).

% Affichage des traversées

ecrire([]).
ecrire([H|T]) :- write(H), nl, ecrire(T).

% Recherche des solutions

recherche(Etat,Etat,[]).
recherche(Init,Final,[aller_pont(I,J,K,L),
               traverser,quitter_pont(P,Q,R,S)]) :-
                    transition(Init,Tmp1,aller_pont(I,J,K,L)),
                    transition(Tmp1,Tmp2,traverser),
                    transition(Tmp2,Final,quitter_pont(P,Q,R,S)).
recherche(Init,Final,[aller_pont(I,J,K,L),
               traverser,quitter_pont(P,Q,R,S)|Trav]) :-
                    transition(Init,Tmp1,aller_pont(I,J,K,L)),
                    transition(Tmp1,Tmp2,traverser),
                    transition(Tmp2,Tmp3,quitter_pont(P,Q,R,S)),
                    recherche(Tmp3,Final,Trav).

% Point d'entrée du test

g :- recherche( etat(1,1,1,1,0,0,0,0,0,0,0,0,gauche,0),
                etat(0,0,0,0,1,1,1,1,0,0,0,0,droite,X),
                Transitions ),
                ecrire(Transitions),
                write('La traversee s\'est faite en : '),
                write(X),
                write(' minutes.').
