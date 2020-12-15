%==============================================================================
% Prédicats représentant les différentes situations de traversée du pont
% en individualisant les différents membres du groupe U2 puisqu'ils
% ne sont pas équivalents (pas le même temps_traversee_par_personne de traversée)
%==============================================================================

% monte_sur_le_pont(Nb_presents_sur_le_pont,
%                   Bono_monte,
%                   Edge_monte,
%                   Adam_monte,
%                   Larry_monte)

% pont vide et une personne monte dessus
monte_sur_le_pont(0, 1, 0, 0, 0).
monte_sur_le_pont(0, 0, 1, 0, 0).
monte_sur_le_pont(0, 0, 0, 1, 0).
monte_sur_le_pont(0, 0, 0, 0, 1).

% pont vide et deux personnes montent dessus
monte_sur_le_pont(0, 1, 1, 0, 0).
monte_sur_le_pont(0, 1, 0, 1, 0).
monte_sur_le_pont(0, 1, 0, 0, 1).
monte_sur_le_pont(0, 0, 1, 1, 0).
monte_sur_le_pont(0, 0, 1, 0, 1).
monte_sur_le_pont(0, 0, 0, 1, 1).

% une personne sur le pont et une personne monte dessus
monte_sur_le_pont(1, 1, 0, 0, 0).
monte_sur_le_pont(1, 0, 1, 0, 0).
monte_sur_le_pont(1, 0, 0, 1, 0).
monte_sur_le_pont(1, 0, 0, 0, 1).

% deux personnes, pas possible de faire monter quelqu'un de plus
monte_sur_le_pont(2, 0, 0, 0, 0).

% descend_du_pont(Bono_est_sur_le_pont,
%                 Edge_est_sur_le_pont,
%                 Adam_est_sur_le_pont,
%                 Larry_est_sur_le_pont,
%                 Bono_descend,
%                 Edge_descend,
%                 Adam_descend,
%                 Larry_descend)

% personne sur le pont
descend_du_pont(0, 0, 0, 0, 0, 0, 0, 0).

% une personne sur le pont et en descend
descend_du_pont(1, 0, 0, 0, 1, 0, 0, 0).
descend_du_pont(0, 1, 0, 0, 0, 1, 0, 0).
descend_du_pont(0, 0, 1, 0, 0, 0, 1, 0).
descend_du_pont(0, 0, 0, 1, 0, 0, 0, 1).

% deux personnes sur le pont et une personne en descend
descend_du_pont(1, 1, 0, 0, 1, 0, 0, 0).
descend_du_pont(1, 1, 0, 0, 0, 1, 0, 0).
descend_du_pont(1, 0, 1, 0, 1, 0, 0, 0).
descend_du_pont(1, 0, 1, 0, 0, 0, 1, 0).
descend_du_pont(1, 0, 0, 1, 1, 0, 0, 0).
descend_du_pont(1, 0, 0, 1, 0, 0, 0, 1).
descend_du_pont(0, 1, 1, 0, 0, 1, 0, 0).
descend_du_pont(0, 1, 1, 0, 0, 0, 1, 0).
descend_du_pont(0, 1, 0, 1, 0, 1, 0, 0).
descend_du_pont(0, 1, 0, 1, 0, 0, 0, 1).
descend_du_pont(0, 0, 1, 1, 0, 0, 1, 0).
descend_du_pont(0, 0, 1, 1, 0, 0, 0, 1).

% deux personnes sur le pont et deux personnes en descendent
descend_du_pont(1, 1, 0, 0, 1, 1, 0, 0).
descend_du_pont(1, 0, 1, 0, 1, 0, 1, 0).
descend_du_pont(1, 0, 0, 1, 1, 0, 0, 1).
descend_du_pont(0, 1, 1, 0, 0, 1, 1, 0).
descend_du_pont(0, 1, 0, 1, 0, 1, 0, 1).
descend_du_pont(0, 0, 1, 1, 0, 0, 1, 1).


%===========================================================================
% Prédicats permettant de vérifier si la contrainte de temps est respectée
%===========================================================================

temps_avant_concert(Minutes) :- Minutes = 17.

% Vérification de la contrainte de temps avant le début du concert
%
% a_temps_pour_concert(Temps_total)

a_temps_pour_concert(Temps_total) :-
    temps_avant_concert(Minutes),
    Temps_total =< Minutes.

% Calcul du temps d'une traversée du pont
%
% max_temps_traversee(BonoTraverse,
%                     EdgeTraverse,
%                     AdamTraverse,
%                     LarryTraverse,
%                     MaxTempsTraversee)

max_temps_traversee(BonoTraverse, EdgeTraverse, AdamTraverse, LarryTraverse, MaxTempsTraversee) :-
    ajouter_a_liste_si_different_de_zero(BonoTraverse, 1, [], Liste1),
    ajouter_a_liste_si_different_de_zero(EdgeTraverse, 2, Liste1, Liste2),
    ajouter_a_liste_si_different_de_zero(AdamTraverse, 5, Liste2, Liste3),
    ajouter_a_liste_si_different_de_zero(LarryTraverse, 10, Liste3, Liste4),
    length(Liste4, NbrPersonnes),
    NbrPersonnes =< 2,
    max_list(Liste4, MaxTempsTraversee).

ajouter_a_liste_si_different_de_zero(PersonneEstPresente, TempsTraversee, Liste, ListeRetour) :-
    PersonneEstPresente == 1 -> append(Liste, [TempsTraversee], ListeRetour) ; ListeRetour = Liste.

%===========================================================================
% Prédicats décrivant les différentes transitions possibles
%===========================================================================

% Une personne monte sur le pont
% 2 cas: Lampe de poche à gauche ou à droite
%
% transition(EtatDepart,
%            EtatArrivee,
%            MonterPont)
transition( etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, gauche, T),
            etat(Bfg, Efg, Afg, Lfg, Bd, Ed, Ad, Ld, Bfp, Efp, Afp, Lfp, gauche, T),
            monter_pont(I, J, K, L) ) :-

    Nb1 is Bp + Ep,
    Nb2 is Ap + Lp,
    Nb is Nb1 + Nb2,
    monte_sur_le_pont(Nb, I, J, K, L),
    I =< Bg, J =< Eg, K =< Ag, L =< Lg,
    Bfg is Bg - I,
    Efg is Eg - J,
    Afg is Ag - K,
    Lfg is Lg - L,
    Bfp is Bp + I,
    Efp is Ep + J,
    Afp is Ap + K,
    Lfp is Lp + L.

transition( etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, droite, T),
            etat(Bg, Eg, Ag, Lg, Bfd, Efd, Afd, Lfd, Bfp, Efp, Afp, Lfp, droite, T),
            monter_pont(I, J, K, L) ) :-
                Nb1 is Bp + Ep,
                Nb2 is Ap + Lp,
                Nb is Nb1 + Nb2,
                monte_sur_le_pont(Nb, I, J, K, L),
                I =< Bd, J =< Ed, K =< Ad, L =< Ld,
                Bfd is Bd - I,
                Efd is Ed - J,
                Afd is Ad - K,
                Lfd is Ld - L,
                Bfp is Bp + I,
                Efp is Ep + J,
                Afp is Ap + K,
                Lfp is Lp + L.

% descend_du_pont du pont
transition( etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, gauche, T),
            etat(Bfg, Efg, Afg, Lfg, Bd, Ed, Ad, Ld, Bfp, Efp, Afp, Lfp, gauche, T),
            descendre_pont(I, J, K, L) ) :-
                descend_du_pont(Bp, Ep, Ap, Lp, I, J, K, L),
                Bfg is Bg + I,
                Efg is Eg + J,
                Afg is Ag + K,
                Lfg is Lg + L,
                Bfp is Bp - I,
                Efp is Ep - J,
                Afp is Ap - K,
                Lfp is Lp - L.

transition( etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, droite, T),
            etat(Bg, Eg, Ag, Lg, Bfd, Efd, Afd, Lfd, Bfp, Efp, Afp, Lfp, droite, T),
            descendre_pont(I, J, K, L) ) :-
                descend_du_pont(Bp, Ep, Ap, Lp, I, J, K, L),
                Bfd is Bd + I,
                Efd is Ed + J,
                Afd is Ad + K,
                Lfd is Ld + L,
                Bfp is Bp - I,
                Efp is Ep - J,
                Afp is Ap - K,
                Lfp is Lp - L.

% Traverser le pont
transition( etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, gauche, T),
            etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, droite, Tf),
            traverser ) :-
                Nb1 is Bp + Ep,
                Nb2 is Ap + Lp,
                Nb is Nb1 + Nb2,
                1 =< Nb,
                Nb =< 2,
                max_temps_traversee(Bp, Ep, Ap, Lp, X),
                Tf is T + X,
                a_temps_pour_concert(Tf).

transition( etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, droite, T),
            etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, gauche, Tf),
            traverser ) :-
                Nb1 is Bp + Ep,
                Nb2 is Ap + Lp,
                Nb is Nb1 + Nb2,
                1 =< Nb,
                Nb =< 2,
                max_temps_traversee(Bp, Ep, Ap, Lp, X),
                Tf is T + X,
                a_temps_pour_concert(Tf).

% Affichage
ecrire([]).
ecrire([H|T]) :- write(H), nl, ecrire(T).

ecrireTransitions([]).
ecrireTransitions([H|T]) :- ecrire(H), nl, nl, ecrireTransitions(T).

% Recherche des solutions
recherche(Etat, Etat, []).
recherche(Init, Final, [monter_pont(I, J, K, L),
               traverser, descendre_pont(P, Q, R, S)]) :-
    transition(Init, Tmp1, monter_pont(I, J, K, L)),
    transition(Tmp1, Tmp2, traverser),
    transition(Tmp2, Final, descendre_pont(P, Q, R, S)).
recherche(Init, Final, [monter_pont(I, J, K, L),
               traverser, descendre_pont(P, Q, R, S)|Trav]) :-
    transition(Init, Tmp1, monter_pont(I, J, K, L)),
    transition(Tmp1, Tmp2, traverser),
    transition(Tmp2, Tmp3, descendre_pont(P, Q, R, S)),
    recherche(Tmp3, Final, Trav).

% Point d'entrée du test
trouver_solutions :- setof(Transitions, recherche( etat(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, gauche, 0),
    etat(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, droite, X),
    Transitions ), SetTransitions),
    ecrireTransitions(SetTransitions),
    write('La traversee s\'est faite en : '),
    write(X),
    write(' minutes.\n\n').

solution_est_valide(Transitions) :-
    temps_avant_concert(Minutes),
    EstValide = recherche(etat(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, gauche, 0),
        etat(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, droite, Minutes),
    Transitions),
    EstValide -> (write("La solution est valide!"), true) ; (write("La solution n'est pas valide!"), false).

exemple_solution_est_valide :-
    solution_est_valide([
        monter_pont(1,1,0,0),
        traverser,
        descendre_pont(1,1,0,0),
        monter_pont(1,0,0,0),
        traverser,
        descendre_pont(1,0,0,0),
        monter_pont(0,0,1,1),
        traverser,
        descendre_pont(0,0,1,1),
        monter_pont(0,1,0,0),
        traverser,
        descendre_pont(0,1,0,0),
        monter_pont(1,1,0,0),
        traverser,
        descendre_pont(1,1,0,0)
    ]).
