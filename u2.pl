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

% Récupération du temps avant le concert
%
% temps_avant_concert(Minutes)
%
temps_avant_concert(Minutes) :- Minutes = 17.

% Vérification de la contrainte de temps avant le début du concert
%
% a_temps_pour_concert(Temps_total)
%
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
%
max_temps_traversee(BonoTraverse, EdgeTraverse, AdamTraverse, LarryTraverse, MaxTempsTraversee) :-
    ajouter_a_liste_si_different_de_zero(BonoTraverse, 1, [], Liste1),
    ajouter_a_liste_si_different_de_zero(EdgeTraverse, 2, Liste1, Liste2),
    ajouter_a_liste_si_different_de_zero(AdamTraverse, 5, Liste2, Liste3),
    ajouter_a_liste_si_different_de_zero(LarryTraverse, 10, Liste3, Liste4),
    length(Liste4, NbrPersonnes),
    NbrPersonnes =< 2,
    max_list(Liste4, MaxTempsTraversee).

% ajouter_a_liste_si_different_de_zero(PersonneEstPresente,
%                     TempsTraversee,
%                     Liste,
%                     ListeRetour)
%
ajouter_a_liste_si_different_de_zero(PersonneEstPresente, TempsTraversee, Liste, ListeRetour) :-
    PersonneEstPresente == 1 -> append(Liste, [TempsTraversee], ListeRetour) ; ListeRetour = Liste.

%===========================================================================
% Prédicats décrivant les différentes transitions possibles
%===========================================================================

% Une personne monte sur le pont depuis le côté gauche (cas aller)
% Lampe de poche à gauche
%
% transition(EtatDepart,
%            EtatArrivee,
%            Action)
%
transition(etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, gauche, Temps),
           etat(Bfg, Efg, Afg, Lfg, Bd, Ed, Ad, Ld, Bfp, Efp, Afp, Lfp, gauche, Temps),
           monter_pont(I, J, K, L)) :-

    NbrPersonnesSurPont is Bp + Ep + Ap + Lp,
    % on vérifie qu'un prédicat autorise cette action
    monte_sur_le_pont(NbrPersonnesSurPont, I, J, K, L),
    % une personne qui monte sur le pont doit se trouver du côté gauche
    I =< Bg, J =< Eg, K =< Ag, L =< Lg,
    % On calcul la valeur des paramètres de l'état d'arrivée
    Bfg is Bg - I,
    Efg is Eg - J,
    Afg is Ag - K,
    Lfg is Lg - L,
    Bfp is Bp + I,
    Efp is Ep + J,
    Afp is Ap + K,
    Lfp is Lp + L.

% Une personne monte sur le pont depuis le côté droit (cas retour)
% Lampe de poche à droite
%
% transition(EtatDepart,
%            EtatArrivee,
%            Action)
%
transition(etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, droite, Temps),
           etat(Bg, Eg, Ag, Lg, Bfd, Efd, Afd, Lfd, Bfp, Efp, Afp, Lfp, droite, Temps),
           monter_pont(I, J, K, L)) :-

    NbrPersonnesSurPont is Bp + Ep + Ap + Lp,
    % on vérifie qu'un prédicat autorise cette action
    monte_sur_le_pont(NbrPersonnesSurPont, I, J, K, L),
    % une personne qui monte sur le pont doit se trouver du côté droit
    I =< Bd, J =< Ed, K =< Ad, L =< Ld,
    % On calcul la valeur des paramètres de l'état d'arrivée
    Bfd is Bd - I,
    Efd is Ed - J,
    Afd is Ad - K,
    Lfd is Ld - L,
    Bfp is Bp + I,
    Efp is Ep + J,
    Afp is Ap + K,
    Lfp is Lp + L.

% Une personne descend du côté gauche du pont (cas retour)
% Lampe de poche à gauche
%
% transition(EtatDepart,
%            EtatArrivee,
%            Action)
%
transition(etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, gauche, Temps),
           etat(Bfg, Efg, Afg, Lfg, Bd, Ed, Ad, Ld, Bfp, Efp, Afp, Lfp, gauche, Temps),
           descendre_pont(I, J, K, L)) :-

    % on vérifie qu'un prédicat autorise cette action
    descend_du_pont(Bp, Ep, Ap, Lp, I, J, K, L),
    % On calcul la valeur des paramètres de l'état d'arrivée
    Bfg is Bg + I,
    Efg is Eg + J,
    Afg is Ag + K,
    Lfg is Lg + L,
    Bfp is Bp - I,
    Efp is Ep - J,
    Afp is Ap - K,
    Lfp is Lp - L.

% Une personne descend du côté droit du pont (cas aller)
% Lampe de poche à droite
%
% transition(EtatDepart,
%            EtatArrivee,
%            Action)
%
transition(etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, droite, Temps),
           etat(Bg, Eg, Ag, Lg, Bfd, Efd, Afd, Lfd, Bfp, Efp, Afp, Lfp, droite, Temps),
           descendre_pont(I, J, K, L)) :-

    % on vérifie qu'un prédicat autorise cette action
    descend_du_pont(Bp, Ep, Ap, Lp, I, J, K, L),
    % On calcul la valeur des paramètres de l'état d'arrivée
    Bfd is Bd + I,
    Efd is Ed + J,
    Afd is Ad + K,
    Lfd is Ld + L,
    Bfp is Bp - I,
    Efp is Ep - J,
    Afp is Ap - K,
    Lfp is Lp - L.

% Une personne traverse le pont de gauche à droite (cas aller)
%
% transition(EtatDepart,
%            EtatArrivee,
%            Action)
%
transition(etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, gauche, Temps),
           etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, droite, TotalTemps),
           traverser) :-

    % Calcul du temps de la traversée du pont
    max_temps_traversee(Bp, Ep, Ap, Lp, MaxTempsTraversee),
    % On additionne avec le temps déjà écoulé
    TotalTemps is Temps + MaxTempsTraversee,
    % On vérifie qu'on est toujours dans les temps pour le concert
    a_temps_pour_concert(TotalTemps).

% Une personne traverse le pont de droite à gauche (cas retour)
%
% transition(EtatDepart,
%            EtatArrivee,
%            Action)
%
transition(etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, droite, Temps),
           etat(Bg, Eg, Ag, Lg, Bd, Ed, Ad, Ld, Bp, Ep, Ap, Lp, gauche, TotalTemps),
           traverser) :-

    % Calcul du temps de la traversée du pont
    max_temps_traversee(Bp, Ep, Ap, Lp, MaxTempsTraversee),
    % On additionne avec le temps déjà écoulé
    TotalTemps is Temps + MaxTempsTraversee,
    % On vérifie qu'on est toujours dans les temps pour le concert
    a_temps_pour_concert(TotalTemps).

%===========================================================================
% Prédicats utilisés pour afficher les solutions
%===========================================================================

% Affiche le contenu d'une liste
%
% ecrire(Liste)
%
ecrire([]).

ecrire([H|T]) :-
    write(H), nl, ecrire(T).

% Affiche le contenu d'une liste de Transitions
%
% ecrireTransitions(ListeTransitions)
%
ecrireTransitions([]).

ecrireTransitions([H|T]) :-

    ecrire(H), nl, nl,
    ecrireTransitions(T).

%===========================================================================
% Prédicats décrivant la recherche de solutions valides
%===========================================================================

recherche(Etat, Etat, []).

recherche(Init, Final, [
        monter_pont(I, J, K, L),
        traverser,
        descendre_pont(P, Q, R, S)
        |T
    ]) :-

    transition(Init, Aux, monter_pont(I, J, K, L)),
    transition(Aux, Aaux, traverser),
    transition(Aaux, Aaaux, descendre_pont(P, Q, R, S)),
    recherche(Aaaux, Final, T).

%===========================================================================
% Prédicat lancant la recherche de solutions
%===========================================================================

trouver_solutions :-
    setof(
        Transitions,
        recherche(
            etat(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, gauche, 0),
            etat(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, droite, _),
            Transitions),
        SetTransitions),
    nl, ecrireTransitions(SetTransitions).

%===========================================================================
% Prédicat permettant de vérifier si une solution est valide
%===========================================================================

solution_est_valide(Transitions) :-

    temps_avant_concert(Minutes),
    EstValide = recherche(
        etat(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, gauche, 0),
        etat(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, droite, Minutes),
        Transitions),
    EstValide -> (write("\nLa solution est valide!\n"), true) ; (write("\nLa solution n'est pas valide!\n"), false).

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
