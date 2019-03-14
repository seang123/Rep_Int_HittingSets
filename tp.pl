%% This file consists of three parts:
%% 	(1) Leancop 2.1 
%%      (2) ncDP: A Non-Clausal Form Decision Procedure
%%      (3) A simple interface tp/4 that uses leancop and ncdp to compute
%%          conflict sets
%%
%% This file is provided for your convenience, but you may as well
%% use the original software.
%%
%% Leancop and ncDP are copyright Jens Otten (see www.leancop.de).
%%
%% For documentation of these tools, we refer to the webpage above.
%%

:- op(1130, xfy, <=>). % equivalence
:- op(1110, xfy, =>).  % implication
%                      % disjunction (;)
%                      % conjunction (,)
:- op( 500, fy, ~).    % negation
:- op( 500, fy, all).  % universal quantifier
:- op( 500, fy, ex).   % existential quantifier
:- op( 500,xfy, :).


% ------------------------------------------------------------------
%  make_matrix(+Fml,-Matrix,+Settings)
%    -  transform first-order formula into set of clauses (matrix)
%
%  Fml, Matrix: first-order formula and matrix
%
%  Settings: list of settings, which can contain def, nodef and conj;
%            if it contains nodef/def, no definitional transformation
%            or a complete definitional transformation is done,
%            otherwise a definitional transformation is done for
%            the conjecture and the standard transformation is done
%            for the axioms; conjecture is marked if conj is given
%
%  Syntax of Fml: negation '~', disjunction ';', conjunction ',',
%      implication '=>', equivalence '<=>', universal/existential
%      quantifier 'all X:<Formula>'/'ex X:<Formula>' where 'X' is a
%      Prolog variable, and atomic formulae are Prolog atoms.
%
%  Example: make_matrix(ex Y:(all X:((p(Y) => p(X)))),Matrix,[]).
%           Matrix = [[-(p(X1))], [p(1 ^ [X1])]]

make_matrix(Fml,Matrix,Set) :-
    univar(Fml,[],F1),
    ( member(conj,Set), F1=(A=>C) -> F2=((A,#)=>(#,C)) ; F2=F1 ),
    ( member(nodef,Set) ->
       def_nnf(F2,NNF,1,_,nnf), dnf(NNF,DNF)
       ;
       \+member(def,Set), F2=(B=>D) ->
        def_nnf(~(B),NNF,1,I,nnf), dnf(NNF,DNF1),
        def_nnf(D,DNF2,I,_,def), DNF=(DNF2;DNF1)
        ;
        def_nnf(F2,DNF,1,_,def)
    ),
    mat(DNF,M),
    ( member(reo(I),Set) -> mreorder(M,Matrix,I) ; Matrix=M ).

% ------------------------------------------------------------------
%  def_nnf(+Fml,-DEF)  -  transform formula into a definitional
%                         Skolemized negation normal form (DEF)
%  Fml, DEF: first-order formula and formula in DEF
%
%  Example: def_nnf(ex Y:(all X:((p(Y) => p(X)))),DEF,def).
%           DEF = ~ p(X1) ; p(1 ^ [X1])

def_nnf(Fml,DEF,I,I1,Set) :-
    def(Fml,[],NNF,DEF1,_,I,I1,Set), def(DEF1,NNF,DEF).

def([],Fml,Fml).
def([(A,(B;C))|DefL],DEF,Fml) :- !, def([(A,B),(A,C)|DefL],DEF,Fml).
def([A|DefL],DEF,Fml) :- def(DefL,(A;DEF),Fml).

def(Fml,FreeV,NNF,DEF,Paths,I,I1,Set) :-
    ( Fml = ~(~A)      -> Fml1 = A;
      Fml = ~(all X:F) -> Fml1 = (ex X: ~F);
      Fml = ~(ex X:F)  -> Fml1 = (all X: ~F);
      Fml = ~((A ; B)) -> Fml1 = ((~A , ~B));
      Fml = ~((A , B)) -> Fml1 = (~A ; ~B);
      Fml = (A => B)   -> Fml1 = (~A ; B);
      Fml = ~((A => B))-> Fml1 = ((A , ~B));
      Fml = (A <=> B)  ->
      ( Set=def        -> Fml1 = ((A => B) , (B => A));
                          Fml1 = ((A , B) ; (~A , ~B)) );
      Fml = ~((A<=>B)) -> Fml1 = ((A , ~B) ; (~A , B)) ), !,
    def(Fml1,FreeV,NNF,DEF,Paths,I,I1,Set).

def((ex X:F),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    def(F,[X|FreeV],NNF,DEF,Paths,I,I1,Set).

def((all X:Fml),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    copy_term((X,Fml,FreeV),((I^FreeV),Fml1,FreeV)), I2 is I+1,
    def(Fml1,FreeV,NNF,DEF,Paths,I2,I1,Set).

def((A ; B),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    def(A,FreeV,NNF1,DEF1,Paths1,I,I2,Set),
    def(B,FreeV,NNF2,DEF2,Paths2,I2,I1,Set),
    append(DEF1,DEF2,DEF), Paths is Paths1 * Paths2,
    (Paths1 > Paths2 -> NNF = (NNF2;NNF1);
                        NNF = (NNF1;NNF2)).

def((A , B),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    def(A,FreeV,NNF3,DEF3,Paths1,I,I2,Set),
    ( NNF3=(_;_), Set=def -> append([(~I2^FreeV,NNF3)],DEF3,DEF1),
                             NNF1=I2^FreeV, I3 is I2+1 ;
                             DEF1=DEF3, NNF1=NNF3, I3 is I2 ),
    def(B,FreeV,NNF4,DEF4,Paths2,I3,I4,Set),
    ( NNF4=(_;_), Set=def -> append([(~I4^FreeV,NNF4)],DEF4,DEF2),
                             NNF2=I4^FreeV, I1 is I4+1 ;
                             DEF2=DEF4, NNF2=NNF4, I1 is I4 ),
    append(DEF1,DEF2,DEF), Paths is Paths1 + Paths2,
    (Paths1 > Paths2 -> NNF = (NNF2,NNF1);
                        NNF = (NNF1,NNF2)).

def(Lit,_,Lit,[],1,I,I,_).

% ------------------------------------------------------------------
%  dnf(+NNF,-DNF)  -  transform formula in NNF into formula in DNF
%  NNF, DNF: formulae in NNF and DNF
%
%  Example: dnf(((p;~p),(q;~q)),DNF).
%           DNF = (p, q ; p, ~ q) ; ~ p, q ; ~ p, ~ q

dnf(((A;B),C),(F1;F2)) :- !, dnf((A,C),F1), dnf((B,C),F2).
dnf((A,(B;C)),(F1;F2)) :- !, dnf((A,B),F1), dnf((A,C),F2).
dnf((A,B),F) :- !, dnf(A,A1), dnf(B,B1),
    ( (A1=(C;D);B1=(C;D)) -> dnf((A1,B1),F) ; F=(A1,B1) ).
dnf((A;B),(A1;B1)) :- !, dnf(A,A1), dnf(B,B1).
dnf(Lit,Lit).

% ------------------------------------------------------------------
%  mat(+DNF,-Matrix)  -  transform formula in DNF into matrix
%  DNF, Matrix: formula in DNF, matrix
%
%  Example: mat(((p, q ; p, ~ q) ; ~ p, q ; ~ p, ~ q),Matrix).
%           Matrix = [[p, q], [p, -(q)], [-(p), q], [-(p), -(q)]]

mat((A;B),M) :- !, mat(A,MA), mat(B,MB), append(MA,MB,M).
mat((A,B),M) :- !, (mat(A,[CA]),mat(B,[CB]) -> union2(CA,CB,M);M=[]).
mat(~Lit,[[-Lit]]) :- !.
mat(Lit,[[Lit]]).

% ------------------------------------------------------------------
%  univar(+Fml,[],-Fml1)  -  rename variables
%  Fml, Fml1: first-order formulae
%
%  Example: univar((all X:(p(X) => (ex X:p(X)))),[],F1).
%           F1 = all Y : (p(Y) => ex Z : p(Z))

univar(X,_,X)  :- (atomic(X);var(X);X==[[]]), !.
univar(F,Q,F1) :-
    F=..[A,B|T], ( (A=ex;A=all) -> B=(X:C), delete2(Q,X,Q1),
    copy_term((X,C,Q1),(Y,D,Q1)), univar(D,[Y|Q],D1), F1=..[A,Y:D1] ;
    univar(B,Q,B1), univar(T,Q,T1), F1=..[A,B1|T1] ).

% ------------------------------------------------------------------
%  union2/member2 - union and member for lists without unification

union2([],L,[L]).
union2([X|L1],L2,M) :- member2(X,L2), !, union2(L1,L2,M).
union2([X|_],L2,M)  :- (-Xn=X;-X=Xn) -> member2(Xn,L2), !, M=[].
union2([X|L1],L2,M) :- union2(L1,[X|L2],M).

member2(X,[Y|_]) :- X==Y, !.
member2(X,[_|T]) :- member2(X,T).

% ------------------------------------------------------------------
%  delete2 - delete variable from list

delete2([],_,[]).
delete2([X|T],Y,T1) :- X==Y, !, delete2(T,Y,T1).
delete2([X|T],Y,[X|T1]) :- delete2(T,Y,T1).

% ------------------------------------------------------------------
%  mreorder - reorder clauses

mreorder(M,M,0) :- !.
mreorder(M,M1,I) :-
    length(M,L), K is L//3, append(A,D,M), length(A,K),
    append(B,C,D), length(C,K), mreorder2(C,A,B,M2), I1 is I-1,
    mreorder(M2,M1,I1).

mreorder2([],[],C,C).
mreorder2([A|A1],[B|B1],[C|C1],[A,B,C|M1]) :- mreorder2(A1,B1,C1,M1).

:- dynamic(pathlim/0), dynamic(lit/4).


%%% prove matrix M / formula F

prove(F,Proof) :- prove2(F,[cut,comp(7)],Proof).

prove2(F,Set,Proof) :-
    (F=[_|_] -> M=F ; make_matrix(F,M,Set)),
    retractall(lit(_,_,_,_)), (member([-(#)],M) -> S=conj ; S=pos),
    assert_clauses(M,S), prove(1,Set,Proof).

prove(PathLim,Set,Proof) :-
    \+member(scut,Set) -> prove([-(#)],[],PathLim,[],Set,[Proof]) ;
    lit(#,_,C,_) -> prove(C,[-(#)],PathLim,[],Set,Proof1),
    Proof=[C|Proof1].
prove(PathLim,Set,Proof) :-
    member(comp(Limit),Set), PathLim=Limit -> prove(1,[],Proof) ;
    (member(comp(_),Set);retract(pathlim)) ->
    PathLim1 is PathLim+1, prove(PathLim1,Set,Proof).

%%% leanCoP core prover

prove([],_,_,_,_,[]).

prove([Lit|Cla],Path,PathLim,Lem,Set,Proof) :-
    Proof=[[[NegLit|Cla1]|Proof1]|Proof2],
    \+ (member(LitC,[Lit|Cla]), member(LitP,Path), LitC==LitP),
    (-NegLit=Lit;-Lit=NegLit) ->
       ( member(LitL,Lem), Lit==LitL, Cla1=[], Proof1=[]
         ;
         member(NegL,Path), unify_with_occurs_check(NegL,NegLit),
         Cla1=[], Proof1=[]
         ;
         lit(NegLit,NegL,Cla1,Grnd1),
         unify_with_occurs_check(NegL,NegLit),
         ( Grnd1=g -> true ; length(Path,K), K<PathLim -> true ;
           \+ pathlim -> assert(pathlim), fail ),
         prove(Cla1,[Lit|Path],PathLim,Lem,Set,Proof1)
       ),
       ( member(cut,Set) -> ! ; true ),
       prove(Cla,Path,PathLim,[Lit|Lem],Set,Proof2).

%%% write clauses into Prolog's database

assert_clauses([],_).
assert_clauses([C|M],Set) :-
    (Set\=conj, \+member(-_,C) -> C1=[#|C] ; C1=C),
    (ground(C) -> G=g ; G=n), assert_clauses2(C1,[],G),
    assert_clauses(M,Set).

assert_clauses2([],_,_).
assert_clauses2([L|C],C1,G) :-
    assert_renvar([L],[L2]), append(C1,C,C2), append(C1,[L],C3),
    assert(lit(L2,L,C2,G)), assert_clauses2(C,C3,G).

assert_renvar([],[]).
assert_renvar([F|FunL],[F1|FunL1]) :-
    ( var(F) -> true ; F=..[Fu|Arg], assert_renvar(Arg,Arg1),
      F1=..[Fu|Arg1] ), assert_renvar(FunL,FunL1).

append3(X,Y,Z,V) :- 
	append(X,Y,L),
	append(L,Z,V).

bmatrix((~X),Pol,M)      :- !, Pol1 is (1-Pol), bmatrix(X,Pol1,M).
bmatrix((X1<=>X2),Pol,M) :- !, bmatrix(((X1=>X2),(X2=>X1)),Pol,M).
bmatrix(X,Pol,M3)        :- X=..[F,X1,X2], alpha(F,Pol,Pol1,Pol2), !,
                            bmatrix(X1,Pol1,M1), bmatrix(X2,Pol2,M2),
                            union(M1,M2,M3).
bmatrix(X,Pol,[M3])      :- X=..[F,X1,X2], beta(F,Pol,Pol1,Pol2), !,
                            bmatrix(X1,Pol1,M1), bmatrix(X2,Pol2,M2),
                            sim(M1,M4), sim(M2,M5), union(M4,M5,M3).
bmatrix(X,0,[[X]]).
bmatrix(X,1,[[-X]]).

sim([M],M) :- !. sim(M,[M]).

alpha(',',1,1,1):-!. alpha(';',0,0,0):-!. alpha((=>),0,1,0):-!.
beta(',',0,0,0):-!.  beta(';',1,1,1):-!.  beta((=>),1,0,1):-!.

%%% prove

prove(F) :- bmatrix(F,0,M), pure(M,M1), dp(M1).

%%% dp (non-clausal DP)

dp([]) :- !, fail.
dp(M)  :- member([],M), !.

% UNIT
dp(M)  :- member([L],M), ( atom(L), N= -L ; -N=L ), !,
          reduce(M,N,L,M1), dp(M1).

% Beta-Splitting
dp([[[C1|M],[C2|M2]|C]|M1])  :- !, 
   dp([[[C1|M]]|M1]), dp([[[C2|M2]]|M1]), dp([C|M1]).

% Splitting
dp(M)  :- selectLit(M,P,N),
          reduce(M,P,N,M1), dp(M1),
          reduce(M,N,P,M2), dp(M2).

%%% reduce MReduce/CReduce

reduce(L,L,_,[[]])   :- !.  %% true
reduce(N,_,N,[])     :- !.  %% false

reduce([C|M],L,N,M1) :- !,
 reduce(C,L,N,C1),             %% evaluate clauses/matrices
 (C1=[] -> M1=[[]] ;           %% matrix/clause elimination
 C1=[[]] -> reduce(M,L,N,M1) ; %% simplify
 reduce(M,L,N,M2),             %% evaluate remaining cl./mat.
 (M2=[[]] -> M1=[[]] ;         %% matrix/clause elimination
  M2=[], C1=[M3] -> M1=M3 ; M1=[C1|M2] )). 
  
reduce(A,_,_,A).

%% select literal
selectLit([M|_],A,An) :- !, selectLit(M,A,An).
selectLit(-A,-A,A)    :- !.
selectLit(A,A,-A).

%%% PURE
pure(M,M1) :- litM(M,LitM), pure(M,LitM,M1,LitM).

pure(M,[],M,_).
pure(M,[L|LitM],M1,LL) :- 
   (L= -N ; -L=N), !,
   (member(N,LL) -> M2=M ;
    reduce(M,N,L,M2)), pure(M2,LitM,M1,LL).

litM([],[])   :- !.
litM([H|T],L) :- !, litM(H,L1), litM(T,L2), union(L1,L2,L).
litM(A,[A]).

% -----------------------------------------------------------------------------
% Code for computing conflict sets

conjunction([], true).
conjunction([X], X).
conjunction([X|L], (X,F)) :- conjunction(L, F).

normal(X, ~ab(X)).

instantiate((_,_), _, [], (,), true).
instantiate((_,_), _, [], (;), false).
instantiate((X,F), COMP, [C], O, FG) :-
	copy_term((X,F), O, (C,I)),
	ground(I, COMP, FG).
instantiate((X,F), COMP, [C|Rest], O, AG) :-  
	copy_term((X,F),(C,I)),
	ground(I, COMP, FG),
	instantiate((X,F), COMP, Rest, O, RG),
	AG =.. [O, FG, RG].

ground(~F, COMP, ~G) :- ground(F, COMP, G).
ground((F1 , F2),   COMP, (G1 , G2))   :- ground(F1, COMP, G1), ground(F2, COMP, G2).
ground((F1 ; F2),   COMP, (G1 ; G2))   :- ground(F1, COMP, G1), ground(F2, COMP, G2).
ground((F1 => F2),  COMP, (G1 => G2))  :- ground(F1, COMP, G1), ground(F2, COMP, G2).
ground((F1 <=> F2), COMP, (G1 <=> G2)) :- ground(F1, COMP, G1), ground(F2, COMP, G2).
ground((all X:F), COMP, G) :- instantiate((X,F), COMP, COMP, (,), G).
ground((ex X:F), COMP, G) :- instantiate((X,F), COMP, COMP, (;), G).
ground(Lit, _, Lit).


% -----------------------------------------------------------------------------
%  tp(+SD,+COMP, +OBS, +HS, -CS)  
%             -  Determines a conflict set for the diagnostic problem
%                (SD,COMP-HS,OBS). The term ~ab(c) is assumed for all
%                elements of COMP-HS.              
%
%  SD: list of first-order formula, where variables are understood to quantify
%      over elements in COMP
%  COMP: set of all components
%  OBS: set of (ground) facts
%  HS: set of components assumed to be abnormal
%
%  Example: tp([all X:(~ab(X) => p(X))], [c,d], [~p(c)], [], CS).
%           CS = [c]

tp(SD, COMP, OBS, HS, CS) :-
	subtract(COMP, HS, NormComp),  % components assumed to be normal
	maplist(normal, NormComp, S),  % normal predicates
	append3(SD, S, OBS, Theory),   % construct theory
	conjunction(Theory, F),        % construct formula
	ground(F, COMP, F2),!,         % ground formula based on COMP
	prove(~((F2))),                % establish truth of negated formula 
	prove(~((F2)), Proof),         % find proof (if true)
	flatten(Proof,FlatProof),      % extract elements of proof
	include(copy_term(ab(_)),FlatProof,Abs),
	maplist(arg(1), Abs, CompProof),% components in proof
	list_to_set(CompProof, CompSorted),   % remove repetitions
	% if elements from HS occur in proof, ignore them
	subtract(CompSorted, HS, CS),!.

% -----------------------------------------------------------------------------
