
noun_phrase(T0,T4,Ind,C0,C4) :-
det(T0,T1,Ind,C0,C1),
adjectives(T1,T2,Ind,C1,C2),
noun(T2,T3,Ind,C2,C3),
mp(T3,T4,Ind,C3,C4).

det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det([arrive | T],T,_,C,C).
det([stop | T],T,_,C,C).
det(T,T,_,C,C).

adjectives(T0,T2,Ind,C0,C2) :-
adj(T0,T1,Ind,C0,C1),
adjectives(T1,T2,Ind,C1,C2).
adjectives(T,T,_,C,C).

mp(T0,T2,I1,C0,C2) :-
reln(T0,T1,I1,I2,C0,C1),
noun_phrase(T1,T2,I2,C1,C2).
mp([that|T0],T2,I1,C0,C2) :-
reln(T0,T1,I1,I2,C0,C1),
noun_phrase(T1,T2,I2,C1,C2).
mp([arrive|T0],T2,I1,C0,C2) :-
reln(T0,T1,I1,I2,C0,C1),
noun_phrase(T1,T2,I2,C1,C2).
mp(T,T,_,C,C).

ask(Q,A) :-
question(Q,[],A,[],C),
prove_all(C).

prove_all([]).
prove_all([H|T]) :-
call(H),      % built-in Prolog predicate calls an atom
prove_all(T).

question([is | T0],T2,Ind,C0,C2) :-
noun_phrase(T0,T1,Ind,C0,C1),
mp(T1,T2,Ind,C1,C2).
question([what | T0],T2,Ind,C0,C2) :-      % allows for a "what ... is ..."
noun_phrase(T0,[is|T1],Ind,C0,C1),
mp(T1,T2,Ind,C1,C2).
question([what | T0],T2,Ind,C0,C2) :-
noun_phrase(T0,T1,Ind,C0,C1),
mp(T1,T2,Ind,C1,C2).
question([where | T0],T2,Ind,C0,C2) :-
noun_phrase(T0,T1,Ind,C0,C1),
mp(T1,T2,Ind,C1,C2).
question([what,is | T0],T1,Ind,C0,C1) :-
noun_phrase(T0,T1,Ind,C0,C1).
question([what,is | T0],T1,Ind,C0,C1) :-
mp(T0,T1,Ind,C0,C1).
question([where,is | T0],T1,Ind,C0,C1) :-
noun_phrase(T0,T1,Ind,C0,C1).
question([where,is | T0],T1,Ind,C0,C1) :-
mp(T0,T1,Ind,C0,C1).
question([what,are | T0],T1,Ind,C0,C1) :-
mp(T0,T1,Ind,C0,C1).
question([what,are | T0],T1,Ind,C0,C1) :-
noun_phrase(T0,T1,Ind,C0,C1).
question([when | T0],T2,Ind,C0,C2) :-
noun_phrase(T0,T1,Ind,C0,C1),
mp(T1,T2,Ind,C1,C2).

question([which,are | T0],T1,Ind,C0,C1) :-
mp(T0,T1,Ind,C0,C1).
question([which,are | T0],T1,Ind,C0,C1) :-
noun_phrase(T0,T1,Ind,C0,C1).

% all above was taken from the before noted program of nl_interface.pl
% the rest below is my modifications and additions to solve this problem

noun([bus|T],T,Ind,C,[bus(Ind)|C]).

noun([times|T],T,Ind,C,[time(Ind)|C]).
noun([stop, for, the|T],T,Ind,C,[stop(T,Ind)|C]).
%noun([busses, on|T], T, Ind, C, [street(T, Ind)|C]).
%noun([time|T],T,Ind,C,[time(Ind)|C]).
% for pronouns
noun([A|T],T,Ind,C,C) :- bus(A).
%noun([A|T],T,Ind,C,C) :- stop(A).
%noun([A|T],T,Ind,C,C) :- time(A).
noun([A|T],T,Ind,C,C) :- streetname(A).

adj([Language,speaking|T],T,Ind,C,[language(Ind,Language)|C]).

time(A) :- atomic(A).

% credit to this link for way to take inputs and the other one for the range function:
%https://stackoverflow.com/questions/5107745/user-input-how-can-we-do-it
%https://stackoverflow.com/questions/7175258/prolog-generating-list-of-numbers-fitting-given-range

% allows to find the next stop time after given the current time
range(Low, Low, High).
range(Out,Low,High) :- NewLow is Low+1, range(Out, NewLow, High).
time_reln([H|R],B) :- write('What time is it? (in 2400 hour terms)'),
nl, read(Y), time(Y),  W is Y+60, V is Y+1, range(B,V,W), time_reln(H,B),
write('The next bus departs at '), write(B), write(' from '), stop(H,E), write(E).

% gives either the stop at UBC for a certain bus, or a list of all bus stops on the route
stop([H|R],A) :- stop(H,A), write('The '), write(H), write(' stops at '), write(A).
stops([H|R],A) :- stops(H,A), write('Here are the list of stops for the '), write(H),
write(':
'), write(A).

% gives best time to leave to get somewhere at desired time
best_time([H|R],A) :- write('What time would you like to get there? (in 2400 hour terms)'),
nl,read(T),performQuery(H,T,A),bus_destination(N,H),write(N),write(' bus departs at '),write(A).

% gives the buses for a certain street
street([H|R],A) :- street(H,A), write('Here are the busses stopping along '), write(H),
write('
'), write(A).

reln([the, next, depature, for |T],T,I1,I2,C,[time_reln(T,I2)|C]).
reln([the, stop, for, the |T],T,I1,I2,C,[stop(T,I2)|C]).
reln([the, stops, for, the |T],T,I1,I2,C,[stops(T,I2)|C]).
reln([the, buses, on |T], T,I1,I2,C, [street(T,I2)|C]).
reln([the, best, time, to, take, the, bus ,to |T],T,I1,I2,C,[best_time(T,I2)|C]).

%reln([borders|T],T,I1,I2,C,[borders(I1,I2)|C]).

%test case:
%ask([what, is, the, next, depature, for, 99],A).
%ask([where, is, the, stop, for, the, 4],A).
%ask([what, are, the, stops, for, the, 4],A).
%ask([which, are, the, buses, on, wesbrook_mall],A).
%ask([what, is, the, best, time, to, take,the,bus,to, twenty_nine_avenue  ], A).

%subtracts amount of time taken for bus to get from ubc to destination from desired arrival time
fixtime(H,R,F):-
D is H-R, clockify(D,F).
clockify(D,F):-
D<0 , L is 2400+D, clockify(L,F).
clockify(D,F):-
M is mod(D,100) , M<60 ,F is D.
clockify(D,F):-
M is mod(D,100) , M>59, H is D-M, L is 100-M , A is 60-L, F is H+A.

%finds the first departure time K, for route S before time R. goes down to maximum of M minutes
getK(S,R,K,M):-
time_reln(S,R),K is R.
getK(S,R,K,M):-
NewR is R-1, NewR>M , getK(S,NewR,K,M).


% Database:

%performQuery gives best time to leave A, based on destination K and time T
performQuery(K,T,A):-
T>2399, write('wrong input'),abort.

performQuery(K,T,A):-
M is mod(T,100), M>59, write('wrong input'),abort.

performQuery(K,T,A):-
bus_destination(4,K), p(T,A).

performQuery(K,T,A):-
bus_destination(9,K), bo(T,A).

performQuery(K,T,A):-
bus_destination(14,K), ha(T,A).

performQuery(K,T,A):-
bus_destination(25,K), bre(T,A).


performQuery(K,T,A):-
bus_destination(33,K), tnave(T,A).


performQuery(K,T,A):-
bus_destination(41,K), du(T,A).


performQuery(K,T,A):-
bus_destination(43,K), j(T,A).


performQuery(K,T,A):-
bus_destination(49,K), mt(T,A).


performQuery(K,T,A):-
bus_destination(84,K), v(T,A).


performQuery(K,T,A):-
bus_destination(99,K), cd(T,A).

performQuery(K,T,A):-
bus_destination(258,K), w(T,A).


performQuery(K,T,A):-
bus_destination(480,K), bp(T,A).


% gets best time to leave for powell
p(T,K):-
T>26, T<806 , K is 2352.
p(T,K):-
fixtime(T,59,R), fixtime(R,30,M),getK(4,R,K,M).

% gets best time to leave for boundary
bo(T,K):-
T>1919, K is 1822.
bo(T,K):-
T<851, K is 1822.
bo(T,K):-
fixtime(T,107,R), fixtime(R,30,M),getK(9,R,K,M).

% gets best time to leave for hastings
ha(T,K):-
T>335, T< 707, K is 225.
ha(T,K):-
fixtime(T,117,R), fixtime(R,30,M),getK(14,R,K,M).

% gets best time to leave for brentwood
bre(T,K):-
T>100, T<705, K is 8.
bre(T,K):-
fixtime(T,125,R), fixtime(R,30,M),getK(25,R,K,M).

% gets best time to leave for 29th avenue
tnave(T,K):-
T<702, K is 2259.
tnave(T,K):-
fixtime(T,101,R), fixtime(R,30,M),getK(33,R,K,M).

% gets best time to leave for dunbar
du(T,K):-
T>20, T<721, K is 6.
du(T,K):-
fixtime(T,18,R), fixtime(R,50,M),getK(41,R,K,M).

% gets best time to leave for Joyce Station
j(T,K):-
T>2000, K is 1935.
j(T,K):-
T<655, K is 1935.
j(T,K):-
fixtime(T,40,R), fixtime(R,30,M),getK(43,R,K,M).

% gets best time to leave for Metrotown
mt(T,K):-
T>2226, K is 2116.
mt(T,K):-
T<853, K is 2116.
mt(T,K):-
fixtime(T,110,R), fixtime(R,40,M),getK(49,R,K,M).

% gets best time to leave for VCC
v(T,K):-
T>2230, K is 2229.
v(T,K):-
T<559, K is 2229.
v(T,K):-
fixtime(T,40,R), fixtime(R,50,M),getK(84,R,K,M).

% gets best time to leave for Commercial Drive
cd(T,K):-
T>312, T<740,K is 210.
cd(T,K):-
fixtime(T,112,R), fixtime(R,50,M),getK(99,R,K,M).

% gets best time to leave for West Vancouver
w(T,K):-
T>1538,T< 1739, K is 1508.
w(T,K):-
K is 1738.

% gets best time to leave for bridgeport
bp(T,K):-
T>2238, K is 2138.
bp(T,K):-
T<750, K is 2138.
bp(T,K):-
fixtime(T,100,R), fixtime(R,55,M),getK(480,R,K,M).

% gets best time to leave for downtown
dt(T,K):-
T>2000,K is 1949.
dt(T,K):-
T<158, K is 1949.
dt(T,K):-
T>157, T<830, night(T,K).
dt(T,K):-
fixtime(T,40,R), fixtime(R,55,M),getK(44,R,K,M).
night(T,K):-
T<228, K is 138.
night(T,K):-
T<258,T>228, K is 208.
night(T,K):-
T>257,T<830, K is 238.

bus(4).
bus(9).
bus(14).
bus(25).
bus(33).
bus(41).
bus(43).
bus(44).
bus(49).
bus(84).
bus(99).
bus(258).
bus(480).
bus(n17).
bus(c18).
bus(c20).

bus_destination(4,powell).
bus_destination(9,boundary).
bus_destination(14,hastings).
bus_destination(25,brentwood).
bus_destination(33,twenty_nine_avenue).
bus_destination(41,dunbar).
bus_destination(43,Joyce_Station).
bus_destination(44,downtown).
bus_destination(49,metrotown).
bus_destination(84,vcc).
bus_destination(99,commerical_drive).
bus_destination(258,west_vancouver).
bus_destination(480,bridgeport).
bus_destination(n17,downtown).
bus_destination(c18,west_mall).
bus_destination(c20,totem_park).

stop(4, 'UBC Exchange Bay 10').
stop(9, 'UBC Exchange Bay 12').
stop(14, 'UBC Exchange Bay 11').
stop(25, 'UBC Exchange Bay 8').
stop(33, 'UBC Exchange Bay 9').
stop(41, 'UBC Exchange Bay 7').
stop(43, 'UBC Exchange Bay 6').
stop(44, 'UBC Exchange Bay 3').
stop(49, 'UBC Exchange Bay 5').
stop(84, 'UBC Exchange Bay 2').
stop(99, 'UBC Exchange Bay 4').
stop(258, 'UBC Exchange Bay 13').
stop(480, 'UBC Exchange Bay 6').
stop(n17, 'UBC Exchange Bay 12').
stop(c18, 'UBC Exchange Bay 1').
stop(c20, 'UBC Exchange Bay 1').
streetname(alma).
streetname(birney_ave).
streetname(blanca_st).
streetname(burrard_st).
streetname(cambie_st).
streetname(central_blvd).
streetname(chancellor_blvd).
streetname(commerical_dr).
streetname(dunbar_st).
streetname(e_2_ave).
streetname(e_22_ave).
streetname(e_29_ave).
streetname(e_33_ave).
streetname(e_41_ave).
streetname(e_49_ave).
streetname(e_broadway).
streetname(e_king_edward_ave).
streetname(east_mall).
streetname(granville_st).
streetname(great_northern_way).
streetname(homer_st).
streetname(howe_st).
streetname(imperial_st).
streetname(joyce_st).
streetname(kincaid_st).
streetname(king_edward_ave).
streetname(kingsway).
streetname(lower_mall).
streetname(marpole_ave).
streetname(midlothian_ave).
streetname(n_grandview_highway).
streetname(nanaimo).
streetname(nw_marine_dr).
streetname(sanderson_way).
streetname(seymour_st).
streetname(slocan_st).
streetname(stadium_rd).
streetname(sw_marine_dr).
streetname(thunderbird_blvd).
streetname(university_blvd).
streetname(w_10_ave).
streetname(w_16_ave).
streetname(w_2_ave).
streetname(w_29_ave).
streetname(w_4_ave).
streetname(w_41_ave).
streetname(w_49_ave).
streetname(w_6_ave).
streetname(w_71).
streetname(w_broadway).
streetname(w_cordova).
streetname(w_king_edward_ave).
streetname(w_pender_st).
streetname(wesbrook_mall).
streetname(willingdon_ave).
streetname(willingdon_extension).
streetname(powell).
streetname(boundary).
streetname(hastings).
streetname(brentwood).
streetname(twenty_nine_avenue).
streetname(dunbar).
streetname(Joyce_Station).
streetname(downtown).
streetname(metrotown).
streetname(vcc).
streetname(commerical_drive).
streetname(west_vancouver).
streetname(bridgeport).
streetname(downtown).
streetname(west_mall).
streetname(totem_park).

street(alma, '14
n17').
street(birney_ave, 'c18').
street(blanca_st, '4').
street(burrard_st, '44').
street(cambie_st, '4
44').
street(central_blvd, '49').
street(chancellor_blvd, '44
84').
street(commerical_dr, '9
99').
street(dunbar_st, '25
49').
street(e_2_ave, '84').
street(e_22_ave, '25').
street(e_29_ave, '33').
street(e_33_ave, '33').
street(e_41_ave, '41
43').
street(e_49_ave, '49').
street(e_broadway, '9
99').
street(e_king_edward_ave, '25').
street(east_mall, 'c18').
street(granville_st, '4
14
480
n17').
street(great_northern_way, '84').
street(homer_st, '258').
street(howe_st,'n17').
street(imperial_st, '49').
street(joyce_st, '41').
street(kincaid_st, '25').
street(king_edward_ave, '25').
street(kingsway, '25').
street(lower_mall, 'c20').
street(marpole_ave, '33').
street(midlothian_ave, '33').
street(n_grandview_highway, '9
99').
street(nanaimo, '25').
street(nw_marine_dr, 'c20').
street(sanderson_way, '25').
street(seymour_st, '44
n17').
street(slocan_st, '33').
street(stadium_rd, 'c20').
street(sw_marine_dr, '41
49').
street(thunderbird_blvd, 'c18').
street(university_blvd, '4
9
14
99
258
n17').
street(w_10_ave, '9
14
n17').
street(w_16_ave, '25
33
49
480').
street(w_2_ave, '84').
street(w_29_ave, '33').
street(w_41_ave, '41
43
49
480').
street(w_6_ave, '84').
street(w_71, '480').
street(w_broadway, '9
14
99
n17').
street(w_cordova, '4').
street(w_king_edward_ave, '25').
street(w_pender_st, '14
44').
street(wesbrook_mall, '25
33
41
43
49
480
c18
c20').
street(willingdon_ave, '25
49').
street(willingdon_extension, '49').


stops(4, '58895 UBC Exchange Bay 10
50268 University Blvd at Allison Rd
57938 University Blvd at Acadia Rd
50270 University Blvd at 5300 Block
50271 University Blvd at 5100 Block
50272 Blanca St at W 10 Ave
50344 Blanca St at W 8 Ave
50345 Blanca St at W 6 Ave
50346 W 4 Ave at Blanca St
50347 W 4 Ave at Tolmie St
50349 W 4 Ave at Trimble St
50350 W 4 Ave at 4100 Block
59298 W 4 Ave at Dieppe Lane
50352 W 4 Ave at Wallace St
50353 W 4 Ave at Highbury St
50354 W 4 Ave at Alma St
50389 W 4 Ave at Collingwood St
50390 W 4 Ave at Waterloo St
50391 W 4 Ave at Trutch St
50392 W 4 Ave at Bayswater St
50393 W 4 Ave at Macdonald St
50394 W 4 Ave at Trafalgar St
50395 W 4 Ave at Larch St
50396 W 4 Ave at Balsam St
50397 W 4 Ave at Vine St
50398 W 4 Ave at Arbutus St
50399 W 4 Ave at Cypress St
50400 W 4 Ave at Burrard St
50401 W 4 Ave at Fir St
50403 W 5 Ave at Granville St
58593 Granville St at Drake St
50220 Granville St at Davie St
50222 Granville St at Nelson St
58144 Granville St at Smithe St
61293 Granville St at W Georgia St
60993 Granville St at Dunsmuir St
58313 Granville St at W Pender St
50034 Granville St at W Hastings St
50035 W Cordova St at Seymour St
50036 W Cordova St at Homer St
50410 Cambie St at W Hastings St').
stops(9, '61702 UBC Exchange Bay 12
50268 University Blvd at Allison Rd
57938 University Blvd at Acadia Rd
50270 University Blvd at 5300 Block
50271 University Blvd at 5100 Block
50274 W 10 Ave at Blanca St
50275 W 10 Ave at Tolmie St
50276 W 10 Ave at Sasamat St
50277 W 10 Ave at Trimble St
50278 W 10 Ave at Discovery St
50279 W 10 Ave at Courtenay St
50280 W 10 Ave at Camosun St
50281 W 10 Ave at Crown St
50282 W 10 Ave at Wallace St
50283 W 10 Ave at Highbury St
50315 W Broadway at Alma St-
60725 W Broadway at Collingwood St
60726 W Broadway at Blenheim St
60727 W Broadway St at Balaclava St
60728 W Broadway at Bayswater St
50319 W Broadway at Macdonald St
50320 W Broadway at Trafalgar St
50321 W Broadway at Larch St
50322 W Broadway at Vine St
50323 W Broadway at Arbutus St
50324 W Broadway at Maple St
50325 W Broadway at Burrard St
58240 W Broadway at Fir St--
50326 W Broadway at Granville St-
50327 W Broadway at Hemlock St
50328 W Broadway at Alder St
50329 W Broadway at Oak St
50872 W Broadway at Laurel St
50873 W Broadway at Willow St
50874 W Broadway at Heather St
52100 W Broadway at Yukon St
60006 W Broadway at Columbia St
50878 E Broadway at Ontario St
50879 E Broadway at Main St
50772 E Broadway at Kingsway
50773 E Broadway at Prince Edward St
50774 E Broadway at Saint George St
58614 E Broadway at Fraser St
50883 E Broadway at Saint Catherines St
50884 E Broadway at Glen Dr
50885 E Broadway at Clark Dr-
50886 E Broadway at Woodland Dr
58491 Commercial Dr at N Grandview Highway
58621 N Grandview Highway at E 8 Ave').
stops(14, '61701 UBC Exchange Bay 11
50268 University Blvd at Allison Rd
57938 University Blvd at Acadia Rd
50270 University Blvd at 5300 Block
50271 University Blvd at 5100 Block
50274 W 10 Ave at Blanca St
50275 W 10 Ave at Tolmie St
50276 W 10 Ave at Sasamat St
50277 W 10 Ave at Trimble St
50278 W 10 Ave at Discovery St
50279 W 10 Ave at Courtenay St
50280 W 10 Ave at Camosun St
50281 W 10 Ave at Crown St
50282 W 10 Ave at Wallace St
50283 W 10 Ave at Highbury St
50314 Alma St at W 10 Ave
50315 W Broadway at Alma St-
60725 W Broadway at Collingwood St
60726 W Broadway at Blenheim St
60727 W Broadway St at Balaclava St
60728 W Broadway at Bayswater St
50319 W Broadway at Macdonald St
50320 W Broadway at Trafalgar St
50321 W Broadway at Larch St
50322 W Broadway at Vine St
50323 W Broadway at Arbutus St
50324 W Broadway at Maple St
50325 W Broadway at Burrard St
50871 W Broadway at Fir St-
50216 Granville St at W Broadway
50217 Granville St at W 7 Ave
50218 Granville St at W 5 Ave
58593 Granville St at Drake St
50220 Granville St at Davie St
50222 Granville St at Nelson St
58144 Granville St at Smithe St
61293 Granville St at W Georgia St
60993 Granville St at Dunsmuir St
58313 Granville St at W Pender St
50078 W Pender St at Seymour St
50079 W Pender St at Richards St
50407 Homer St at W Pender St').
stops(25, '60099 UBC Exchange Bay 8
51479 Wesbrook Mall at Agronomy Rd
51480 Wesbrook Mall at Thunderbird Blvd
51483 Wesbrook Mall at Hampton Pl
51484 W 16 Ave at Wesbrook Mall
51485 W 16 Ave at Pacific Spirit Park
51486 W 16 Ave at Blanca St
51487 W 16 Ave at Tolmie St
51488 W 16 Ave at Trimble St
51489 W 16 Ave at Courtenay St
51490 W 16 Ave at Camosun St
51491 W 16 Ave at Crown St
51492 W 16 Ave at Highbury St
50362 Dunbar St at W 17 Ave
50363 Dunbar St at W 19 Ave
50364 Dunbar St at W 21 Ave
50365 Dunbar St at W 23 Ave
51493 W King Edward Ave at Dunbar St
51494 W King Edward Ave at Collingwood St
51495 W King Edward Ave at Blenheim St
51496 W King Edward Ave at Balaclava St
51497 W King Edward Ave at Quesnel Dr
51498 W King Edward Ave at Puget Dr
51499 W King Edward Ave at Brakenridge St
51500 W King Edward Ave at Valley Dr
51501 W King Edward Ave at Yew St
51502 W King Edward Ave at Arbutus St
51503 W King Edward Ave at Maple Cres
51504 W King Edward Ave at Cypress St
51505 W King Edward Ave at Marguerite St
51506 W King Edward Ave at Alexandra St
51507 W King Edward Ave at Granville St
51508 W King Edward Ave at Hudson St
51509 W King Edward Ave at Osler St
51510 W King Edward Ave at Oak St
51511 W King Edward Ave at Willow St
51512 W King Edward Ave at Ash St
51513 King Edward Ave at Cambie St
51516 W King Edward Ave at Manitoba St
51517 E King Edward Ave at Quebec St
51518 E King Edward Ave at Main St
51519 E King Edward Ave at Prince Edward St
51520 E King Edward Ave at Saint George St
51521 E King Edward Ave at Fraser St
51522 E King Edward Ave at Saint Catherines St
51523 E King Edward Ave at Glen Dr
51524 E King Edward Ave at Clark Dr
51525 E King Edward Ave at Knight St
51144 Kingsway at Perry St
51145 Kingsway at Miller St
50659 Kingsway at Victoria Dr
61990 Kingsway at Gladstone St
51526 Nanaimo St at Kingsway
51527 Nanaimo St at Brock St
51528 Nanaimo St at E 27 Ave
51529 Nanaimo Station Bay 2
50690 E 22 Ave at Nanaimo St
50691 E 22 Ave at Penticton St
51530 E 22 Ave at Slocan St
51531 E 22 Ave at Kaslo St
51532 E 22 Ave at Boyd Diversion
51533 E 22 Ave at Lillooet St
51534 E 22 Ave at Rupert St
51535 E 22 Ave at Cassiar St
51536 E 22 Ave at Kootenay St
51537 Kincaid St at Boundary Rd
51538 Kincaid St at Smith Ave
51539 Kincaid St at Macdonald Ave
58605 Sanderson Way at Gilmore Way
51541 Sanderson Way at Mathissi Pl
51542 Willingdon Ave at Goard Way
51543 Willingdon Ave at Canada Way
61259 Willingdon Ave at Still Creek Dr
51458 Willingdon Ave at Juneau St
58496 Willingdon Ave at Lougheed Highway
61805 Brentwood Station Bay 5').
stops(33, '60100 UBC Exchange Bay 9
51479 Wesbrook Mall at Agronomy Rd
51480 Wesbrook Mall at Thunderbird Blvd
51483 Wesbrook Mall at Hampton Pl
51484 W 16 Ave at Wesbrook Mall
51485 W 16 Ave at Pacific Spirit Park
51486 W 16 Ave at Blanca St
51487 W 16 Ave at Tolmie St
51488 W 16 Ave at Trimble St
51489 W 16 Ave at Courtenay St
51490 W 16 Ave at Camosun St
51491 W 16 Ave at Crown St
51492 W 16 Ave at Highbury St
61088 W 16 Ave at Dunbar St
61089 W 16 Ave at Blenheim St
50054 W 16 Ave at Trutch St
50055 W 16 Ave at Carnarvon St
61090 W 16 Ave at Macdonald St
61091 W 16 Ave at Trafalgar St
61092 W 16 Ave at Vine St
61093 W 16 Ave at East Blvd
61094 W 16 Ave at Burrard St
61095 Marpole Ave at Mcrae Ave
61096 W 16 Ave at Oak St
61097 W 16 Ave at Willow St
50415 Cambie St at W 16 Ave
50416 Cambie St at W 18 Ave
50417 Cambie St at W 20 Ave
50418 Cambie St at W 22 Ave
50419 Cambie St at W King Edward Ave
50420 Cambie St at W 27 Ave
61099 W 29 Ave at Cambie St
61100 Midlothian Ave at Clancy Loranger Way
61101 E 33 Ave at Ontario St
61150 E 33 Ave at Main St
61103 E 33 Ave at Prince Edward St
61104 E 33 Ave at Fraser St
61105 E 33 Ave at Somerville St
61135 E 33 Ave at Inverness St
61106 E 33 Ave at Knight St
61107 E 33 Ave at Argyle St
61108 E 33 Ave at Victoria Dr
61109 E 33 Ave at Nanaimo St
50662 Slocan St at Kingsway
50663 Slocan St at Ward St
50664 Slocan St at Euclid Ave
51817 E 29 Ave at Slocan St
51818 29th Ave Station Bay 1').
stops(41, '60117 UBC Exchange Bay 7
51479 Wesbrook Mall at Agronomy Rd
51480 Wesbrook Mall at Thunderbird Blvd
51483 Wesbrook Mall at Hampton Pl
61579 Wesbrook Mall at Birney Ave
61581 Wesbrook Mall at TRIUMF Centre
59489 SW Marine Dr at Wesbrook Mall
51887 SW Marine Dr at Kullahun Dr
58650 W 41 Ave at Crown St-
51888 W 41 Ave at Crown St--
51889 W 41 Ave at Wallace St
51890 W 41 Ave at Highbury St
50641 W 41 Ave at Dunbar St
50374 W 41 Ave at Collingwood St
50375 W 41 Ave at Blenheim St
50377 W 41 Ave at Carnarvon St
50132 W 41 Ave at Mackenzie St
50378 W 41 Ave at Macdonald St
50379 W 41 Ave at Elm St
50380 W 41 Ave at Balsam St
50381 W 41 Ave at Yew St
50382 W 41 Ave at West Boulevard
50383 W 41 Ave at Maple St
50384 W 41 Ave at Cypress St
50385 W 41 Ave at Wiltshire St
50386 W 41 Ave at Adera St
50388 W 41 Ave at Granville St
50199 W 41 Ave at Granville St
50200 W 41 Ave at Selkirk St
50201 W 41 Ave at Osler St
50804 W 41 Ave at Oak St
50134 W 41 Ave at Willow St
50135 W 41 Ave at Heather St
50136 W 41 Ave at Cambie St
50138 W 41 Ave at Columbia St
50139 W 41 Ave at Manitoba St
50140 E 41 Ave at Ontario St
50253 E 41 Ave at Main St
50642 E 41 Ave at Sophia St
50643 E 41 Ave at Saint George St
50644 E 41 Ave at Fraser St
50645 E 41 Ave at Prince Albert St
50646 E 41 Ave at Windsor St
50647 E 41 Ave at Sherbrooke St
50648 E 41 Ave at Knight St
50650 E 41 Ave at Fleming St
50651 E 41 Ave at Bruce St
51121 E 41 Ave at Victoria Dr
51122 E 41 Ave at Gladstone St
51123 E 41 Ave at Nanaimo St
51124 E 41 Ave at Clarendon St
51125 E 41 Ave at Wales St
51126 E 41 Ave at Earles St
51127 E 41 Ave at Killarney St
51128 E 41 Ave at Rupert St
51129 E 41 Ave at Kerr St
51130 E 41 Ave at School Ave
51636 Joyce St at Kingsway
51638 Joyce St at Euclid Ave
51891 Joyce Station Bay 4').
stops(43, '61934 UBC Exchange Bay 6
51479 Wesbrook Mall at Agronomy Rd
51480 Wesbrook Mall at Thunderbird Blvd
51882 W 16 Ave at Wesbrook Mall
50641 W 41 Ave at Dunbar St
50377 W 41 Ave at Carnarvon St
50382 W 41 Ave at West Boulevard
50388 W 41 Ave at Granville St
50804 W 41 Ave at Oak St
59305 W 41 Ave at Ash St
50253 E 41 Ave at Main St
61503 E 41 Ave at Fraser St
50648 E 41 Ave at Knight St
51121 E 41 Ave at Victoria Dr
51124 E 41 Ave at Clarendon St
51128 E 41 Ave at Rupert St
51130 E 41 Ave at School Ave
61485 Joyce Station Bay 5').
stops(44, '59936 UBC Exchange Bay 3
51928 Chancellor Blvd at Western Parkway
51930 Chancellor Blvd at Hamber Rd
50347 W 4 Ave at Tolmie St
50354 W 4 Ave at Alma St
50393 W 4 Ave at Macdonald St
50397 W 4 Ave at Vine St
51941 Burrard St at W 3 Ave
50075 Burrard St at Davie St
61759 Burrard St at Comox St
50029 Burrard St at Nelson St
50030 Burrard St at Robson St
50031 Burrard St at W Georgia St
56558 Burrard Station Bay 5
50076 W Pender St at Burrard St
50077 W Pender St at Granville St
50408 Seymour St at W Hastings St').
stops(49, '61979 UBC Exchange Bay 5
51479 Wesbrook Mall at Agronomy Rd
51480 Wesbrook Mall at Thunderbird Blvd
51483 Wesbrook Mall at Hampton Pl
51882 W 16 Ave at Wesbrook Mall
51883 W 16 Ave at SW Marine Dr
59489 SW Marine Dr at Wesbrook Mall
51887 SW Marine Dr at Kullahun Dr
58650 W 41 Ave at Crown St-
51888 W 41 Ave at Crown St--
51889 W 41 Ave at Wallace St
51890 W 41 Ave at Highbury St
50737 Dunbar St at W 41 Ave
51944 Dunbar St at W 43 Ave
51945 Dunbar St at SW Marine Dr
51946 SW Marine Dr at Blenheim St
51947 SW Marine Dr at Balaclava St
51950 W 49 Ave at Mccleery St
51951 W 49 Ave at Marine Cres
51952 W 49 Ave at Balsam St
51953 W 49 Ave at Yew St
51954 W 49 Ave at Arbutus St
51955 W 49 Ave at East Boulevard
59103 W 49 Ave at Maple St
51957 W 49 Ave at Angus Dr
51958 W 49 Ave at Marguerite St
51959 W 49 Ave at Churchill St
51960 W 49 Ave at Granville St
51961 W 49 Ave at Hudson St
51962 W 49 Ave at Montgomery St
51963 W 49 Ave at Oak St
51965 W 49 Ave at Heather St
51966 W 49 Ave at Tisdall St
51967 W 49 Ave at Cambie St
51968 W 49 Ave at Alberta St
51969 W 49 Ave at Manitoba St
51970 E 49 Ave at Quebec St
51971 E 49 Ave at Main St
51972 E 49 Ave at Prince Edward St
51973 E 49 Ave at Saint George St
51974 E 49 Ave at Frederick St
51975 E 49 Ave at Fraser St
51976 E 49 Ave at Prince Albert St
51977 E 49 Ave at Windsor St
51978 E 49 Ave at Sherbrooke St
51979 E 49 Ave at Knight St
51980 E 49 Ave at Dumfries St
51981 E 49 Ave at Argyle St
51982 E 49 Ave at Commercial St
51983 E 49 Ave at Victoria Dr
51984 E 49 Ave at Gladstone St
51985 E 49 Ave at Nanaimo St
51987 E 49 Ave at Elliott St
51988 E 49 Ave at Vivian St
51989 E 49 Ave at Killarney St
59317 E 49 Ave at Kerr St
60020 E 49 Ave at Doman St
51647 E 49 Ave at Tyne St
51648 E 49 Ave at Arlington St
51992 E 49 Ave at Frontenac St
51993 Imperial St at Boundary Rd
51994 Imperial St at Mandy Ave
59607 Willingdon Extension at Imperial St
51998 Willingdon Ave at Maywood St
51163 Central Blvd at Willingdon Ave
60195 Metrotown Station Bay 10').
stops(84, '60045 UBC Exchange Bay 2
51928 Chancellor Blvd at Western Parkway
51930 Chancellor Blvd at Hamber Rd
50347 W 4 Ave at Tolmie St
50350 W 4 Ave at 4100 Block
50354 W 4 Ave at Alma St
50393 W 4 Ave at Macdonald St
50397 W 4 Ave at Vine St
50400 W 4 Ave at Burrard St
50401 W 4 Ave at Fir St
59752 W 6 Ave at Alder Crossing
59754 W 6 Ave at Moberly Rd
61039 W 2 Ave at Cambie St
59758 W 2 Ave at Columbia St
59760 E 2 Ave at Ontario St
59762 E 2 Ave at Main St
59763 Great Northern Way at Brunswick St
59766 Great Northern Way at Foley St
59767 VCC / Clark Station Load / Unload').
stops(99, '61935 UBC Exchange Bay 4
50268 University Blvd at Allison Rd
50276 W 10 Ave at Sasamat St
58037 W Broadway at Alma St--
50319 W Broadway at Macdonald St
59997 W Broadway at Arbutus St
58503 W Broadway at Granville St--
52099 W Broadway at Heather St
50875 W Broadway at Cambie St
52101 E Broadway at Main St
58614 E Broadway at Fraser St
58499 E Broadway at Clark Dr--
58491 Commercial Dr at N Grandview Highway
58620 N Grandview Highway at Commercial Dr').
stops(258, '58292 UBC Exchange Bay 13
50268 University Blvd at Allison Rd
50270 University Blvd at 5300 Block
50271 University Blvd at 5100 Block
54606 Homer St at W Georgia St').
stops(480, '61934 UBC Exchange Bay 6
51479 Wesbrook Mall at Agronomy Rd
51480 Wesbrook Mall at Thunderbird Blvd
51882 W 16 Ave at Wesbrook Mall
50641 W 41 Ave at Dunbar St
50377 W 41 Ave at Carnarvon St
50382 W 41 Ave at West Boulevard
50388 W 41 Ave at Granville St
50856 Granville St at W 49 Ave
50862 Granville St at W 63 Ave
50865 Granville St at W 70 Ave
52220 Marpole Loop Bay 3
54837 W 71 Ave at Oak St
61321 Bridgeport Station Bay 1 Unloading Only').
stops(n17, '61702 UBC Exchange Bay 12
50268 University Blvd at Allison Rd
57938 University Blvd at Acadia Rd
50270 University Blvd at 5300 Block
50271 University Blvd at 5100 Block
50274 W 10 Ave at Blanca St
50275 W 10 Ave at Tolmie St
50276 W 10 Ave at Sasamat St
50277 W 10 Ave at Trimble St
50278 W 10 Ave at Discovery St
50279 W 10 Ave at Courtenay St
50280 W 10 Ave at Camosun St
50281 W 10 Ave at Crown St
50282 W 10 Ave at Wallace St
50283 W 10 Ave at Highbury St
50314 Alma St at W 10 Ave
50315 W Broadway at Alma St-
60725 W Broadway at Collingwood St
60726 W Broadway at Blenheim St
60727 W Broadway St at Balaclava St
60728 W Broadway at Bayswater St
50319 W Broadway at Macdonald St
50320 W Broadway at Trafalgar St
50321 W Broadway at Larch St
50322 W Broadway at Vine St
50323 W Broadway at Arbutus St
50324 W Broadway at Maple St
50325 W Broadway at Burrard St
50871 W Broadway at Fir St-
50216 Granville St at W Broadway
50217 Granville St at W 7 Ave
50218 Granville St at W 5 Ave
51004 Seymour St at Davie St
51006 Seymour St at Nelson St
51007 Seymour St at Robson St
61519 Seymour St at Dunsmuir St
59831 Howe St at W Pender St Bay 2').
stops(c18, '61900 Agronomy Rd at West Mall
61895 Thunderbird Blvd at Eagles Dr
61832 East Mall at Eagles Dr
61894 Birney Ave at Ross Dr
61580 Wesbrook Mall at Birney Ave
51600 Wesbrook Mall at 2900 Block
59715 Wesbrook Mall at Thunderbird Blvd
58606 Wesbrook Mall at 2100 Block
60040 UBC Exchange Bay 1').
stops(c20, '59941 West Mall at Hawthorn Lane
59940 Stadium Rd at West Mall
59964 NW Marine Dr at Agronomy Rd
59587 Lower Mall at University Blvd
51925 NW Marine Dr at West Mall
59957 NW Marine Dr at East Mall
59946 Wesbrook Mall at Iona Dr
60040 UBC Exchange Bay 1').
time_reln(4, 707).
time_reln(4, 722).
time_reln(4, 737).
time_reln(4, 752).
time_reln(4, 807).
time_reln(4, 821).
time_reln(4, 836).
time_reln(4, 851).
time_reln(4, 906).
time_reln(4, 921).
time_reln(4, 937).
time_reln(4, 952).
time_reln(4, 1007).
time_reln(4, 1022).
time_reln(4, 1037).
time_reln(4, 1052).
time_reln(4, 1107).
time_reln(4, 1122).
time_reln(4, 1136).
time_reln(4, 1151).
time_reln(4, 1206).
time_reln(4, 1221).
time_reln(4, 1236).
time_reln(4, 1251).
time_reln(4, 1305).
time_reln(4, 1320).
time_reln(4, 1335).
time_reln(4, 1350).
time_reln(4, 1405).
time_reln(4, 1420).
time_reln(4, 1435).
time_reln(4, 1447).
time_reln(4, 1459).
time_reln(4, 1510).
time_reln(4, 1522).
time_reln(4, 1534).
time_reln(4, 1545).
time_reln(4, 1557).
time_reln(4, 1609).
time_reln(4, 1621).
time_reln(4, 1633).
time_reln(4, 1645).
time_reln(4, 1657).
time_reln(4, 1719).
time_reln(4, 1739).
time_reln(4, 1759).
time_reln(4, 1819).
time_reln(4, 1839).
time_reln(4, 1859).
time_reln(4, 1920).
time_reln(4, 1943).
time_reln(4, 2003).
time_reln(4, 2023).
time_reln(4, 2043).
time_reln(4, 2103).
time_reln(4, 2123).
time_reln(4, 2143).
time_reln(4, 2203).
time_reln(4, 2223).
time_reln(4, 2247).
time_reln(4, 2307).
time_reln(4, 2327).
time_reln(4, 2352).
time_reln(9, 745).
time_reln(9, 756).
time_reln(9, 805).
time_reln(9, 816).
time_reln(9, 828).
time_reln(9, 841).
time_reln(9, 854).
time_reln(9, 907).
time_reln(9, 920).
time_reln(9, 933).
time_reln(9, 947).
time_reln(9, 1000).
time_reln(9, 1013).
time_reln(9, 1026).
time_reln(9, 1038).
time_reln(9, 1051).
time_reln(9, 1510).
time_reln(9, 1520).
time_reln(9, 1528).
time_reln(9, 1535).
time_reln(9, 1543).
time_reln(9, 1551).
time_reln(9, 1559).
time_reln(9, 1609).
time_reln(9, 1619).
time_reln(9, 1629).
time_reln(9, 1639).
time_reln(9, 1649).
time_reln(9, 1659).
time_reln(9, 1709).
time_reln(9, 1719).
time_reln(9, 1733).
time_reln(9, 1746).
time_reln(9, 1759).
time_reln(9, 1806).
time_reln(9, 1813).
time_reln(9, 1822).
time_reln(14, 550).
time_reln(14, 608).
time_reln(14, 618).
time_reln(14, 628).
time_reln(14, 642).
time_reln(14, 650).
time_reln(14, 706).
time_reln(14, 714).
time_reln(14, 720).
time_reln(14, 730).
time_reln(14, 740).
time_reln(14, 750).
time_reln(14, 800).
time_reln(14, 810).
time_reln(14, 820).
time_reln(14, 830).
time_reln(14, 840).
time_reln(14, 850).
time_reln(14, 900).
time_reln(14, 912).
time_reln(14, 924).
time_reln(14, 936).
time_reln(14, 948).
time_reln(14, 1000).
time_reln(14, 1012).
time_reln(14, 1024).
time_reln(14, 1036).
time_reln(14, 1048).
time_reln(14, 1100).
time_reln(14, 1112).
time_reln(14, 1124).
time_reln(14, 1135).
time_reln(14, 1147).
time_reln(14, 1159).
time_reln(14, 1210).
time_reln(14, 1222).
time_reln(14, 1234).
time_reln(14, 1246).
time_reln(14, 1258).
time_reln(14, 1309).
time_reln(14, 1321).
time_reln(14, 1333).
time_reln(14, 1345).
time_reln(14, 1357).
time_reln(14, 1409).
time_reln(14, 1421).
time_reln(14, 1433).
time_reln(14, 1445).
time_reln(14, 1457).
time_reln(14, 1508).
time_reln(14, 1518).
time_reln(14, 1528).
time_reln(14, 1538).
time_reln(14, 1548).
time_reln(14, 1558).
time_reln(14, 1608).
time_reln(14, 1618).
time_reln(14, 1628).
time_reln(14, 1638).
time_reln(14, 1648).
time_reln(14, 1658).
time_reln(14, 1708).
time_reln(14, 1718).
time_reln(14, 1731).
time_reln(14, 1743).
time_reln(14, 1755).
time_reln(14, 1803).
time_reln(14, 1809).
time_reln(14, 1821).
time_reln(14, 1833).
time_reln(14, 1844).
time_reln(14, 1845).
time_reln(14, 1902).
time_reln(14, 1917).
time_reln(14, 1918).
time_reln(14, 1932).
time_reln(14, 1947).
time_reln(14, 1950).
time_reln(14, 2002).
time_reln(14, 2017).
time_reln(14, 2154).
time_reln(14, 2239).
time_reln(14, 119).
time_reln(14, 149).
time_reln(14, 218).
time_reln(14, 225).
time_reln(25, 545).
time_reln(25, 601).
time_reln(25, 618).
time_reln(25, 633).
time_reln(25, 639).
time_reln(25, 645).
time_reln(25, 651).
time_reln(25, 657).
time_reln(25, 703).
time_reln(25, 709).
time_reln(25, 715).
time_reln(25, 721).
time_reln(25, 727).
time_reln(25, 730).
time_reln(25, 736).
time_reln(25, 742).
time_reln(25, 747).
time_reln(25, 753).
time_reln(25, 758).
time_reln(25, 804).
time_reln(25, 810).
time_reln(25, 815).
time_reln(25, 821).
time_reln(25, 826).
time_reln(25, 832).
time_reln(25, 838).
time_reln(25, 844).
time_reln(25, 850).
time_reln(25, 859).
time_reln(25, 905).
time_reln(25, 911).
time_reln(25, 919).
time_reln(25, 925).
time_reln(25, 931).
time_reln(25, 939).
time_reln(25, 947).
time_reln(25, 955).
time_reln(25, 1004).
time_reln(25, 1012).
time_reln(25, 1020).
time_reln(25, 1028).
time_reln(25, 1036).
time_reln(25, 1044).
time_reln(25, 1053).
time_reln(25, 1103).
time_reln(25, 1113).
time_reln(25, 1123).
time_reln(25, 1132).
time_reln(25, 1142).
time_reln(25, 1152).
time_reln(25, 1201).
time_reln(25, 1211).
time_reln(25, 1221).
time_reln(25, 1231).
time_reln(25, 1241).
time_reln(25, 1251).
time_reln(25, 1301).
time_reln(25, 1311).
time_reln(25, 1321).
time_reln(25, 1331).
time_reln(25, 1341).
time_reln(25, 1351).
time_reln(25, 1400).
time_reln(25, 1410).
time_reln(25, 1418).
time_reln(25, 1426).
time_reln(25, 1432).
time_reln(25, 1439).
time_reln(25, 1447).
time_reln(25, 1450).
time_reln(25, 1456).
time_reln(25, 1502).
time_reln(25, 1505).
time_reln(25, 1511).
time_reln(25, 1517).
time_reln(25, 1523).
time_reln(25, 1529).
time_reln(25, 1536).
time_reln(25, 1542).
time_reln(25, 1550).
time_reln(25, 1556).
time_reln(25, 1602).
time_reln(25, 1609).
time_reln(25, 1621).
time_reln(25, 1627).
time_reln(25, 1633).
time_reln(25, 1641).
time_reln(25, 1647).
time_reln(25, 1654).
time_reln(25, 1700).
time_reln(25, 1707).
time_reln(25, 1715).
time_reln(25, 1722).
time_reln(25, 1730).
time_reln(25, 1737).
time_reln(25, 1745).
time_reln(25, 1755).
time_reln(25, 1805).
time_reln(25, 1815).
time_reln(25, 1826).
time_reln(25, 1836).
time_reln(25, 1846).
time_reln(25, 1856).
time_reln(25, 1907).
time_reln(25, 1918).
time_reln(25, 1932).
time_reln(25, 1943).
time_reln(25, 1955).
time_reln(25, 2008).
time_reln(25, 2105).
time_reln(25, 2135).
time_reln(25, 2205).
time_reln(25, 2240).
time_reln(25, 2308).
time_reln(25, 2338).
time_reln(25, 0008).
time_reln(33, 601).
time_reln(33, 629).
time_reln(33, 659).
time_reln(33, 710).
time_reln(33, 721).
time_reln(33, 732).
time_reln(33, 744).
time_reln(33, 756).
time_reln(33, 813).
time_reln(33, 827).
time_reln(33, 842).
time_reln(33, 857).
time_reln(33, 912).
time_reln(33, 928).
time_reln(33, 942).
time_reln(33, 957).
time_reln(33, 1012).
time_reln(33, 1027).
time_reln(33, 1042).
time_reln(33, 1057).
time_reln(33, 1112).
time_reln(33, 1127).
time_reln(33, 1142).
time_reln(33, 1157).
time_reln(33, 1212).
time_reln(33, 1227).
time_reln(33, 1242).
time_reln(33, 1257).
time_reln(33, 1310).
time_reln(33, 1325).
time_reln(33, 1340).
time_reln(33, 1355).
time_reln(33, 1409).
time_reln(33, 1424).
time_reln(33, 1439).
time_reln(33, 1452).
time_reln(33, 1458).
time_reln(33, 1514).
time_reln(33, 1524).
time_reln(33, 1534).
time_reln(33, 1544).
time_reln(33, 1554).
time_reln(33, 1605).
time_reln(33, 1626).
time_reln(33, 1636).
time_reln(33, 1646).
time_reln(33, 1656).
time_reln(33, 1708).
time_reln(33, 1718).
time_reln(33, 1729).
time_reln(33, 1739).
time_reln(33, 1749).
time_reln(33, 1801).
time_reln(33, 1815).
time_reln(33, 1834).
time_reln(33, 1849).
time_reln(33, 1908).
time_reln(33, 1927).
time_reln(33, 1959).
time_reln(33, 2029).
time_reln(33, 2059).
time_reln(33, 2129).
time_reln(33, 2159).
time_reln(33, 2229).
time_reln(33, 2259).
time_reln(41, 703).
time_reln(41, 724).
time_reln(41, 730).
time_reln(41, 737).
time_reln(41, 744).
time_reln(41, 751).
time_reln(41, 758).
time_reln(41, 805).
time_reln(41, 812).
time_reln(41, 818).
time_reln(41, 827).
time_reln(41, 833).
time_reln(41, 839).
time_reln(41, 845).
time_reln(41, 851).
time_reln(41, 857).
time_reln(41, 903).
time_reln(41, 909).
time_reln(41, 915).
time_reln(41, 922).
time_reln(41, 928).
time_reln(41, 935).
time_reln(41, 943).
time_reln(41, 949).
time_reln(41, 956).
time_reln(41, 1003).
time_reln(41, 1010).
time_reln(41, 1018).
time_reln(41, 1025).
time_reln(41, 1033).
time_reln(41, 1040).
time_reln(41, 1048).
time_reln(41, 1054).
time_reln(41, 1102).
time_reln(41, 1109).
time_reln(41, 1117).
time_reln(41, 1124).
time_reln(41, 1132).
time_reln(41, 1139).
time_reln(41, 1147).
time_reln(41, 1151).
time_reln(41, 1159).
time_reln(41, 1206).
time_reln(41, 1214).
time_reln(41, 1220).
time_reln(41, 1228).
time_reln(41, 1235).
time_reln(41, 1244).
time_reln(41, 1251).
time_reln(41, 1259).
time_reln(41, 1306).
time_reln(41, 1313).
time_reln(41, 1319).
time_reln(41, 1327).
time_reln(41, 1334).
time_reln(41, 1342).
time_reln(41, 1348).
time_reln(41, 1356).
time_reln(41, 1404).
time_reln(41, 1412).
time_reln(41, 1415).
time_reln(41, 1422).
time_reln(41, 1429).
time_reln(41, 1436).
time_reln(41, 1441).
time_reln(41, 1448).
time_reln(41, 1455).
time_reln(41, 1501).
time_reln(41, 1508).
time_reln(41, 1515).
time_reln(41, 1522).
time_reln(41, 1529).
time_reln(41, 1536).
time_reln(41, 1543).
time_reln(41, 1550).
time_reln(41, 1559).
time_reln(41, 1606).
time_reln(41, 1613).
time_reln(41, 1620).
time_reln(41, 1626).
time_reln(41, 1631).
time_reln(41, 1637).
time_reln(41, 1643).
time_reln(41, 1649).
time_reln(41, 1656).
time_reln(41, 1702).
time_reln(41, 1709).
time_reln(41, 1718).
time_reln(41, 1725).
time_reln(41, 1731).
time_reln(41, 1738).
time_reln(41, 1748).
time_reln(41, 1755).
time_reln(41, 1802).
time_reln(41, 1809).
time_reln(41, 1824).
time_reln(41, 1831).
time_reln(41, 1838).
time_reln(41, 1845).
time_reln(41, 1853).
time_reln(41, 1902).
time_reln(41, 1912).
time_reln(41, 1922).
time_reln(41, 1932).
time_reln(41, 1942).
time_reln(41, 1952).
time_reln(41, 2003).
time_reln(41, 2013).
time_reln(41, 2023).
time_reln(41, 2034).
time_reln(41, 2047).
time_reln(41, 2101).
time_reln(41, 2115).
time_reln(41, 2130).
time_reln(41, 2145).
time_reln(41, 2201).
time_reln(41, 2233).
time_reln(41, 2303).
time_reln(41, 2335).
time_reln(41, 0006).
time_reln(43, 615).
time_reln(43, 645).
time_reln(43, 700).
time_reln(43, 712).
time_reln(43, 722).
time_reln(43, 732).
time_reln(43, 742).
time_reln(43, 752).
time_reln(43, 800).
time_reln(43, 808).
time_reln(43, 815).
time_reln(43, 823).
time_reln(43, 833).
time_reln(43, 842).
time_reln(43, 852).
time_reln(43, 900).
time_reln(43, 908).
time_reln(43, 916).
time_reln(43, 924).
time_reln(43, 932).
time_reln(43, 940).
time_reln(43, 955).
time_reln(43, 1010).
time_reln(43, 1025).
time_reln(43, 1040).
time_reln(43, 1055).
time_reln(43, 1110).
time_reln(43, 1125).
time_reln(43, 1140).
time_reln(43, 1155).
time_reln(43, 1210).
time_reln(43, 1225).
time_reln(43, 1240).
time_reln(43, 1255).
time_reln(43, 1310).
time_reln(43, 1325).
time_reln(43, 1340).
time_reln(43, 1355).
time_reln(43, 1410).
time_reln(43, 1425).
time_reln(43, 1440).
time_reln(43, 1455).
time_reln(43, 1508).
time_reln(43, 1521).
time_reln(43, 1534).
time_reln(43, 1544).
time_reln(43, 1554).
time_reln(43, 1604).
time_reln(43, 1614).
time_reln(43, 1623).
time_reln(43, 1632).
time_reln(43, 1641).
time_reln(43, 1652).
time_reln(43, 1659).
time_reln(43, 1708).
time_reln(43, 1717).
time_reln(43, 1726).
time_reln(43, 1735).
time_reln(43, 1744).
time_reln(43, 1753).
time_reln(43, 1802).
time_reln(43, 1812).
time_reln(43, 1822).
time_reln(43, 1831).
time_reln(43, 1841).
time_reln(43, 1853).
time_reln(43, 1905).
time_reln(43, 1922).
time_reln(43, 1935).
time_reln(44, 750).
time_reln(44, 807).
time_reln(44, 819).
time_reln(44, 830).
time_reln(44, 840).
time_reln(44, 850).
time_reln(44, 900).
time_reln(44, 910).
time_reln(44, 922).
time_reln(44, 934).
time_reln(44, 952).
time_reln(44, 1012).
time_reln(44, 1032).
time_reln(44, 1052).
time_reln(44, 1112).
time_reln(44, 1132).
time_reln(44, 1152).
time_reln(44, 1212).
time_reln(44, 1232).
time_reln(44, 1252).
time_reln(44, 1312).
time_reln(44, 1332).
time_reln(44, 1352).
time_reln(44, 1412).
time_reln(44, 1432).
time_reln(44, 1452).
time_reln(44, 1506).
time_reln(44, 1519).
time_reln(44, 1532).
time_reln(44, 1544).
time_reln(44, 1556).
time_reln(44, 1608).
time_reln(44, 1620).
time_reln(44, 1632).
time_reln(44, 1644).
time_reln(44, 1658).
time_reln(44, 1714).
time_reln(44, 1729).
time_reln(44, 1744).
time_reln(44, 1759).
time_reln(44, 1814).
time_reln(44, 1829).
time_reln(44, 1844).
time_reln(44, 1859).
time_reln(44, 1919).
time_reln(44, 1949).
time_reln(49, 743).
time_reln(49, 746).
time_reln(49, 752).
time_reln(49, 801).
time_reln(49, 807).
time_reln(49, 813).
time_reln(49, 819).
time_reln(49, 825).
time_reln(49, 835).
time_reln(49, 842).
time_reln(49, 849).
time_reln(49, 856).
time_reln(49, 903).
time_reln(49, 910).
time_reln(49, 917).
time_reln(49, 924).
time_reln(49, 931).
time_reln(49, 940).
time_reln(49, 947).
time_reln(49, 955).
time_reln(49, 1002).
time_reln(49, 1010).
time_reln(49, 1017).
time_reln(49, 1025).
time_reln(49, 1033).
time_reln(49, 1039).
time_reln(49, 1047).
time_reln(49, 1055).
time_reln(49, 1102).
time_reln(49, 1110).
time_reln(49, 1118).
time_reln(49, 1126).
time_reln(49, 1134).
time_reln(49, 1142).
time_reln(49, 1150).
time_reln(49, 1158).
time_reln(49, 1206).
time_reln(49, 1214).
time_reln(49, 1222).
time_reln(49, 1230).
time_reln(49, 1238).
time_reln(49, 1246).
time_reln(49, 1254).
time_reln(49, 1303).
time_reln(49, 1311).
time_reln(49, 1320).
time_reln(49, 1328).
time_reln(49, 1336).
time_reln(49, 1343).
time_reln(49, 1350).
time_reln(49, 1356).
time_reln(49, 1403).
time_reln(49, 1409).
time_reln(49, 1416).
time_reln(49, 1422).
time_reln(49, 1429).
time_reln(49, 1432).
time_reln(49, 1444).
time_reln(49, 1454).
time_reln(49, 1458).
time_reln(49, 1509).
time_reln(49, 1514).
time_reln(49, 1524).
time_reln(49, 1528).
time_reln(49, 1533).
time_reln(49, 1537).
time_reln(49, 1544).
time_reln(49, 1548).
time_reln(49, 1553).
time_reln(49, 1557).
time_reln(49, 1602).
time_reln(49, 1606).
time_reln(49, 1612).
time_reln(49, 1621).
time_reln(49, 1625).
time_reln(49, 1630).
time_reln(49, 1634).
time_reln(49, 1639).
time_reln(49, 1643).
time_reln(49, 1648).
time_reln(49, 1652).
time_reln(49, 1657).
time_reln(49, 1701).
time_reln(49, 1706).
time_reln(49, 1712).
time_reln(49, 1718).
time_reln(49, 1725).
time_reln(49, 1731).
time_reln(49, 1738).
time_reln(49, 1746).
time_reln(49, 1753).
time_reln(49, 1800).
time_reln(49, 1807).
time_reln(49, 1813).
time_reln(49, 1820).
time_reln(49, 1832).
time_reln(49, 1842).
time_reln(49, 1852).
time_reln(49, 1903).
time_reln(49, 1913).
time_reln(49, 1924).
time_reln(49, 1936).
time_reln(49, 2000).
time_reln(49, 2024).
time_reln(49, 2052).
time_reln(49, 2116).
time_reln(84, 559).
time_reln(84, 614).
time_reln(84, 629).
time_reln(84, 639).
time_reln(84, 645).
time_reln(84, 650).
time_reln(84, 656).
time_reln(84, 701).
time_reln(84, 707).
time_reln(84, 712).
time_reln(84, 718).
time_reln(84, 723).
time_reln(84, 729).
time_reln(84, 734).
time_reln(84, 740).
time_reln(84, 746).
time_reln(84, 752).
time_reln(84, 757).
time_reln(84, 803).
time_reln(84, 809).
time_reln(84, 816).
time_reln(84, 822).
time_reln(84, 828).
time_reln(84, 834).
time_reln(84, 840).
time_reln(84, 847).
time_reln(84, 854).
time_reln(84, 902).
time_reln(84, 909).
time_reln(84, 918).
time_reln(84, 925).
time_reln(84, 933).
time_reln(84, 941).
time_reln(84, 949).
time_reln(84, 957).
time_reln(84, 1007).
time_reln(84, 1019).
time_reln(84, 1031).
time_reln(84, 1042).
time_reln(84, 1054).
time_reln(84, 1106).
time_reln(84, 1118).
time_reln(84, 1130).
time_reln(84, 1141).
time_reln(84, 1153).
time_reln(84, 1205).
time_reln(84, 1217).
time_reln(84, 1229).
time_reln(84, 1242).
time_reln(84, 1254).
time_reln(84, 1306).
time_reln(84, 1318).
time_reln(84, 1330).
time_reln(84, 1342).
time_reln(84, 1354).
time_reln(84, 1406).
time_reln(84, 1417).
time_reln(84, 1429).
time_reln(84, 1441).
time_reln(84, 1453).
time_reln(84, 1500).
time_reln(84, 1507).
time_reln(84, 1513).
time_reln(84, 1520).
time_reln(84, 1526).
time_reln(84, 1533).
time_reln(84, 1539).
time_reln(84, 1545).
time_reln(84, 1551).
time_reln(84, 1557).
time_reln(84, 1603).
time_reln(84, 1609).
time_reln(84, 1615).
time_reln(84, 1621).
time_reln(84, 1627).
time_reln(84, 1633).
time_reln(84, 1639).
time_reln(84, 1645).
time_reln(84, 1652).
time_reln(84, 1658).
time_reln(84, 1705).
time_reln(84, 1711).
time_reln(84, 1718).
time_reln(84, 1725).
time_reln(84, 1733).
time_reln(84, 1741).
time_reln(84, 1749).
time_reln(84, 1757).
time_reln(84, 1807).
time_reln(84, 1818).
time_reln(84, 1828).
time_reln(84, 1843).
time_reln(84, 1858).
time_reln(84, 1914).
time_reln(84, 1929).
time_reln(84, 1949).
time_reln(84, 2009).
time_reln(84, 2029).
time_reln(84, 2049).
time_reln(84, 2109).
time_reln(84, 2129).
time_reln(84, 2149).
time_reln(84, 2229).
time_reln(99, 628).
time_reln(99, 635).
time_reln(99, 643).
time_reln(99, 650).
time_reln(99, 654).
time_reln(99, 658).
time_reln(99, 702).
time_reln(99, 706).
time_reln(99, 710).
time_reln(99, 714).
time_reln(99, 718).
time_reln(99, 722).
time_reln(99, 726).
time_reln(99, 730).
time_reln(99, 734).
time_reln(99, 738).
time_reln(99, 739).
time_reln(99, 742).
time_reln(99, 746).
time_reln(99, 749).
time_reln(99, 752).
time_reln(99, 755).
time_reln(99, 758).
time_reln(99, 801).
time_reln(99, 804).
time_reln(99, 807).
time_reln(99, 810).
time_reln(99, 813).
time_reln(99, 816).
time_reln(99, 819).
time_reln(99, 822).
time_reln(99, 825).
time_reln(99, 828).
time_reln(99, 831).
time_reln(99, 834).
time_reln(99, 837).
time_reln(99, 840).
time_reln(99, 843).
time_reln(99, 846).
time_reln(99, 849).
time_reln(99, 852).
time_reln(99, 856).
time_reln(99, 900).
time_reln(99, 904).
time_reln(99, 908).
time_reln(99, 912).
time_reln(99, 916).
time_reln(99, 920).
time_reln(99, 924).
time_reln(99, 928).
time_reln(99, 932).
time_reln(99, 936).
time_reln(99, 940).
time_reln(99, 944).
time_reln(99, 948).
time_reln(99, 953).
time_reln(99, 957).
time_reln(99, 1002).
time_reln(99, 1006).
time_reln(99, 1011).
time_reln(99, 1015).
time_reln(99, 1020).
time_reln(99, 1024).
time_reln(99, 1029).
time_reln(99, 1033).
time_reln(99, 1038).
time_reln(99, 1042).
time_reln(99, 1047).
time_reln(99, 1051).
time_reln(99, 1056).
time_reln(99, 1100).
time_reln(99, 1105).
time_reln(99, 1109).
time_reln(99, 1114).
time_reln(99, 1118).
time_reln(99, 1123).
time_reln(99, 1127).
time_reln(99, 1132).
time_reln(99, 1136).
time_reln(99, 1140).
time_reln(99, 1145).
time_reln(99, 1149).
time_reln(99, 1154).
time_reln(99, 1158).
time_reln(99, 1203).
time_reln(99, 1207).
time_reln(99, 1212).
time_reln(99, 1216).
time_reln(99, 1221).
time_reln(99, 1225).
time_reln(99, 1230).
time_reln(99, 1234).
time_reln(99, 1239).
time_reln(99, 1243).
time_reln(99, 1248).
time_reln(99, 1252).
time_reln(99, 1257).
time_reln(99, 1301).
time_reln(99, 1306).
time_reln(99, 1310).
time_reln(99, 1315).
time_reln(99, 1319).
time_reln(99, 1324).
time_reln(99, 1328).
time_reln(99, 1331).
time_reln(99, 1337).
time_reln(99, 1342).
time_reln(99, 1346).
time_reln(99, 1351).
time_reln(99, 1355).
time_reln(99, 1400).
time_reln(99, 1404).
time_reln(99, 1409).
time_reln(99, 1413).
time_reln(99, 1418).
time_reln(99, 1422).
time_reln(99, 1427).
time_reln(99, 1431).
time_reln(99, 1435).
time_reln(99, 1439).
time_reln(99, 1443).
time_reln(99, 1447).
time_reln(99, 1451).
time_reln(99, 1454).
time_reln(99, 1457).
time_reln(99, 1500).
time_reln(99, 1503).
time_reln(99, 1506).
time_reln(99, 1509).
time_reln(99, 1512).
time_reln(99, 1515).
time_reln(99, 1518).
time_reln(99, 1521).
time_reln(99, 1524).
time_reln(99, 1527).
time_reln(99, 1530).
time_reln(99, 1533).
time_reln(99, 1536).
time_reln(99, 1539).
time_reln(99, 1542).
time_reln(99, 1545).
time_reln(99, 1548).
time_reln(99, 1551).
time_reln(99, 1554).
time_reln(99, 1557).
time_reln(99, 1600).
time_reln(99, 1603).
time_reln(99, 1606).
time_reln(99, 1609).
time_reln(99, 1612).
time_reln(99, 1615).
time_reln(99, 1618).
time_reln(99, 1621).
time_reln(99, 1624).
time_reln(99, 1627).
time_reln(99, 1630).
time_reln(99, 1633).
time_reln(99, 1636).
time_reln(99, 1639).
time_reln(99, 1642).
time_reln(99, 1645).
time_reln(99, 1648).
time_reln(99, 1651).
time_reln(99, 1654).
time_reln(99, 1657).
time_reln(99, 1700).
time_reln(99, 1703).
time_reln(99, 1706).
time_reln(99, 1709).
time_reln(99, 1712).
time_reln(99, 1715).
time_reln(99, 1718).
time_reln(99, 1721).
time_reln(99, 1724).
time_reln(99, 1727).
time_reln(99, 1730).
time_reln(99, 1733).
time_reln(99, 1736).
time_reln(99, 1739).
time_reln(99, 1742).
time_reln(99, 1745).
time_reln(99, 1748).
time_reln(99, 1751).
time_reln(99, 1754).
time_reln(99, 1757).
time_reln(99, 1800).
time_reln(99, 1803).
time_reln(99, 1806).
time_reln(99, 1809).
time_reln(99, 1812).
time_reln(99, 1816).
time_reln(99, 1820).
time_reln(99, 1824).
time_reln(99, 1828).
time_reln(99, 1832).
time_reln(99, 1836).
time_reln(99, 1840).
time_reln(99, 1845).
time_reln(99, 1850).
time_reln(99, 1855).
time_reln(99, 1900).
time_reln(99, 1905).
time_reln(99, 1910).
time_reln(99, 1915).
time_reln(99, 1920).
time_reln(99, 1925).
time_reln(99, 1930).
time_reln(99, 1935).
time_reln(99, 1940).
time_reln(99, 1945).
time_reln(99, 1950).
time_reln(99, 1955).
time_reln(99, 2001).
time_reln(99, 2007).
time_reln(99, 2013).
time_reln(99, 2019).
time_reln(99, 2025).
time_reln(99, 2031).
time_reln(99, 2037).
time_reln(99, 2043).
time_reln(99, 2049).
time_reln(99, 2055).
time_reln(99, 2102).
time_reln(99, 2110).
time_reln(99, 2117).
time_reln(99, 2125).
time_reln(99, 2132).
time_reln(99, 2140).
time_reln(99, 2147).
time_reln(99, 2155).
time_reln(99, 2202).
time_reln(99, 2212).
time_reln(99, 2222).
time_reln(99, 2232).
time_reln(99, 2242).
time_reln(99, 2252).
time_reln(99, 2302).
time_reln(99, 2312).
time_reln(99, 2322).
time_reln(99, 2332).
time_reln(99, 2342).
time_reln(99, 2352).
time_reln(99, 0003).
time_reln(99, 0018).
time_reln(99, 0034).
time_reln(99, 0049).
time_reln(99, 104).
time_reln(99, 118).
time_reln(99, 140).
time_reln(99, 210).
time_reln(258, 1508).
time_reln(258, 1708).
time_reln(480, 650).
time_reln(480, 720).
time_reln(480, 745).
time_reln(480, 805).
time_reln(480, 822).
time_reln(480, 838).
time_reln(480, 854).
time_reln(480, 910).
time_reln(480, 925).
time_reln(480, 940).
time_reln(480, 955).
time_reln(480, 1010).
time_reln(480, 1025).
time_reln(480, 1040).
time_reln(480, 1055).
time_reln(480, 1110).
time_reln(480, 1125).
time_reln(480, 1140).
time_reln(480, 1155).
time_reln(480, 1210).
time_reln(480, 1225).
time_reln(480, 1240).
time_reln(480, 1255).
time_reln(480, 1310).
time_reln(480, 1325).
time_reln(480, 1340).
time_reln(480, 1355).
time_reln(480, 1410).
time_reln(480, 1425).
time_reln(480, 1440).
time_reln(480, 1455).
time_reln(480, 1500).
time_reln(480, 1505).
time_reln(480, 1510).
time_reln(480, 1520).
time_reln(480, 1530).
time_reln(480, 1542).
time_reln(480, 1554).
time_reln(480, 1604).
time_reln(480, 1614).
time_reln(480, 1619).
time_reln(480, 1627).
time_reln(480, 1634).
time_reln(480, 1644).
time_reln(480, 1654).
time_reln(480, 1704).
time_reln(480, 1715).
time_reln(480, 1726).
time_reln(480, 1736).
time_reln(480, 1746).
time_reln(480, 1756).
time_reln(480, 1809).
time_reln(480, 1821).
time_reln(480, 1836).
time_reln(480, 1851).
time_reln(480, 1908).
time_reln(480, 1928).
time_reln(480, 1948).
time_reln(480, 2008).
time_reln(480, 2038).
time_reln(480, 2108).
time_reln(480, 2138).
time_reln(n17, 138).
time_reln(n17, 208).
time_reln(n17, 238).
time_reln(c18, 700).
time_reln(c18, 730).
time_reln(c18, 800).
time_reln(c18, 830).
time_reln(c18, 900).
time_reln(c18, 930).
time_reln(c18, 1000).
time_reln(c18, 1030).
time_reln(c18, 1100).
time_reln(c18, 1130).
time_reln(c18, 1200).
time_reln(c18, 1230).
time_reln(c18, 1300).
time_reln(c18, 1330).
time_reln(c18, 1400).
time_reln(c18, 1430).
time_reln(c18, 1500).
time_reln(c18, 1530).
time_reln(c18, 1600).
time_reln(c18, 1630).
time_reln(c18, 1700).
time_reln(c18, 1730).
time_reln(c18, 1800).
time_reln(c18, 1830).
time_reln(c18, 1900).
time_reln(c18, 1930).
time_reln(c18, 2000).
time_reln(c18, 2030).
time_reln(c18, 2100).
time_reln(c18, 2130).
time_reln(c18, 2200).
time_reln(c18, 2230).
time_reln(c18, 2300).
time_reln(c18, 2330).
time_reln(c18, 0000).
time_reln(c18, 0030).
time_reln(c20, 715).
time_reln(c20, 745).
time_reln(c20, 815).
time_reln(c20, 845).
time_reln(c20, 915).
time_reln(c20, 945).
time_reln(c20, 1015).
time_reln(c20, 1045).
time_reln(c20, 1115).
time_reln(c20, 1145).
time_reln(c20, 1215).
time_reln(c20, 1245).
time_reln(c20, 1315).
time_reln(c20, 1345).
time_reln(c20, 1415).
time_reln(c20, 1445).
time_reln(c20, 1515).
time_reln(c20, 1545).
time_reln(c20, 1615).
time_reln(c20, 1645).
time_reln(c20, 1715).
time_reln(c20, 1745).
time_reln(c20, 1815).
time_reln(c20, 1845).
time_reln(c20, 1915).
time_reln(c20, 1945).
time_reln(c20, 2015).
time_reln(c20, 2045).
time_reln(c20, 2115).
time_reln(c20, 2145).
time_reln(c20, 2215).
time_reln(c20, 2245).
time_reln(c20, 2315).
time_reln(c20, 2345).
time_reln(c20, 0015).



