0.6::str(dry).
0.3::str(wet).
0.1::str(snow_covered).

0.2::flw. % volante  desgastado
0.95::b.  % lâmpada  funcionando
0.98::k.  % cabo funcionando

% r (dínamo deslizante)
0.1::r :- str(dry), flw.
0.05::r :- str(dry), \+flw.
0.3::r :- str(wet), flw.
0.1::r :- str(wet), \+flw.
0.5::r :- str(snow_covered), flw.
0.2::r :- str(snow_covered), \+flw.

% v (dínamo mostra tensão)
0.9::v :- r.
0.2::v :- \+r.

% li (luz ligada)
0.99::li :- v, b, k.
0.01::li :- v, b, \+k.
0.01::li :- v, \+b, k.
0.001::li :- v, \+b, \+k.
0.3::li :- \+v, b, k.
0.005::li :- \+v, b, \+k.
0.005::li :- \+v, \+b, k.
0.0::li :- \+v, \+b, \+k.

evidence(str(snow_covered)).

% p(v | str = snow_covered)
query(v).
