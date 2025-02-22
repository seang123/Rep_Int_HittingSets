:- [tp].

% Definition of logical gates, used in the examples below.
and_gate(all X:(and(X) , ~ab(X) => (in1(X), in2(X) <=> out(X)))).
or_gate( all X:(or(X)  , ~ab(X) => (in1(X) ; in2(X) <=> out(X)))).
xor_gate(all X:(xor(X) , ~ab(X) => (out(X) <=> in1(X),~in2(X);~in1(X),in2(X)))).

% Two unconnected AND gates with two inputs. It is observed that the
% inputs are true and the outputs are false.
problem1(SD, COMP, OBS) :-
  and_gate(AND),
  SD = [ AND, and(a1), and(a2) ],
  COMP = [a1, a2],
  OBS = [in1(a1), in2(a1), ~out(a1), in1(a2), in2(a2), ~out(a2)].

% Example of wwo AND gates where the output of the first gate (a1) is
% connected to the first input (in1) of the second gate (a2). It is
% easy to see that the observations are inconsistent with the
% specification.
problem2(SD, COMP, OBS) :-
  and_gate(AND),
  SD = [ AND, and(a1), and(a2), out(a1) <=> in1(a2) ],
  COMP = [a1, a2],
  OBS = [in1(a1), ~in2(a1), out(a2)].

% Another wiring example, now with two AND gates and an OR gate.
problem3(SD, COMP, OBS) :-
  and_gate(AND), or_gate(OR),
  SD = [ AND, OR, and(a1), and(a2), or(o1),
         out(a1) <=> in1(o1), out(a2) <=> in2(o1)],
  COMP = [a1, a2, o1],
  OBS = [in1(a1), in2(a1), in1(a2), in2(a2), ~out(o1)].

% The following represents a (one-bit) full adder: a
% circuit that can be used for the addition of two bits with
% carry-in and carry-out bits.
%
% in1(fa), in2(fa): input bits
% carryin(fa):      carry-in bit
% out(fa):          output bit
% carryout(fa):     carry-out bit
%
% returns the sum of in1(fa) + in2(fa) + carryin(fa)
% as 2 * carryout(fa) + out(fa) (i.e., as 2 bits)
fulladder(SD, COMP, OBS) :-
  and_gate(AND), or_gate(OR), xor_gate(XOR),
  SD = [AND, OR, XOR,
	and(a1), and(a2), xor(x1), xor(x2), or(r1),
        in1(fa) <=> in1(x1), in1(fa) <=> in1(a1),
        carryin(fa) <=> in1(a2), carryin(fa) <=> in2(x2),
	out(fa) <=> out(x2), carryout(fa) <=> out(r1),
	in2(fa) <=> in2(x1), in2(fa) <=> in2(a1),
        out(x1) <=> in2(a2), out(x1) <=> in1(x2),
        out(a2) <=> in1(r1), out(a1) <=> in2(r1) ],
  COMP = [a1, a2, x1, x2, r1],
  OBS = [in1(fa), ~in2(fa), carryin(fa), out(fa), ~carryout(fa)]. %1+1=1?

% problem from the nonmonotonic lecture slides
problem5(SD, COMP, OBS) :-
    xor_gate(XOR), and_gate(AND), or_gate(OR),
    SD = [ XOR, AND, OR,  xor(x1), and(a1), and(a2), xor(x2), or(o1),
        out(x1) <=> in1(x2), out(x1) <=> in2(a2), out(a2) <=> in1(o1), out(a1) <=> in2(o1) ],
    COMP = [ x1, x2, a1, a2, o1 ],
    OBS = [ in1(x1), ~in2(x1), in1(a1), ~in2(a1), in1(a2), in2(x2), out(x2), ~out(o1) ].

% equal to problem5 appened to itself.
problem6(SD, COMP, OBS) :-
    xor_gate(XOR), and_gate(AND), or_gate(OR),
    SD = [ XOR, AND, OR, xor(x1), and(a1), and(a2), xor(x2), or(o1),
            xor(x3), xor(x4), and(x3), and(x4), or(o2),
            out(x1) <=> in1(x2), out(x1) <=> in2(a2), out(a2) <=> in1(o1), out(a1) <=> in2(o1),
            out(x2) <=> in1(x3), out(o1) <=> in2(x3), out(x2) <=> in1(a3), out(o1) <=> in2(a3),
            out(x3) <=> in1(a4), in1(x4) <=> out(x3), in2(x4) <=> out(a4), in1(o2) <=> out(a4), in2(o2) <=> out(a3)],
    COMP = [ x1, x2, a1, a2, o1, x3, x4, a3, a4, o2 ],
    OBS = [ in1(x1), ~in2(x1), in1(a1), ~in2(a1), in1(a2), in2(x2), out(x2), ~out(o1),
            in1(a3), out(x2), ~out(o2) ].
