(* Wolfram Language package *)

BeginPackage["TestData`"]

testDeals::usage=""
testSpot::usage=""

Begin["`Private`"]

testDeals =  
Dataset[List[
    <|"label" -> "0_4", "description" -> "31 days - start 0 - end 5 - full 10 - inj/wit 1 - storage \
inventory fixed for 2 days", 
    "definition" -> {31, (-1 &) & /@ Range[31], (1 &) & /@ Range[31], 
      Interval[{0, 0}], Interval[{5, 5}], 
      Join[Interval[{0, 10}] & /@ Range[3], 
       Interval[{2, 2}] & /@ Range[2], 
       Interval[{0, 10}] & /@ Range[26]]} |>, <|"label" -> "0_8", 
    "description" -> 
     "10 days - start 0 - end 7 - full 10 - wit 1 - inj variable - \
storage inventory fixed for 2 days", 
    "definition" -> {10, (-1 &) & /@ Range[10], 
      Join[(1 &) & /@ Range[6], (2 &) & /@ Range[4]], 
      Interval[{0, 0}], Interval[{7, 7}], 
      Join[Interval[{0, 10}] & /@ Range[3], 
       Interval[{2, 2}] & /@ Range[2], 
       Interval[{0, 10}] & /@ 
        Range[4], {Interval[{7, 7}]}]} |>, <|"label" -> "8", 
    "description" -> 
     "365 days - start 0 - end 0 - full 350 - inj/wit 5", 
    "definition" -> {365, (-5 &) & /@ Range[365], (5 &) & /@ 
       Range[365], Interval[{0, 0}], Interval[{0, 0}], 
      Join[
       Interval[{0, 350}] & /@ Range[364], {Interval[{0, 0}]}]}|>]];

SeedRandom[123987]
nsims=100;
ir = 0.023;
vol = 0.35;
s0 = 50.0
testSpot = NestList[# Exp[-ir 1.0/365] Exp[vol Sqrt[1.0/365] RandomVariate[NormalDistribution[0, 1], nsims]] &,
	                ConstantArray[s0, {nsims}], 364];

End[]

EndPackage[]