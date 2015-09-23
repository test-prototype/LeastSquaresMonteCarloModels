(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 20-May-2015 *)

BeginPackage["Storage`", {"PlottingUtilities`"}]
(* Exported symbols added here with SymbolName::usage *) 

fromHDFDatasetToList::usage = "fromHDFDatasetToList[datasets] builds a Mathematica square list out of
                               the corresponding blocks from an HDF5 dataset."
fromPythonHDFDebugFile::usage = ""
fromPythonHDFLSMCDebugFile2Dataset::usage = ""
fromPythonHDFBacktestFile::usage = ""
fromMHDFDebugFile::usage = ""

m2Fraction::usage = ""

feasibleRangeOneStepBackward::usage = ""
feasibleRangeOneStepForward::usage = ""
feasibleRange::usage = "feasibleRange[nT, minRate, maxRate, startRange, endRange, invRange] where :
		nT : number of exercise dates;
		minRate : list of nT minimum rate functions of inventory level;
		maxRate : list of nT minimum rate functions of inventory level;
		startRange : Interval[] like initial inventory level at the start of the first exercise date;
		endRange : Interval[] like final inventory level at the end of the last exercise date;
		invRange : list of Interval[] like inventory constraints at the end of each exercies date;
		             invRange[[1]] can be different from startRange
		             invRange[[nT]] == endRange
"

plotFeasibilityRange::usage = ""

cashflow::usage = ""
ccashflow::usage = ""

discretiseIntervals::usage = ""

makeInventoryMeshOneStep::usage = ""
makeInventoryMesh::usage = ""
makeAllowedActions::usage = "makeAllowedActions[levels, minRate, maxRate, minFea, maxFea, nActions, tolerance]"
same::usage = "same[x, y, relTol, absTol] returns True if Abs[x - y] < relTol Abs[y] + absTol"

trOp::usage = ""
appendToDataset::usage = ""

plotConvergence::usage = ""
plotConvergenceControl::usage = ""
plotMeshData::usage = ""
plotMeshHistogram::usage = ""

basisFunctions::usage = ""
makeMatrix::usage = ""

Begin["`Private`"]
(* Implementation of the package *)

$datasetsHDF5DebugFile =
    {"/actions/axis0", "/actions/axis1", "/actions/block0_items", "/actions/block0_values",
      "/cashflows_opex/axis0", "/cashflows_opex/axis1", "/cashflows_opex/block0_items", "cashflows_opex/block0_values",
      "/cashflows_spot/axis0", "/cashflows_spot/axis1", "/cashflows_spot/block0_items", "cashflows_spot/block0_values",
      "/levels/axis0", "/levels/axis1", "/levels/block0_items", "/levels/block0_values",
      "/sims_bwd/axis0", "/sims_bwd/axis1", "/sims_bwd/block0_items", "/sims_bwd/block0_values",
      "/sims_fwd/axis0", "/sims_fwd/axis1", "/sims_fwd/block0_items", "/sims_fwd/block0_values"
    };


$groupsHDF5DebugFile =
    {"/actions", "/cashflows_opex", "/cashflows_spot", "/levels", "/sims_bwd", "/sims_fwd"};


$datasetsProcessedHDF5DebugFile =
    {{"/actions_axis0", "/actions_axis1", "/actions_block0_values"},
      {"/cashflows_opex_axis0", "/cashflows_opex_axis1" , "cashflows_opex_block0_values"},
      {"/cashflows_spot_axis0", "/cashflows_spot_axis1", "cashflows_spot_block0_values"},
      {"/levels_axis0", "/levels_axis1", "/levels_block0_values"},
      {"/sims_bwd_axis0", "/sims_bwd_axis1", "/sims_bwd_block0_values"},
      {"/sims_fwd_axis0", "/sims_fwd_axis1", "/sims_fwd_block0_values"}
    };


fromPythonHDFDebugFile[file_, group_, debugQ_, h5DatesQ_] :=
    If[debugQ,
      Module[{zero, datasets, local},
        zero = DateDifference[{1942, 7, 2, 0, 0, 0.`}, {2012, 7, 1, 0, 0, 0}, "Second"][[1]] ;
        datasets = StringJoin[group, #] & /@ {"/axis0", "/axis1", "/block0_values"} ;
        local = {Import[file, {"Datasets", datasets[[1]]}], Import[file, {"Datasets", datasets[[2]]}], Import[file, {"Datasets", datasets[[3]]}]};

        fromHDFDatasetToList[{local[[1]], If[h5DatesQ, local[[2]], DateList[# / 10^9 + zero] & /@ local[[2]]], local[[3]]}]

      ],
      Module[{zero, datasets, local},
        zero = DateDifference[{1942, 7, 2, 0, 0, 0.`}, {2012, 7, 1, 0, 0, 0}, "Second"][[1]] ;
        datasets = StringJoin[group, #] & /@ {"/index", "/values"};
        local = {Import[file, {"Datasets", datasets[[1]]}], Import[file, {"Datasets", datasets[[2]]}]};

        Join[{{"", ""}}, Transpose[{If[h5DatesQ, local[[1]], (DateList[# / 10^9 + zero] & /@ local[[1]])], local[[2]]}]]
      ]
    ]


(*fromPythonHDFLSMCDebugFile2Dataset[file_, group_, type_] :=
	Which[
		group == "/cashflows_intrinsic",
		Association["value_int"->Import[file, {"Datasets", {group<>type}}][[1]]],
		group == "/cashflows",
		With[{data=Import[file, {"Datasets", {group<>type}}]},
			Association["value_avg" -> Mean[data],
				 		"value_stderr" -> StandardDeviation[data]/Sqrt[Length[data]]]
		],
		group == "/actions_intrinsic",
		With[{data=Flatten@Import[file, {"Datasets", {group<>type}}]},
			Dataset[Association[Thread[Rule[Range[Length[data]], Association["action_int" -> #] & /@ data]]]]
		],
		group == "/actions",
		Module[{data, action, minmax},
			data = Import[file, {"Datasets", {group<>type}}];
			action = Association[Thread[Rule[{"action_avg","action_stderr"}, #]]] & /@ (Through[{Mean, StandardDeviation[#]/Sqrt[Length[#]] &}[#]] & /@ data);
			minmax = Association["inventory_min" -> Min[#], "inventory_max" -> Max[#]] & /@ Transpose[(Accumulate[#] & /@ Transpose[data])] ;
			Dataset[Association[Thread[Rule[Range[Length[data]], (Association[Flatten[#]] & /@ Transpose[{Thread[Rule[{"action_avg","action_stderr"}, #]] & /@ (Through[{Mean, StandardDeviation[#]/Sqrt[Length[#]] &}[#]] & /@ data),
			 {"inventory_min" -> Min[#], "inventory_max" -> Max[#]} & /@ Transpose[(Accumulate[#] & /@ Transpose[data])]
			}])]]]]

			]

	]*)

fromPythonHDFLSMCDebugFile2Dataset[args_, file_] :=
    Association@Join[Thread[Rule[{"nsims", "solver", "max_basis_order", "basis_function", "num_actions"}, args]],
      {"value_int" -> Import[file, {"Datasets", {"/cashflows_intrinsic" <> "/values"}}][[1]]},
      With[{data = Import[file, {"Datasets", {"/cashflows" <> "/values"}}]},
        {"value_avg" -> Mean[data], "value_stderr" -> StandardDeviation[data] / Sqrt[Length[data]]}
      ],
      {With[{data = Flatten@Import[file, {"Datasets", {"/actions_intrinsic" <> "/block0_values"}}]},
        "action_int" -> Dataset[Association[Thread[Rule[Range[Length[data]], Association["action_int" -> #] & /@ data]]]]
      ]},
      {Module[{data, action, minmax},
        data = Import[file, {"Datasets", {"/actions" <> "/block0_values"}}];
        action = Association[Thread[Rule[{"action_avg", "action_stderr"}, #]]] & /@ (Through[{Mean, StandardDeviation[#] / Sqrt[Length[#]] &}[#]] & /@ data);
        minmax = Association["inventory_min" -> Min[#], "inventory_max" -> Max[#]] & /@ Transpose[(Accumulate[#] & /@ Transpose[data])] ;
        "action" -> Dataset[Association[Thread[Rule[Range[Length[data]], (Association[Flatten[#]] & /@ Transpose[{Thread[Rule[{"action_avg", "action_stderr"}, #]] & /@ (Through[{Mean, StandardDeviation[#] / Sqrt[Length[#]] &}[#]] & /@ data),
          {"inventory_min" -> Min[#], "inventory_max" -> Max[#]} & /@ Transpose[(Accumulate[#] & /@ Transpose[data])]
        }])]]]]

      ]}

    ]



fromHDFDatasetToList[datasets_] := ArrayFlatten[{{{{""}}, {datasets[[1]]}}, {{#} & /@ datasets[[2]], datasets[[3]]}}]


fromPythonHDFBacktestFile[file_, group_] :=
    Which[  group == "/Backtest Results",
      Module[{zero, datasets, local},

        zero = DateDifference[{1942, 7, 2, 0, 0, 0}, {2012, 7, 1, 0, 0, 0}, "Second"][[1]] ;

        datasets = StringJoin[group, #] & /@ {"/axis0", "/axis1", "/block0_values"} ;

        local = {Import[file, {"Datasets", datasets[[1]]}], Import[file, {"Datasets", datasets[[2]]}], Import[file, {"Datasets", datasets[[3]]}]};

        fromHDFDatasetToList[{local[[1]], (DateList[# / 10^9 + zero] & /@ local[[2]]), local[[3]]}]
      ],
      group == "/Attribution Data" || group == "/Hedging Data",
      Module[{zero, datasets, local},

        zero = DateDifference[{1942, 7, 2, 0, 0, 0}, {2012, 7, 1, 0, 0, 0}, "Second"][[1]] ;

        datasets = StringJoin[group, #] & /@ {"/axis1", "/axis2", "/block0_values"} ;

        local = {Import[file, {"Datasets", datasets[[1]]}], Import[file, {"Datasets", datasets[[2]]}], Import[file, {"Datasets", datasets[[3]]}]} ;

        fromHDFDatasetToList[{(DateList[# / 10^9 + zero] & /@ local[[1]]), (DateList[# / 10^9 + zero] & /@ local[[2]]), #}] & /@ Transpose[local[[3]], {2, 3, 1}]
      ]
    ]


fromMHDFDebugFile[file_, axis0_, axis1_, values_] :=
    Module[{zero, datasets, local},
      zero = DateDifference[{1942, 7, 2, 0, 0, 0.`}, {2012, 7, 1, 0, 0, 0}, "Second"][[1]] ;
      datasets = {axis0, axis1, values};
      local = {Import[file, {"Datasets", datasets[[1]]}], Import[file, {"Datasets", datasets[[2]]}], Import[file, {"Datasets", datasets[[3]]}]};
      ArrayFlatten[{{{{""}}, {local[[1]]}}, {{#} & /@ (DateList[# / 10^9 + zero] & /@ local[[2]]), local[[3]]}}]
    ]


m2Fraction[list_] := StringJoin["Fraction(", ToString[Numerator[#]], ",", ToString[Denominator[#]], ")"] & /@ list


feasibleRangeOneStepBackward[boundRegion_, minRate_, maxRate_, currentRange_] :=
    IntervalUnion[Sequence @@
        (Function[int, Interval[{MinValue[{x, Min[boundRegion] <= x <= Max[boundRegion], int[[1]] <= x + maxRate[x]}, x],
          MaxValue[{x, Min[boundRegion] <= x <= Max[boundRegion], x + minRate[x] <= int[[2]] }, x]}]][#]
            & /@ (List @@ currentRange))]


feasibleRangeOneStepForward[boundRegion_, minRate_, maxRate_, currentRange_] :=
    IntervalUnion[Sequence @@
        (Function[int, IntervalIntersection[boundRegion,
          Interval[{MinValue[{x + minRate[x], int[[1]] <= x <= int[[2]]}, x],
            MaxValue[{x + maxRate[x], int[[1]] <= x <= int[[2]]}, x]}]]][#]
            & /@ (List @@ currentRange))]


feasibleRange[nT_, minRate_, maxRate_, startRange_, endRange_, invRange_] :=
    Module[{bwd, fwd},
      bwd = NestList[{#[[1]] - 1, feasibleRangeOneStepBackward[invRange[[#[[1]] - 1]], minRate[[#[[1]]]], maxRate[[#[[1]]]], #[[2]]]} &,
        {Length[invRange], endRange}, nT - 1][[All, 2]] // Reverse;
      fwd = NestList[{#[[1]] + 1, feasibleRangeOneStepForward[invRange[[#[[1]] + 1]], minRate[[#[[1]] + 1]], maxRate[[#[[1]] + 1]], #[[2]]]} &,
        {1, feasibleRangeOneStepForward[invRange[[1]], minRate[[1]], maxRate[[1]], startRange]}, nT - 1][[All, 2]];
      Join[{startRange}, MapThread[IntervalIntersection[#1, #2] &, {bwd, fwd}]]
    ]


plotFeasibilityRange[label_, description_, definition_, size_, opts___] :=
    {description,
      With[{fea = feasibleRange[Sequence @@ definition]},
        ListLinePlot[{Min[#] & /@ fea, Max[#] & /@ fea},
          PlotStyle -> {Black}, Filling -> {2 -> {1}},
          AxesLabel -> {"days", "feasible range"},
          PlotLabel -> "deal_" <> label,
          ImageSize -> size,
          opts]
      ]
    }


cashflow[flow_, spot_, fixedCost_, fuelCost_] := - flow (1 - fuelCost)^(-Sign[flow]) spot - Abs[flow] fixedCost


ccashflow = Compile[{{flow, _Real}, {spot, _Real}, {fixedCost, _Real}, {fuelCost, _Real}},
  - flow (1 - fuelCost)^(-Sign[flow]) spot - Abs[flow] fixedCost,
  RuntimeAttributes -> {Listable}, CompilationTarget -> "C"
]


discretiseIntervals[intervals_, nominalDelta_, sametest_] :=
    Module[{delta = Max[nominalDelta, $MachineEpsilon], interiorPoints, pointsToBeKept},
      interiorPoints =
          Union[Flatten[
            Function[interval,
              If[delta > (Max[interval] - Min[interval]),
                interval,
                Range[Min[interval], Max[interval], (Max[interval] - Min[interval]) / Max[1, Ceiling[(Max[interval] - Min[interval]) / delta]]]
              ]][#] & /@ (List @@ intervals)
          ], SameTest -> sametest];
      pointsToBeKept = Flatten[List @@ intervals];
      Union[Join[pointsToBeKept, interiorPoints], SameTest -> sametest]
    ]    


makeInventoryMesh["NominalDelta"][nT_, minRate_, maxRate_, startRange_, endRange_, invRange_, nominalDelta_, tolerance_] :=
    discretiseIntervals[#, nominalDelta, tolerance] & /@ feasibleRange[nT, minRate, maxRate, startRange, endRange, invRange]


same =
	Compile[{{x, _Real}, {y, _Real}, {relTol, _Real}, {absTol, _Real}},
		Abs[x - y] < relTol Abs[y] + absTol, CompilationTarget->"C"
	]


(* TODO does it work with gaps in the feasibility region ? *)
makeInventoryMeshOneStep["Decisions"][levels_, minRate_, maxRate_, minInv_, maxInv_, nDecisions_, tolerance_] :=
    Module[{interiorPoints},
      interiorPoints =
          Union[
            Flatten[{
              Function[lev, Join[
                lev + # / nDecisions[[1]] minRate[lev] & /@ Range[nDecisions[[1]]],
                lev + # / nDecisions[[2]] maxRate[lev] & /@ Range[nDecisions[[2]]]]][#] & /@ levels
            }], SameTest -> (Abs[#1 - #2] <= tolerance &)
          ];
      Union[Clip[Join[levels, interiorPoints], {minInv, maxInv}], SameTest -> (Abs[#1 - #2] < $MachineEpsilon &)]
    ]


(* TODO does it work with gaps in the feasibility region ? *)
makeInventoryMesh["Decisions"][nT_, minRate_, maxRate_, startRange_, endRange_, invRange_, nDecisions_, tolerance_] :=
    With[{feaRange = feasibleRange[nT, minRate, maxRate, startRange, endRange, invRange]},
      NestList[
        {#[[1]] + 1,
         makeInventoryMeshOneStep["Decisions"][#[[2]],
                                               minRate[[#[[1]]]],
                                               maxRate[[#[[1]]]],
                                               Min[feaRange[[#[[1]] + 1]]],
                                               Max[feaRange[[#[[1]] + 1]]],
                                               nDecisions,
                                               tolerance]} &,
        {1, discretiseIntervals[startRange, (Max[startRange] - Min[startRange]) / tolerance, tolerance]},
        nT][[All, 2]]
    ]


makeAllowedActions[levels_, minRate_, maxRate_, minFea_, maxFea_, nActions_, tolerance_] :=
    If[ nActions < 2,
      Print["nActions must be greater than 1"],
      Module[ {q = #, qmin, qmax, dq},
        If[ minRate[q] > maxRate[q],
          Print["minRate must be less than maxRate"],
          qmin = Clip[q + minRate[q], {minFea, maxFea}];
          qmax = Clip[q + maxRate[q], {minFea, maxFea}];
          dq = (qmax - qmin) / (nActions - 1);
          Union[Range[qmin, qmax, dq], SameTest -> (Abs[#1 - #2] <= tolerance &)]
        ]
      ] & /@ levels
    ]


trOp = Transpose[#, AllowedHeads -> All] &;


appendToDataset[dataset_, key_, values_] := dataset[trOp /* Append[key -> AssociationThread[Normal[dataset[Keys]], values]] /* trOp]


plotConvergence[values_, plotLabel_, isize_] :=
    Module[{levels, firstPlot, remainingPlots},
      levels = Union@Normal@values[All, "nLevels"] ;
      firstPlot = ListLinePlot[
        Function[basis, Values@Normal@values[Select[#nLevels == levels[[1]] && #basis == basis &], {"nSims", "avg"}]][#] & /@
            Normal@Union@values[All, "basis"],
        PlotLegends -> Normal@Union@values[All, "basis"],
        AxesLabel -> {"nSims", "value"},
        PlotLabel -> plotLabel,
        PlotRange -> {values[Min, "avg"], values[Max, "avg"]},
        Joined -> {True, False, False, False, False, False},
        PlotRangePadding -> {{5, 5}, {5, 5}}];
      remainingPlots =
          Function[level, ListLinePlot[
            Function[basis, Values@Normal@values[Select[#nLevels == level && #basis == basis &], {"nSims", "avg"}]][#] & /@
                Normal@Union@values[All, "basis"],
            Joined -> {True, False, False, False, False, False},
            PlotRangePadding -> {{5, 5}, {5, 5}}]][#]
              & /@ levels[[2 ;;]];

      Show[Join[{firstPlot}, remainingPlots], ImageSize -> isize]

    ]


plotConvergenceControl[values_, basis_, valuesControl_, plotLabel_, isize_] :=
    ListLinePlot[Join[Function[lev, Values@Normal@values[Select[#nLevels == lev && #basis == basis &], {"nSims", "avg"}]][#] & /@
        Normal@Union@values[All, "nLevels"],
      {Values@Normal@valuesControl[Select[#nLevels == None && #basis == basis &], {"nSims", "avg"}]}],
      PlotLegends -> Join[Normal@Union@values[All, "nLevels"], {"None"}],
      AxesLabel -> {"nSims", "value"},
      PlotRange -> {Min[{values[Min, "avg"], valuesControl[Min, "avg"]}], Max[{values[Max, "avg"], valuesControl[Max, "avg"]}]},
      PlotStyle -> Join[{Automatic} & /@ Normal@Union@values[All, "nLevels"], {{Thick, Dashed, Black}}],
      PlotLabel -> plotLabel,
      PlotRangePadding -> {{1, 1}, {1, 1}},
      ImageSize -> isize
    ]


(*plotMeshData[fea_, meshes_, isize_] :=
	ListLinePlot[Join[{Min[#] & /@ fea, Max[#] & /@ fea}, Function[mesh, Length[#] & /@ mesh][#] & /@ meshes],
		AxesLabel -> {"days", "max number of levels"},
		PlotStyle -> {Black, Black, Pink, Red, Purple, Blue},
		Filling -> {2 -> {1}},
		PlotLegends -> Join[{None, None},
							Function[mesh, ToString[Max[Length[#] & /@ mesh]] <> " \[Rule] " <> ToString[Total[Length[#] & /@ mesh]]][#] & /@
								meshes[[1;; -2]],
							{"Dynamic" <> " \[Rule] " <> ToString[Total[Length[#] & /@ meshes[[-1]]]]}
						 	],
		ImageSize -> isize
	]*)



plotMeshData[fea_, meshes_, isize_] :=
    Module[{pltFea, pltMesh},
      pltFea = ListLinePlot[{Min[#] & /@ fea, Max[#] & /@ fea},
        PlotStyle -> { Black}, Filling -> {2 -> {1}},
        Frame -> {{False, True}, {False, False}},
        FrameTicks -> {{None, All}, {None, None}},
        FrameLabel -> {{ None, "feasibility region"}, {None, None}},
        ImagePadding -> {{50, 50}, {40, 10}}, ImageSize -> isize];
      pltMesh = ListLinePlot[Function[mesh, Length[#] & /@ mesh][#] & /@ meshes,
        Frame -> {{True, False}, {True, True}},
        FrameLabel -> {{"max number of levels", None}, {"days", None}},
        PlotStyle -> {Pink, Red, Purple, Blue},
        FrameTicks -> {{All, None}, {All, None}},
        PlotLegends -> Join[
          Function[mesh, ToString[Max[Length[#] & /@ mesh]] <> " \[Rule] " <> ToString[Total[Length[#] & /@ mesh]]][#] & /@
              meshes[[1 ;; -2]],
          {"Dynamic" <> " \[Rule] " <> ToString[Total[Length[#] & /@ meshes[[-1]]]]}
        ],
        ImagePadding -> {{50, 50}, {40, 10}},
        ImageSize -> isize];
      Overlay[{pltMesh, pltFea}]
    ]


plotMeshHistogram[meshes_, isize_] :=
    Histogram[Function[mesh, Flatten[Differences[#] & /@ mesh]][#] & /@ meshes,
      Automatic,
      ChartLegends -> Join[Function[mesh, ToString[Max[Length[#] & /@ mesh]]][#] & /@ meshes[[1 ;; -2]], {"Automatic"}],
      AxesLabel -> {" level spacing ", " Counts "},
      ImageSize -> isize
    ]


basisFunctions["poly"] =
  Compile[{{order, _Integer}, {x, _Real}},
    x^# & /@ Range[0, order],
    RuntimeAttributes -> Listable
  ]


makeMatrix["poly"] =
    Compile[{{order, _Integer}, {sims, _Real, 1}},
      basisFunctions["poly"][order, sims],
      CompilationOptions -> {"InlineExternalDefinitions" -> True}
    ]



(*
basisFunctions =
    Compile[{{type, _Integer}, {order, _Integer}, {x, _Real}},
      Which[
        type == 1, "poly" 
        x^# & /@ Range[0, order],
        type == 2,   "herm" 
        HermiteH[#, x] & /@ Range[0, order],
        type == 3,  herme
        2^(-#/2) HermiteH[#, x / Sqrt[2]] & /@ Range[0, order],
        type == 4,  lag
        LaguerreL[#, x] & /@ Range[0, order],
        type == 5,  leg
        LegendreP[#, x] & /@ Range[0, order],
        type == 6,  cheb
        ChebyshevT[#, x] & /@ Range[0, order]
      ]
    ]
*)


(*
makeMatrix =
    Compile[{{type, _Integer}, {order, _Integer}, {sims, _Real, 1}},
      basisFunctions[type, order, sims],
      CompilationOptions -> {"InlineExternalDefinitions" -> True}
    ]
*)

End[]

EndPackage[]

