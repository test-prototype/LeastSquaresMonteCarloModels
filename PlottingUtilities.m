(* Wolfram Language Package *)

BeginPackage["PlottingUtilities`"]
(* Exported symbols added here with SymbolName::usage *)  

(*plotFeasibilityRange::usage = ""
plotConvergence::usage = ""
plotConvergenceControl::usage = ""
plotMeshData::usage = ""
plotMeshHistogram::usage = ""*)


Begin["`Private`"] (* Begin Private Context *) 

(*plotFeasibilityRange[label_, description_, definition_, size_, opts___] :=
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
    ]*)
    
    




End[] (* End Private Context *)

EndPackage[]