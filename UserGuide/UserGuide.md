# Least Squares Monte Carlo User Guide

`nT` is the number of exercise dates left, _i.e._ not including the valuation, or current, date.

## Grid construction (feasibility region + mesh)

The basic element is `Interval[]`; it is useful because it supports single points, proper intervals and any combination of them.
 For instance one can have a final level to be either `0` or the interval `[100, 150]` : this would be expressed as
 `Interval[{0, 0}, {100, 150}]`. The `Interval[]` object also handles nicely situations with gaps in the feasibility region.

### Feasibility region

The feasibility region is determined from the constraints of the deal in three steps:

 * First, the region of allowed inventories starting from the initial inventory and moving forward in time is built.
 * Next, the region of allowed inventories starting from the final inventory and moving backward in time is built.
 * The feasibility region is finally given the by intersection, at each time, of the two regions above.

### Mesh

The mesh is the collection of the set of points at each time slice.

* There are 2 ways to build a grid :

    1. Specify a `NominalDelta`. This is the grid made of equally spaced points at all time slices, therefore it can have different number of
    nodes at different time slices. It takes a tolerance parameter and it will merge grid points closer than the tolerance.
    In practise the first step is building the feasibility range, which will yield an `Interval[]` object at each time slice;
    next each `Interval[]` is discretised by dividing each connected sub-interval in chunks of size equal to the nominal delta.
    In this process one should preserve the end-points of each connected sub-interval.

    2. Specify a number of decisions (actually two, one going up and one going down : `nMax`, `nMin`).
    For each grid point `q` at the current time slice consider the interval `[q + minRate, q]` (if `minRate < 0` or
    `[q, q + minRate]` if `minRate > 0`) and divide it in `nMin` chunks and
      similarly divide the interval `[q, q + maxRate]` in `nMax` chunks.
      All the newly generated points are then merged according to a tolerance parameter and this final set constitutes the mesh
      at the next time slice.



### Temp

* keep "Decisions" and "makeAllowedActions" ?
* `nT` exercise dates (decisions). The initial inventory  refers to the valuation date (when a decision can be made) whereas the
final inventory refers to the end of the last exercise date (when no more decisions can be taken).

### Junk

  * Check definition of cashflows: is it consistent with LSMC ?
  
