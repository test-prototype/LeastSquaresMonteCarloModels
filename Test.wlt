BeginTestSection["Test"]

VerificationTest[(* 1 *)
	feasibleRange[31, Map[Function[Function[-1]], Range[31]], Map[Function[Function[1]], Range[31]], Interval[List[0, 0]], Interval[List[5, 5]], Join[Map[Function[Interval[List[0, 10]]], Range[3]], Map[Function[Interval[List[2, 2]]], Range[2]], Map[Function[Interval[List[0, 10]]], Range[26]]]]
	,
	List[Interval[List[0, 0]], Interval[List[0, 1]], Interval[List[0, 2]], Interval[List[1, 3]], Interval[List[2, 2]], Interval[List[2, 2]], Interval[List[1, 3]], Interval[List[0, 4]], Interval[List[0, 5]], Interval[List[0, 6]], Interval[List[0, 7]], Interval[List[0, 8]], Interval[List[0, 9]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[0, 10]], Interval[List[1, 9]], Interval[List[2, 8]], Interval[List[3, 7]], Interval[List[4, 6]], Interval[List[5, 5]]]	
	,
	TestID->"418b09b5-e0b3-4abe-9be8-495cdcb4cdee"
]

VerificationTest[(* 2 *)
	makeInventoryMeshOneStep["Decisions"][List[0, 1.5`, 8.9`], Function[-5], Function[2], 0, 10, List[1, 2], 0.01`]
	,
	List[0, 1, 1.5`, 2, 2.5`, 3.5`, 3.9000000000000004`, 8.9`, 9.9`, 10]	
	,
	TestID->"89220b88-e6ce-4ec2-ae6a-e5369aaa7140"
]

VerificationTest[(* 3 *)
	makeAllowedActions[List[0, 1.5`], -5, 2, 0, 10, 3, 0.1`]
	,
	List[List[0, 1, 2], List[0.`, 1.75`, 3.5`]]	
	,
	TestID->"dca4abfc-6919-43c3-9732-ab94ffb3204e"
]

VerificationTest[(* 4 *)
	Apply[And, Map[Function[MemberQ[discretiseIntervals[Interval[List[0, 2.`], List[3.1`, 3.3`]], 0.3`, Times[50, Power[10, -2]]], Slot[1]]], Flatten[Apply[List, Interval[List[0, 2.`], List[3.1`, 3.3`]]]]]]
	,
	True	
	,
	TestID->"778be095-c7c2-44a8-9f32-aa0d764ac631"
]

VerificationTest[(* 5 *)
	Apply[And, Map[Function[MemberQ[discretiseIntervals[Interval[List[0, 0], List[1, 3], List[5, 5]], 0.33`, Power[10, -2]], Slot[1]]], Flatten[Apply[List, Interval[List[0, 0], List[1, 3], List[5, 5]]]]]]
	,
	True	
	,
	TestID->"7bc3410e-7896-41b6-bb79-68f1dbaa09f3"
]

VerificationTest[(* 6 *)
	Apply[And, Map[Function[MemberQ[makeInventoryMeshOneStep["Decisions"][List[2, Times[7, Power[3, -1]], Times[8, Power[3, -1]], 3, Times[10, Power[3, -1]], Times[11, Power[3, -1]], 4, Times[13, Power[3, -1]], Times[14, Power[3, -1]], 5, Times[16, Power[3, -1]], Times[17, Power[3, -1]], 6, Times[19, Power[3, -1]], Times[20, Power[3, -1]], 7, Times[22, Power[3, -1]], Times[23, Power[3, -1]], 8], Function[-1], Function[1], 0, 10, List[3, 3], Power[10, -2]], Slot[1]]], makeInventoryMeshOneStep["Decisions"][List[2, Times[7, Power[3, -1]], Times[8, Power[3, -1]], 3, Times[10, Power[3, -1]], Times[11, Power[3, -1]], 4, Times[13, Power[3, -1]], Times[14, Power[3, -1]], 5, Times[16, Power[3, -1]], Times[17, Power[3, -1]], 6, Times[19, Power[3, -1]], Times[20, Power[3, -1]], 7, Times[22, Power[3, -1]], Times[23, Power[3, -1]], 8], Function[-1], Function[1], 0, 10, List[3, 3], Power[10, -2]]]]
	,
	True	
	,
	TestID->"1517717b-ffb7-4417-b335-0044fc092b9f"
]

EndTestSection[]
