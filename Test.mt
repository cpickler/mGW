(* Mathematica Test File *)

Test[
	mGWItemString["Adorned Amber Jewel"][[1]]
	,
	"24477"
	,
	TestID -> "Test-20160410-U6U6H7"
]

Test[
	Length[Flatten["id" /. Values[Association[mGWAchievements["Daily"]]]]]
	,
	19
	,
	TestID -> "Test-20160410-I7B2R6"
]