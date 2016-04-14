(* Mathematica Test File *)

Test[
    mGWItemString["Adorned Amber Jewel"][[1]]
    ,
    "24477"
    ,
    TestID -> "Test-20160410-U6U6H7"
]

Test[
    mGWAchievements["Daily"] // Keys // 
 ContainsAll[#, {"pve", "wvw", "pvp"}] &
    ,
    True
    ,
    TestID -> "Test-20160410-I7B2R6"
]
    
Test[
    mGWAchievements["Groups"]
    ,
    {"65B4B678-607E-4D97-B458-076C3E96A810", \
	"A4ED8379-5B6B-4ECC-B6E1-70C350C902D2", \
	"647D3B02-2302-41F0-88A3-6D88BB6F55CF", \
	"1CAFA333-0C2B-4782-BC4C-7DA30E9CE289", \
	"18DB115A-8637-4290-A636-821362A3C4A8", \
	"BE8B9954-5B55-4FCB-9022-B871AD00EAAB", \
	"A9F7378E-9C8A-48CC-9505-3094E661D5F6", \
	"56A82BB9-6B07-4AB0-89EE-E4A6D68F5C47", \
	"45410F60-AB66-4146-A0F7-CE99250C4CB0", \
	"4E6A6CE7-B131-40BB-81A3-235CDBACDAA9"}
    ,
    TestID -> "Test-20160411-B8C7O5"
]

Test[
    mGWAchievements[{"65B4B678-607E-4D97-B458-076C3E96A810", \
	"A4ED8379-5B6B-4ECC-B6E1-70C350C902D2"},"Groups"]
    ,
    {{"order" -> 4, "id" -> "65B4B678-607E-4D97-B458-076C3E96A810", 
      "description" -> 
       "Achievements for accomplishments throughout the jungle.", 
      "name" -> "Heart of Thorns", 
      "categories" -> {108, 109, 110, 111, 112, 116}}, {"order" -> 2, 
      "id" -> "A4ED8379-5B6B-4ECC-B6E1-70C350C902D2", 
      "description" -> "Achievements related to the story journal.", 
      "name" -> "Story Journal", 
      "categories" -> {121, 122, 123, 104, 71, 72, 68, 100, 81, 83, 82, 
        70}}}
    ,
    TestID -> "Test-20160411-I8Y1I3"
]

Test[
	mGWAchievements["Categories"]//ContainsAll[#, {1, 2, 3, 4, 5, 6, 7, 10}]&
	,
	True
	,
	TestID->"Test-20160411-L1P7L8"
]

Test[
	"id" /. mGWAccount[
 	"C53020FE-F672-514F-B5C9-D7C209927B91CC796525-2980-4FD6-8B85-3C9D96BFCD4E"]
	,
	"42AC08D1-E6A2-E511-80C3-ECB1D78A5C75"
	,
	TestID ->"Test-20160413-T4Z2D2"
]

Test[
	ContainsAll[
	 Keys[mGWAccount["C53020FE-F672-514F-B5C9-D7C209927B91CC796525-2980-4FD6-8B85-3C9D96BFCD4E", "Wallet"]], {1, 3, 5, 7}]
	 ,
	 True
	 ,
	 TestID->"Test-20160413-L9W9J1"
]

Test[
	AssociationQ[mGWAccount["C53020FE-F672-514F-B5C9-D7C209927B91CC796525-2980-4FD6-8B85-3C9D96BFCD4E","Wallet", "Invalid" -> "Invalid"]]
	,
	True
	,
	OptionValue::nodef
	,
	TestID->"Test-20160413-M3O1Q6"
]