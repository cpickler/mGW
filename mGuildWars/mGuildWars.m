(* Mathematica Package *)

(* Created by the Wolfram Workbench Apr 4, 2016 *)
(* Created by Arassilma.8096 -> @Chris_P_y *)


BeginPackage["mGuildWars`"]
(* Exported symbols added here with SymbolName::usage *) 
mGWIds::usage="List of all valid item Ids; when needed it called and then stored."

mGWItemString::usage="idString[name] returns the Association <|Subscript[name, 1]-> Subscript[id, 1]|>
mGWItemString[name,element] returns a list of the requested element."

mGWItem::usage="item[id] returns information for item with the given id."

mGWIdQ::usage="mGWIdQ[id] tests to see if the given id is valid."

mGWCharacter::usage="To be added"
mGWCharacter::form="Invalid argument for character, string or integer expected."

mGWGuild::usage="mGWGuild[id] returns v1 API information about the guild with the given id."

mGWInventory::usage="mGWInventory[token, character] returns an Association of the inventory including id, cound, binding and bound_to."

mGWInvCount::usage="mGWInvCount[token,character] returns the association <|id -> counts|>."

mGWMats::usage="mGWMats[api] returns the Dataset for material strage with catagory, id, and count."

mGWTp::usage="mGWTp[item_ids] returns the raw JSON from the API.
mGWTp[item_ids, element] returns the association with ids as keysmapped to the value for the element. Options include \"SellPrice\", \"SellQuantity\", \"BuyPrice\", and \"BuyQuantity\"."
mGWTp::ider="At least 1 Item Id given does not exist."

mGWAchievements::usage = "mGWAchievements[] returns a list of all achievement ids. \
	mGWAchievments[\"Daily\"] returns raw data for the daily achievements."

Begin["`Private`"]
(* Implementation of the package *)

(* ::Section:: *)
(* Items *)

mGWIds[] := mGWIds[] = URLExecute["https://api.guildwars2.com/v2/items/"]

mGWItemString[term_] :=
    Module[ {url, result},
        url = URLBuild[{"https://www.gw2shinies.com/api/json/idbyname/", 
           term}];
        result = 
         Import[url, 
          "JSON"];
        KeySort[AssociationThread["name" /. result, "item_id" /. result]]
    ]

mGWItemString[term_, element_] :=
    Module[ {url, result},
        url = URLBuild[{"https://www.gw2shinies.com/api/json/idbyname/", 
           term}];
        result = URLExecute[url];
        element /. result
    ]

mGWItem[id_] :=
    Module[ {url},
        url = "https://api.guildwars2.com/v2/items/" <> id;
        URLExecute[url]
    ]

mGWIdQ[id_] :=
    Which[
    ListQ[id], Map[MemberQ[mGWIds[], #] &, id],
    IntegerQ[id], MemberQ[mGWIds[], id]
    ]
    
(* ::Section:: *)
(* Character API *)

mGWCharacter[api_] :=
    URLExecute[
     "https://api.guildwars2.com/v2/characters", {"access_token" -> api}]

mGWCharacter[api_,character_] :=
    Module[ {charcode,list,name},
        list = mGWCharacter[api];
        If[ StringQ[character],
            name = Nearest[list,character,1][[1]],
            If[ IntegerQ[character],
                name = list[[character]],
                Message[mGWCharacter::form];
                $Failed
            ]
        ];
        charcode = StringReplace[URLEncode[name], "+" -> "%20"];
        URLExecute[
         "https://api.guildwars2.com/v2/characters/" <> 
          charcode, {"access_token" -> api}]
    ]
     
mGWCharacter[api_, character_, element_] :=
    Module[ {data},(*gets character data*)
        data = mGWCharacter[api, character];
        Which[
         StringMatchQ[element, "Created"], DateObject["created" /. data],
         StringMatchQ[element, "Age"], Quantity["age" /. data, "Seconds"],
         StringMatchQ[element, "Guild"], mGWGuild["guild" /. data],
         (*StringMatchQ[element, "Inventory"],mGWInventory[api,character],*)
         True, data]
    ]

(* ::Section:: *)
(* Guilds *)

(* ::Subsection:: *)
(* v1 API *)

mGWGuild[id_] :=
    Module[ {},
        URLExecute[
         "https://api.guildwars2.com/v1/guild_details.json", {"guild_id" -> 
           id}]
    ]

(* ::Section:: *)
(* Inventory *)
mGWInventory[api_, character_] :=
    Map[Association, 
      Flatten["inventory" /. {"bags" /. mGWCharacter[api, character]}, 
        2] // DeleteCases[#, Null]&]

mGWInvCount[api_, character_] := Module[{data},
  data = mGWInventory[api, character];
  Merge[#["id"] -> #["count"] & /@ data, Total]]

(* ::Section:: *)
(*Account*)


(* ::Subsection:: *)
(*Material Storage*)
mGWMats[api_] := 
 Dataset[Association /@ 
   URLExecute[
    "https://api.guildwars2.com/v2/account/materials", \
{"access_token" -> token}]]

(* ::Section:: *)
(* Trading Post *)

mGWTp[items_] :=
    Module[ {idString},
        idString = StringRiffle[items, ","];
        URLExecute[
         "https://api.guildwars2.com/v2/commerce/prices", {"ids" -> 
           idString}]
    ]

mGWTp[items_, element_] :=
    Module[ {data, result},
        data = mGWTp[items];
        result =
         Which[
          data == {"text" -> "all ids provided are invalid"}, (Message[
            mGWTp::ider];
                                                               Return[$Failed]),
          element == "SellPrice", 
          AssociationThread["id" /. data, "unit_price" /. ("sells" /. data)],
          element == "SellQuantity", 
          AssociationThread["id" /. data, "quantity" /. ("sells" /. data)],
          element == "BuyPrice", 
          AssociationThread["id" /. data, "unit_price" /. ("buys" /. data)],
          element == "BuyQuantity", 
          AssociationThread["id" /. data, "quantity" /. ("buys" /. data)],
          True, $Failed
          ];
        If[ ! AssociationQ[result],
            (Message[mGWTp::ider];
             $Failed)
        ]
    ]


(* ::Section:: *)
(* Achievements *)

mGWAchievements[] := URLExecute["https://api.guildwars2.com/v2/achievements"]
mGWAchievments["Daily"] := URLExecute["https://api.guildwars2.com/v2/achievements/daily"]

(* ::Section:: *)
(* Footer *)
  
End[]

EndPackage[]

