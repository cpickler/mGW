(* Mathematica Package *)

(* Created by the Wolfram Workbench Apr 4, 2016 *)
(* Created by Arassilma.8096 -> @Chris_P_y *)


BeginPackage["mGuildWars`"]
(* Exported symbols added here with SymbolName::usage *) 

mGWItemString::usage="idString[name] returns the Association <|Subscript[name, 1]-> Subscript[id, 1]|>
mGWItemString[name,element] returns a list of the requested element."

mGWItem::usage="item[id] returns information for item with the given id."

mGWCharacter::usage="To be added"
mGWCharacter::form="Invalid argument for character, string or integer expected."

mGWGuild::usage="mGWGuild[id] returns v1 API information about the guild with the given id."

testToken::usage="API token for testing."

mGWInventory::usage="mGWInventory[token, character] returns an Association of the inventory including id, cound, binding and bound_to."

mGWInvCount::usage="mGWInvCount[token,character] returns the association <|id -> counts|>."

Begin["`Private`"]
(* Implementation of the package *)

testToken = "C53020FE-F672-514F-B5C9-D7C209927B91CC796525-2980-4FD6-8B85-\
3C9D96BFCD4E"

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
        2] // DeleteCases[Null]]

mGWInvCount[api_, character_] := Module[{data},
  data = mGWInventory[api, character];
  Merge[#["id"] -> #["count"] & /@ data, Total]]

(* ::Section:: *)
(* Footer *)
  
End[]

EndPackage[]

