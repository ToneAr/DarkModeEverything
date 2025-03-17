(*!* start:DME *!*)
(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
If[$Notebooks,

	(* ::Section:: *)(* Initialization *)
	PacletInstall[CloudObject["user:tonya/PacletSites/DME/DME"]];
	<<TonyAristeidou`DME`;

	(* ::Section:: *)(* Sugar *)
	MakeExpression[RowBox[{func_, SubscriptBox["/@", level_], list_}], StandardForm] :=
	MakeExpression[
			RowBox[
				{"Map", "[", RowBox[{func, ",", list, ",", RowBox[{"{", level, "}"}]}], "]"}
			],
			StandardForm
		];

	(* ::Section:: *)(* Set default DME options *)
	TonyAristeidou`DME`ResetOptions[];

];

(* ::Section:: *)(* Kernel tweaks *)
$HistoryLength = 100;

(* ::Section:: *)(* Helper functions *)
Options[buildAndDeployPaclet]={
	Permissions -> "Public"
};
buildAndDeployPaclet[dir_, cloudLoc_, OptionsPattern[]] := Module[{
		res
	},
	Needs["PacletTools`"];
	res = PacletTools`PacletBuild[ dir ];
	Quiet[
		DeleteObject[CloudObject[cloudLoc]],
		CloudObject::cloudnf
	];
	CopyFile[
		res["PacletArchive"],
		CloudObject[cloudLoc],
		OverwriteTarget->True
	];
	SetPermissions[CloudObject[cloudLoc], OptionValue[Permissions]];
	CloudObject[cloudLoc]
];
(* :!CodeAnalysis::EndBlock:: *)
(*!* end:DME *!*)
