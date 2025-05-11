(*!* start:DME *!*)
(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
If[$Notebooks,

	(* ::Section:: *)(* Initialization *)
	Quiet @ PacletInstall[CloudObject["user:tonya/PacletSites/DME/DME"]];
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

(* :!CodeAnalysis::EndBlock:: *)
(*!* end:DME *!*)
