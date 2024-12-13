(* ::Package:: *)

BeginPackage["TonyAristeidou`DME`"];

SetAccentColor::usage = "Sets the main accent color of the UI";
SetBackgroundColor::usage = "Sets the main background color of the UI";
ResetOptions::usage = "Sets all default options required by the DarkModeEverything theme.";

Begin["`Private`"];

copyFile[from_?FileExistsQ, to_?FileExistsQ]/;$OperatingSystem === "Windows" :=
	Module[{
			proc, ret
		},
		WithCleanup[
			proc = StartProcess[{"Start-Process","PowerShell","-Verb","runAs"}]
			,
			WriteLine[proc,
				"Copy-Item -Force "<>StringRiffle[
					AbsoluteFileName[from],
					AbsoluteFileName[to]
				]
			];
			ret = ReadString[proc];
			,
			Close[proc]
		];
		AbsoluteFileName[to]
	];

Module[{
		
		infoLoc,backedUpFiles,fileList,backupDir
	},
	(*
		Check for non-empty .backup folder and backup all effected system files
	*)
	backupDir = FileNameJoin[{PacletObject["TonyAristeidou/DME"]["Location"], ".backup"}];
	infoLoc = FileNameJoin[{backupDir, "info.wl"}];
	If[!FileExistsQ[infoLoc],
		Enclose[
			(*
				Define all system files effected by DME
			*)
			fileList = FileNames["*.*",
					FileNameJoin[{PacletObject["TonyAristeidou/DME"]["Location"], "Resources"}],
					Infinity
				];
			(*
				Copy all effected files to .backup directory
			*)
			backedUpFiles = ({file}|->
				With[{
						from = StringReplace[file,
							___~~"Resources" -> $InstallationDirectory
						],
						to = FileNameJoin[PacletObject["TonyAristeidou/DME"]["Location"],file]
					},
					If[FileExistsQ[from],
						ConfirmQuiet[
							CopyFile[from, to, Method->"Copy-File"]
						];
						from
						,
						Nothing
					]
				]
			) /@ fileList;
			(*
				Export info.wl file upon successful backup
			*)
			Export[
				infoLoc,
				<|
					"Timestamp" -> Now,
					"BackedUpFiles" -> backedUpFiles
				|>,
				"WL"
			]
		] // ({enc} |-> (
			If[FailureQ[enc],
				Print["!! SystemFiles backup failed !!"];
				Print[enc];
			]
		))
	]
];

If[Not@*ValueQ @ LocalSymbol["DME:AccentColor"],
	LocalSymbol["DME:AccentColor"] = Hue[0.47, 0.6, 0.75]
];
If[Not@*ValueQ @ LocalSymbol["DME:BackgroundColor"],
	LocalSymbol["DME:BackgroundColor"] = Hue[0.08, 0.05, 0.07]
];

$AccentColor = LocalSymbol["DME:AccentColor"];
$BackgroundColor = LocalSymbol["DME:BackgroundColor"];

GetColor[h_, s_, b_] := Module[{
		clip = Clip[#,{0,1}]&,
		mod = Mod[#,1]&
	},
	Apply[
		Hue[ mod[#1+h], clip[#2+s], clip[#3+b], ##4 ]&,
		$AccentColor
	]
];
GetBackground[h_, s_, b_] := Module[{
		clip = Clip[#,{0,1}]&,
		mod = Mod[#,1]&
	},
	Apply[
		Hue[ mod[#1+h], clip[#2+s], clip[#3+b], ##4 ]&,
		$BackgroundColor
	]
];

SetAccentColor[color_?ColorQ]:= ( 
	With[{col=$AccentColor = Hue[color]},
		LocalSymbol["DME:AccentColor"] = col;
		$AccentColor = col
	]
);
SetAccentColor[Automatic|Default]:= ( 
	LocalSymbol["DME:AccentColor"] = Hue[0.47, 0.6, 0.75];
	$AccentColor = Hue[0.47, 0.6, 0.75]
);
SetBackgroundColor[color_?ColorQ]:= ( 
	With[{col = Hue[color]},
		LocalSymbol["DME:BackgroundColor"] = col;
		$BackgroundColor = col
	]
);
SetBackgroundColor[Automatic|Default]:= ( 
	LocalSymbol["DME:BackgroundColor"] = Hue[0.08, 0.05, 0.07];
	$BackgroundColor = Hue[0.08, 0.05, 0.07]
);

ResetOptions[] := 
	Module[{enclose =
		Enclose[
			Confirm @ SetOptions[ Dataset,
				HeaderStyle -> Hue[0., 0., 0.7],
				HeaderBackground -> Hue[0., 0., 0.15],
				Background -> Hue[0., 0., 0.05],
				HeaderAlignment -> {Center, Baseline},
				Alignment -> {Center, Baseline}, 
				MaxItems -> {30, 6},
				DatasetTheme -> {"FullDividers", Hue[0, 0, 0.2]},
				HeaderDisplayFunction -> (Style[#, FontWeight -> "DemiBold"]& )
			];
			Confirm @ SetOptions[MessagesNotebook[], StyleDefinitions -> "DME.nb"];
			Confirm @ SetOptions[TableView, Background-> Black,ItemStyle->{FontColor->GrayLevel[0.8]}];
			Confirm @ SetOptions[InputField,BaseStyle -> {FontColor->White} ];
			Confirm @ SetOptions[SetterBar, BaseStyle -> {FontColor -> White}];
			Confirm @ SetOptions[MenuView,  BaseStyle -> {FontColor -> White}];
			Confirm @ SetOptions[Rasterize, Background-> Black ];
			Confirm @ SetOptions[SlideView, Background-> Black ];
			Confirm @ SetOptions[MenuView,  Background-> Black ];
		]
		} ,
		If[Not@*FailureQ@enclose,
			True
			,
			Print[enclose];
			False
		]
	];
ResetOptions[Automatic|Default] := Module[{
		enclose =
			Enclose[
				Confirm @ SetOptions[ Dataset,
					HeaderStyle -> Automatic,
					HeaderBackground -> Automatic,
					Background -> Automatic,
					HeaderAlignment -> Automatic,
					Alignment -> Automatic,
					MaxItems -> Automatic,
					DatasetTheme -> Automatic,
					HeaderDisplayFunction -> Automatic
				];
				Confirm @ SetOptions[MessagesNotebook[], StyleDefinitions -> "Default.nb"];
				Confirm @ SetOptions[TableView, Background-> Automatic,ItemStyle->{FontColor->Automatic}];
				Confirm @ SetOptions[InputField,BaseStyle -> {FontColor->Automatic} ];
				Confirm @ SetOptions[SetterBar, BaseStyle -> {FontColor -> Automatic}];
				Confirm @ SetOptions[MenuView,  BaseStyle -> {FontColor -> Automatic}];
				Confirm @ SetOptions[Rasterize, Background-> Automatic ];
				Confirm @ SetOptions[SlideView, Background-> Automatic ];
				Confirm @ SetOptions[MenuView,  Background-> Automatic ];
			]
		},
		If[Not@*FailureQ@enclose,
			True
			,
			Print[enclose];
			False
		]
	];

End[];
EndPackage[];