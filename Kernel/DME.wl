(* ::Package:: *)

(* ::Section:: *)(*
	Public symbols
*)
BeginPackage["TonyAristeidou`DME`"];

SetAccentColor::usage = "SetAccentColor[color_] Sets the main accent color of the UI";
SetBackgroundColor::usage = "SetBackgroundColor[color_] Sets the main background color of the UI";
ResetOptions::usage = "ResetOptions[] Sets all default options required by the DME theme while ResetOptions[Default] resets them to the default.";
SetInitialization::usage = "SetInitialization[True] sets the initialization of the DME theme while SetInitialization[False] disables it.";
ReplaceSystemFiles::usage = "ReplaceSystemFiles[] replaces the system files with the DME theme.";
RestoreSystemFiles::usage = "RestoreSystemFiles[] restores the system files that were replaced by the DME theme.";

HelpDME::usage = "HelpDME[] opens the DME theme demo notebook.";

InstallDME::usage = "InstallDME[] installs the DME theme.";
UninstallDME::usage = "UninstallDME[] uninstalls the DME theme.";

Begin["`Private`"];

(* ::Section:: *)(*
	Initialize default colors
*)
If[Not@*ValueQ @ LocalSymbol["DME:AccentColor"],
	LocalSymbol["DME:AccentColor"] = Hue[0.47, 0.6, 0.75]
];
If[Not@*ValueQ @ LocalSymbol["DME:BackgroundColor"],
	LocalSymbol["DME:BackgroundColor"] = Hue[0.08, 0.05, 0.07]
];

(* ::Section:: *)(*
	Global variables
*)
LocalSymbol["DME:PreInstallStyleDefinitions"] =
	LocalSymbol["DME:PreInstallStyleDefinitions"] /. {
		LocalSymbol["DME:PreInstallStyleDefinitions"] :> (
			LocalSymbol["DME:PreInstallStyleDefinitions"] =
				Options[$FrontEnd, DefaultStyleDefinitions]
		)
	};
$AccentColor = LocalSymbol["DME:AccentColor"];
$BackgroundColor = LocalSymbol["DME:BackgroundColor"];
$resourcesDir = FileNameJoin[{
	PacletObject["TonyAristeidou/DME"]["Location"],
	"Resources"
}];
$dmeInitLoc = PacletObject["TonyAristeidou/DME"]["AssetLocation", "initFile"];
$userInitLoc = FileNameJoin[{$UserBaseDirectory, "Kernel", "init.m"}];

(* ::Section:: *)(*
	Help functions
*)
HelpDME[]:=SystemOpen[ PacletObject["TonyAristeidou/DME"]["AssetLocation", "DemoNB"] ];

(* ::Section:: *)(*
	Initialization functions
*)
InstallDME[] := (
	SetInitialization[True];
	ReplaceSystemFiles[];
	ResetOptions[];
	
	"DME successfully installed. \n"<>
	"Please restart your Mathematica session."
);
UninstallDME[] := (
	SetInitialization[False];
	RestoreSystemFiles[];
	ResetOptions[Default];
	
	"DME successfully uninstalled. \n"<>
	"Please restart your Mathematica session."
);

SetInitialization[True] := Module[{
		finalInit ,dmeInitCode,
		existingInitCode =  Import[
			$userInitLoc,
			"Text"
		]
	},
	Enclose[
		ConfirmAssert[
			!StringContainsQ[
				existingInitCode,
				"(*!* start:DME *!*)"~~___~~"(*!* end:DME *!*)"
			]
		];
		LocalSymbol["DME:PreInstallStyleDefinitions"] = (
			Options[$FrontEnd, DefaultStyleDefinitions] /. {
				"DME.nb" -> "Default.nb"
			}
		);
		Confirm @ SetOptions[$FrontEnd,
			DefaultStyleDefinitions -> "DME.nb"
		];
		dmeInitCode = Confirm @ Import[
			$dmeInitLoc,
			"Text"
		];
		finalInit = Confirm @ StringRiffle[{
				existingInitCode,
				dmeInitCode
			},
			"\n"
		];
		Confirm @ Export[
			$userInitLoc,
			finalInit,
			"Text",
			OverwriteTarget->True
		]
	] // !FailureQ[#]&
];
SetInitialization[False] := Module[{
		finalInit ,
		existingInitCode =  Import[
			$userInitLoc,
			"Text"
		]
	},
	Enclose[
		finalInit = Confirm @ StringDelete[
			existingInitCode,
			"(*!* start:DME *!*)"~~___~~"(*!* end:DME *!*)"
		];
		ConfirmAssert[
			!StringContainsQ[
				existingInitCode,
				"(*!* start:DME *!*)"~~___~~"(*!* end:DME *!*)"
			]
		];
		Confirm @ SetOptions[$FrontEnd,
			LocalSymbol["DME:PreInstallStyleDefinitions"]
		];
		Confirm @ Export[
			$userInitLoc,
			finalInit,
			"Text",
			OverwriteTarget->True
		]
	]// !FailureQ[#]&
];

ReplaceSystemFiles[] := Module[{
		fileNames
	},

	(* Get all file affected file names *)
	fileNames = StringDelete[
		FileNames[
			"*.*",
			FileNameJoin[{
				$resourcesDir, "SystemFiles"
			}],
			Infinity
		],
		$resourcesDir
	];

	(* Backup existing system files *)
	CopyFile[
		FileNameJoin[{$InstallationDirectory, #}], 
		FileNameJoin[{$resourcesDir, ".backup", #}],
		OverwriteTarget -> True
	] & /@ fileNames;
	
	(* Replace existing system files with DME files *)
	CopyFile[
		FileNameJoin[{$resourcesDir, #}],
		FileNameJoin[{$InstallationDirectory, #}],
		OverwriteTarget -> True
	]& /@ fileNames
  
];

RestoreSystemFiles[] := Module[{
		fileNames
	},
	(* Get all file affected file names *)
	fileNames = StringDelete[
		FileNames[
			"*.*",
			FileNameJoin[{
				$resourcesDir, "SystemFiles"
			}],
			Infinity
		],
		$resourcesDir
	];

	(* Replace DME files with .backup files *)
	CopyFile[
		FileNameJoin[{$resourcesDir, ".backup", #}],
		FileNameJoin[{$InstallationDirectory, #}], 
		OverwriteTarget -> True
	] & /@ fileNames

];

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

(* ::Section:: *)(*
	Color functions
*)
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

(* ::Section:: *)(*
	End
*)
End[];
EndPackage[];