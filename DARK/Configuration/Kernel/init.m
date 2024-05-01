
(* ::Section:: *)
(* DME initialization *)

(* ::Subsection:: *)
(* DME Package *)
BeginPackage["DME`"];
SetAccentColor::usage = "Sets the main accent color of the UI"; 
ResetOptions::usage = "Sets all default options required by the DarkModeEverything theme.";
Begin["`Private`"];

$AccentColor = Hue[0.47, 0.6, 0.75];

GetColor[h_, s_, b_] := Module[{
        clip = Clip[#,{0,1}]&,
        mod = Mod[#,1]&
    },
    Apply[
        Hue[ mod[#1+h], clip[#2+s], clip[#3+b], ##4 ]&,
        $AccentColor
    ]
];

SetAccentColor[color_?ColorQ]:= (
    $AccentColor = Hue[color]
);
SetAccentColor[Automatic|Default]:= (
    $AccentColor = Hue[0.47, 0.6, 0.75]
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


(* ::Subsection:: *)
(* MakExpressions *)
MakeExpression[RowBox[{func_, SubscriptBox["/@", level_], list_}], StandardForm] :=
    MakeExpression[
        RowBox[
            {"Map", "[", RowBox[{func, ",", list, ",", RowBox[{"{", level, "}"}]}], "]"}
        ],
        StandardForm
    ];

(* ::Subsection:: *)
(* Operations *)
ResetOptions[];