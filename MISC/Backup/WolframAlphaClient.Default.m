(* ::Package:: *)

(* ::Section::Closed:: *)
(*Package header*)


(* :Summary: Support for using the Wolfram|Alpha webservice API from within Mathematica. *)

(* :Mathematica Version: Mathematica 12 *)

(* :Author: Louis J. D'Andria *)

(* :Keywords: *)

(* :Discussion: *)

(* :Warning: *)

(* :Sources: *)


System`Private`NewContextPath[{"WolframAlphaClient`", "System`"}]

Begin["WolframAlphaClient`Private`"];


(* ::Section::Closed:: *)
(*Implementation*)


(* ::Subsection::Closed:: *)
(*$EchoMode*)


If[!ValueQ[WolframAlphaClient`Internal`$EchoMode], WolframAlphaClient`Internal`$EchoMode = False]; 


If[NameQ["System`Echo"],
	doEcho[args___] := Echo[args],
	(* simple implementation for versions that don't know about System`Echo *)
	doEcho[expr_] := doEcho[expr, "", Identity];
	doEcho[expr_, label_] := doEcho[expr, label, Identity];
	doEcho[expr_, label_, f_] := (Print[
		Style["\[RightGuillemet] ", FontColor -> Orange],
		Style[label, FontColor -> Orange], " ", f[expr]]; expr);
]


SetAttributes[echoTiming, HoldRest];

echoTiming[label_, expr_] /; WolframAlphaClient`Internal`$EchoMode := 
	Block[{start, end, result},
		If[!IntegerQ[$echoLevel] || $echoLevel < 0, $echoLevel = 0];
		start = AbsoluteTime[];
		doEcho[DateList @ start, Row[{StringJoin[Table["\t", {$echoLevel++}]], label, " STARTING"}]];
		result = expr;
		end = AbsoluteTime[];
		doEcho[DateList @ end, Row[{StringJoin[Table["\t", {--$echoLevel}]], label, " COMPLETE"}]];
		doEcho[Style[end - start, Bold], Row[{StringJoin[Table["\t", {$echoLevel}]], label, " TOTAL"}]];
		result
	]

echoTiming[label_, expr_] := expr


SetAttributes[echoInfo, HoldRest]

echoInfo[label_, info_] /; WolframAlphaClient`Internal`$EchoMode := 
	Block[{},
		If[!IntegerQ[$echoLevel] || $echoLevel < 0, $echoLevel = 0];
		doEcho[info, Row[{StringJoin[Table["\t", {$echoLevel}]], label}], Style[#, FontColor -> Gray]&]
	]

echoInfo[label_, info_] := info


SetAttributes[echoOpener, HoldRest]

echoOpener[label_, info_] /; WolframAlphaClient`Internal`$EchoMode := 
	Block[{},
		If[!IntegerQ[$echoLevel] || $echoLevel < 0, $echoLevel = 0];
		doEcho[info, Row[{StringJoin[Table["\t", {$echoLevel}]], label}], OpenerView[{"", #}, False]&]
	]

echoOpener[label_, info_] := info


echoInfo["Get @ WolframAlphaClient` STARTING", DateList[$WolframAlphaClientLoadStart = AbsoluteTime[]]]


(* ::Subsection::Closed:: *)
(*Managing FrontEnd preferences*)


(*
Preference settings managed by setPreference etc below are stored in the front end,
usually as a global option, that is, as an option to $FrontEnd. The options are
collectively stored in PrivateFrontEndOptions -> "WolframAlphaSettings" -> {...}

Since we want WolframAlpha[] to work even when a front end is not present, we do two
things. First, the set/get utilities themselves call Quiet to avoid the typical "you need
a front end to do that" message. Second, all the utilities that call these preference
management functions should make sure that the return value is sensible, and provide a
default in case it's not. An alternate solution would be to use UsingFrontEnd to guarantee
a front end is present, but that defeats one of the biggest benefits of working in a raw
kernel.
*)


dynamicPreference[opt_] := Dynamic[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}]]

dynamicPreference[opt_, default_] := Dynamic[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}, default]]

dynamicPreference[target_, opt_, default_] := Dynamic[CurrentValue[target, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}, default]]


getPreferences[] := Quiet[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings"}]]

getPreference[opt_] := Quiet[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}]]

getPreference[opt_, default_] := Quiet[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}, default]]

getPreference[target_, opt_, default_] := Quiet[CurrentValue[target, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}, default]]


getStringPreference[opt_, default_String] := Quiet[Replace[getPreference[opt, default], Except[_String] -> default]]


setPreference[opt_, val_] := Quiet[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}] = val]

setPreference[target_, opt_, val_] := Quiet[CurrentValue[target, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}] = val]


clearPreferences[] := Quiet[
	CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings"}] = {};
	$AlphaQuerySendMathematicaSessionInfo = Automatic;
]

clearPreference[opt_] := setPreference[opt, Inherited]



cachedMessageTemplate[resource_, name_] :=
	cachedMessageTemplate[resource, name] = If[TrueQ[$Notebooks], FrontEndResource[resource, name], name]

cachedFrontEndResource[args__] := cachedFrontEndResource[args] = FrontEndResource[args]



(*bitmapResource[name_] := With[{file = name <> ".png"}, Dynamic[RawBoxes[FEPrivate`ImportImage[FrontEnd`FileName[{"WolframAlphaClient"}, file]]]]]*)

bitmapResource[name_] := Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", name]]]

bitmapResource[name_, opts__] := Style[bitmapResource[name], GraphicsBoxOptions -> {opts}]


iconResource[name_] := RawBoxes @ DynamicBox[FEPrivate`ImportImage[FrontEnd`FileName[{"WolframAlphaClient"}, name]]]

imageResource[name_] := FrontEnd`FileName[{"WolframAlphaClient"}, name]


stringResource[id_] := stringResource[id] = 
	If[$CloudControlEqual,
		FrontEndResource["WAStrings", id],
		Dynamic[FEPrivate`FrontEndResource["WAStrings", id]]
	]

rawStringResource[id_] := FrontEndResource["WAStrings", id]



(* ::Subsection::Closed:: *)
(*Global variables*)


$AlphaQueryClientLocation = System`Private`$InputFileName


$AlphaQueryBaseURLProtocol = "https://";


$AlphaQueryBaseURLs := {
	$AlphaQueryBaseURLProtocol <> "api.devel.wolframalpha.com/v1/" -> "Devel",
	$AlphaQueryBaseURLProtocol <> "api.test.wolframalpha.com/v1/" -> "Test",
	$AlphaQueryBaseURLProtocol <> "www.test2.wolframalpha.com/api/v1/" -> "Test2",
	$AlphaQueryBaseURLProtocol <> "www.current.wolframalpha.com/api/v1/" -> "Current",
	$AlphaQueryBaseURLProtocol <> "preview.wolframalpha.com/api/v1/" -> "Preview",
	$AlphaQueryBaseURLProtocol <> "ja.wolframalpha.com/api/v1/" -> "Japanese",
	$AlphaQueryBaseURLProtocol <> "es.wolframalpha.com/api/v1/" -> "Spanish",
	$AlphaQueryBaseURLProtocol <> "es.current.wolframalpha.com/api/v1/" -> "Current-Spanish",
	$AlphaQueryBaseURLProtocol <> "api.wolframalpha.com/v1/" -> "Production",
	$AlphaQueryBaseURLProtocol <> "www.centos6-dev.wolframalpha.com/api/v1/" -> "centos6-dev",
	$AlphaQueryBaseURLProtocol <> "www.centos6-cur.wolframalpha.com/api/v1/" -> "centos6-cur",
	$AlphaQueryBaseURLProtocol <> "www.centos6-test.wolframalpha.com/api/v1/" -> "centos6-test",
	$AlphaQueryBaseURLProtocol <> "www.centos6-test2.wolframalpha.com/api/v1/" -> "centos6-test2",
	$AlphaQueryBaseURLProtocol <> "api-maps.wolframalpha.com/v1/" -> "Maps"
}


$AlphaQueryBaseURL[] := $AlphaQueryBaseURL[getPreference["BaseURL", "Automatic"]]

$AlphaQueryBaseURL[Automatic] := $AlphaQueryBaseURL[getStringPreference["BaseURL", "Automatic"]]

$AlphaQueryBaseURL[url_] :=
	Block[{baseurl, reversedrules = Map[Reverse, $AlphaQueryBaseURLs]},
		baseurl = If[StringQ[url] && (StringTrim[url] =!= ""), url, "Automatic"];
		baseurl = Switch[ ToLowerCase[baseurl],
				"devel", "Devel" /. reversedrules,
				"test", "Test" /. reversedrules,
				"test2", "Test2" /. reversedrules,
				"current", "Current" /. reversedrules,
				"preview", "Preview" /. reversedrules,
				"japanese" | "ja", "Japanese" /. reversedrules,
				"spanish" | "es", "Spanish" /. reversedrules,
				"current-spanish", "Current-Spanish" /. reversedrules,
				"production" | "public" | "api" | "english" | "en", "Production" /. reversedrules,
				"centos6-dev", "centos6-dev" /. reversedrules,
				"centos6-cur", "centos6-cur" /. reversedrules,
				"centos6-test", "centos6-test" /. reversedrules,
				"centos6-test2", "centos6-test2" /. reversedrules,
				"maps", "Maps" /. reversedrules,
				"automatic",
					Switch[$Language,
						"Japanese", "Japanese" /. reversedrules,
						"English" | _, "Production" /. reversedrules
					],
				_, baseurl (* any other string is assumed to be valid base url *)
		]
	];


$AlphaQueryJSP = "query.jsp";


$AlphaParseJSP = "mparse.jsp";


$AlphaNotebookParseJSP = "wanparse.jsp";


$AlphaValidateJSP = "validatequery";


$AlphaSimpleJSP = "simple.jsp";


$AlphaSpokenJSP = "spoken.jsp";


$AlphaResultJSP = "result.jsp";


$AlphaComputeJSP = "compute.jsp";


$AlphaMInterfaceJSP = "minterface.jsp";


$AlphaQueryAppID[] := With[{id = getPreference["AppID", Automatic]}, If[StringQ[id] && id =!= "Automatic", id, Automatic]];


$AlphaQueryLanguageRules = {
	"English" -> "en",
	"Japanese" -> "ja",
	"ChineseSimplified" -> "zh",
	"ChineseTraditional" -> "zh",
	"French" -> "fr",
	"German" -> "de",
	"Italian" -> "it",
	"Korean" -> "ko",
	"Spanish" -> "es",
	_ -> "en"
};


$AlphaQueryLanguageCode[lang_] :=
	Block[{language = lang},
		If[Not @ StringQ[language], language = getPreference["Language", Automatic]];
		If[language === "Automatic", language = Automatic];
		If[Not @ StringQ[language], language = Quiet[CurrentValue[Language]]];
		If[Not @ StringQ[language], language = $Language];
		If[Not @ StringQ[language], language = "English"];
		Which[
			(* Allow "<2 letter language code>" or "<2 letter language code>-<region code>" to go through as is *)
			StringLength[language] === 2, language,
			StringLength[language] > 4 && StringTake[language, {3}] === "-", language,
			(* otherwise, translate to a language code *)
			True, language /. $AlphaQueryLanguageRules
		]
	]


$AlphaQueryMClient = "2";


$AlphaQueryMMode = "function";


$AlphaQueryPrefixQ = False;


$AlphaQueryPrefix = "devmode: ";


$AllAlphaQueryAppearanceElements = {"Extrusion", "Warnings", "CDFWarnings", "Assumptions", "CDFAssumptions", "Brand", "CDFBrand", "ContentPadding", "Pods", "UseInput", "UseInputLeft", "TearOffButtons", "PodMenus", "CDFPodMenus", "PodCornerCurl", "SubpodMenus", "Unsuccessful", "Sources", "PluginAndPacletCheck"};


$AutomaticAlphaQueryAppearanceElements = {"Warnings", "Assumptions", "Brand", "Pods", "PodMenus", "Unsuccessful", "Sources"};


$AlphaQueryAppearanceElements = $AutomaticAlphaQueryAppearanceElements;

$APITimeZone = None;

$APILatLong = None;

$APIUnitSystem = None;

$APITimeout = None;


(* Disable these interfaces, now that W|A can return interfaces when given interactive=true *)
(*
$AlphaQueryInterfaceAddedToMInput["N"] = "Manipulate"
$AlphaQueryInterfaceAddedToMInput["ContinuedFraction"] = "Manipulate"
$AlphaQueryInterfaceAddedToMInput["Series"] = "Manipulate"
$AlphaQueryInterfaceAddedToMInput["Plot"] = "Explore"
$AlphaQueryInterfaceAddedToMInput["Plot3D"] = "Manipulate"
$AlphaQueryInterfaceAddedToMInput["ContourPlot"] = "Manipulate"
*)


$AlphaQueryAsync[] := With[{async = getPreference["Asynchronous", True]}, If[MatchQ[async, True | False | All | _?NumericQ], async, True]]


$AlphaQueryInteractive[] := With[{int = getPreference["Interactive", True]}, If[MemberQ[{True, False}, int], int, True]];


$AlphaQuerySimpleRecalculate[] := With[{re = getPreference["Recalculate", True]}, If[MemberQ[{True, False}, re], re, True]];


$AlphaQueryRecalculateSpace = 30;


$AlphaQueryReinterpret[] := With[{re = getPreference["Reinterpret", True]}, If[MemberQ[{True, False}, re], re, True]];


$AlphaQueryUseURLFetch = True;


$AlphaQueryBodyByteArrayXML = True;


$AlphaQueryFormats = {"cell", "minput", "msound", "dataformats"};


$AlphaQueryDataFormats = {"ComputableData", "FormattedData", "FormulaData", "NumberData", "QuantityData", "SoundData", "TimeSeriesData"};


$AlphaQueryTextFormats = {"Plaintext", "Input", "Output"};


$AlphaQueryPodWidth = Automatic;


$AlphaQueryExtrusionShowMOutputs = True;


$AlphaQueryExtrusionClickClose[] := With[{close = getPreference["ExtrusionClickClose", False]}, If[MemberQ[{True, False}, close], close, False]];


$AlphaQueryExtrusionClickEvaluate[] := With[{eval = getPreference["ExtrusionClickEvaluate", False]}, If[MemberQ[{True, False}, eval], eval, False]];


$AlphaQueryExtrusionEvaluateCachedMForms = True;


$AlphaQueryExtrusionWidth = 560;


$AlphaQueryQuiet = Automatic;


$AlphaQueryLogNotebook = Automatic;


$AlphaQueryAsynchoronousTimeout = Automatic;


$AlphaQueryTimeConstraint = 20;


$AlphaQueryScanTimeout = Automatic;


$AlphaQueryPodTimeout = Automatic;


$AlphaQueryFormatTimeout = Automatic;


$AlphaQueryDefaultAssumptionsVersion = "2";


$AlphaQueryUseAssumptionTemplates = True;


$AlphaQuerySendMathematicaSessionInfo = Automatic;


$AlphaQueryIncludeSemanticTypes = True;


$AlphaQuerySkeletonEncoding = 1;


$AlphaQueryIncludeEvaluationCloudObjectURL = True;


$AlphaQueryGeoLocation := If[TrueQ[$SynchronousEvaluation] || !TrueQ[$AllowInternet],
	(* don't calculate location during synchronous evaluations, or if internet is disallowed *)
	None,
	$GeoLocation
];


$AlphaQueryShowLinksByDefault = True;


$AlphaQueryAdditionalInformation = False;


$sendSessionInfoDefault = 
	If[
		Or[
			$VersionNumber > 12.0,
			$VersionNumber == 12.0 && $ReleaseNumber > 0,
			BoxForm`sufficientVersionQ[12.0, 1]
		],
		(* The default value has changed to True in 12.0.1 *)
		True,
		Automatic
	];


With[{default = $sendSessionInfoDefault},
	$AlphaQueryGlobalSendMathematicaSessionInfo[] :=
		With[{send = getPreference[$FrontEnd, "SendMathematicaSessionInfo", default]}, If[MemberQ[{True, False, Automatic}, send], send, default] ]
];


$AlphaQueryRawParameters = {};


$AlphaQueryMathematicaFormsFormatType[] := 
	With[{fmt = getPreference["MathematicaFormsFormatType", "MostlyInputForm"]},
		If[MemberQ[{"InputForm", "StandardForm", "TraditionalForm"}, fmt], fmt, "MostlyInputForm"] ]




$MostlyInputFormTypesetHeads = "Placeholder" | "Out" | "Quantity" | "Entity" | "EntityClass" | "EntityProperty" | "EntityValue" | "WolframAlphaClient`Private`rawCompressedBoxes";


$MostlyInputFormEvaluateAndTypesetHeads = "DateObject" | "TimeObject";


$MostlyInputFormTypesetCurriedHeadsANB = "Derivative";


$MostlyInputFormTypesetInnerHeads = "Graphics" | "Graphics3D";


$MostlyInputFormTypesetInnerThreshhold = 15000;


$WolframAlphaNotebookFormToStringOverrides = {And, Or, Not};


$TrialProductQ := $TrialProductQ = TrueQ[FE`Evaluate[FEPrivate`TrialQ[]]];


sbsUpgradeURL[] = 
	Replace[Lookup[$ProductInformation, "ProductIDName"], {
		"Mathematica" -> "https://www.wolfram.com/mathematica/pricing/",
		"WolframDesktop" -> "https://www.wolfram.com/mathematica/pricing/",
		"WolframAlphaNB" -> "https://www.wolfram.com/wolfram-alpha-notebook-edition/pricing/",
		_ -> "https://www.wolfram.com/"
	}]


If[Not @ StringQ @ WolframAlpha::trpr,
	WolframAlpha::trpr = "Step\[Hyphen]by\[Hyphen]step capabilities are not available in this trial product. " <>
		ToString[Hyperlink["Upgrade now \[RightGuillemet]", sbsUpgradeURL[]], StandardForm]
]




(* ::Subsection::Closed:: *)
(*Typesetting utilities*)


mparseMakeExpression[str_String] := ToExpression[str, InputForm, HoldComplete] // resolveSessionReferences


(*
resolveSessionReferences takes an expression wrapped in HoldComplete, and returns an
expression wrapped in HoldComplete.

This is where parses which are intended to be processed on the client side before being
given to the user are manipulated. Details of which parses and in what ways the should be
processed will be determined in cooperation with JMichelson and Falloon.
*)


(* Sample rule, just to see the mechanism working *)
(*resolveSessionReferences[HoldComplete[Entity["Country", "Australia"]]] := HoldComplete[Out[-1]]*)


(* Fall-through case *)
resolveSessionReferences[else_] := else /.
	{
		HoldPattern[Placeholder[_ -> patt_]] /; MatchQ[HoldComplete @@ {Out[-1]}, HoldComplete[patt]] :> Out[-1]
	} /.
	{
		HoldPattern[Placeholder[tp_ -> _]] :> Placeholder[tp]
	}



(* 
alphaQueryToString could simply use Block to avoid the formatting
rules for most symbols, but that doesn't work if the symbols are
locked. Here's a version that does.
*)

alphaQueryToString[HoldComplete[expr_], "MostlyInputForm"] := ToString[Unevaluated[expr], InputForm]

alphaQueryToString[HoldComplete[expr_], "WolframAlphaNotebookForm"] :=
Module[{syms, uniqueSyms, str},
	syms = $WolframAlphaNotebookFormToStringOverrides;
	uniqueSyms = Symbol["WolframAlphaClient`Private`" <> SymbolName[#]]& /@ syms;
	str = Replace[
		HoldComplete[expr] /. Thread[syms -> uniqueSyms],
		HoldComplete[x_] :> ToString[Unevaluated[x], InputForm]
	];
	StringReplace[str, Thread[(ToString /@ uniqueSyms) -> (ToString /@ syms)]]
]



SetAttributes[AlphaQueryMakeBoxes, HoldAllComplete];

AlphaQueryMakeBoxes[expr_] := With[{fmt = $AlphaQueryMathematicaFormsFormatType[]}, AlphaQueryMakeBoxes[expr, fmt]]

AlphaQueryMakeBoxes[expr_, fmt:("InputForm" | "StandardForm" | "TraditionalForm" | "MostlyInputForm" | "WolframAlphaNotebookForm")] :=
	alphaQueryMakeBoxes[HoldComplete[expr] /.
		(* Contractually, Uncompress expressions should always be expanded during formatting *)
		HoldPattern[Uncompress][a_String] :> Block[{}, Uncompress[a] /; True] /.
		(*
			This rule makes Images renderable in typeset forms, including MostlyInputForm.
			The rawCompressedBoxes wrapper is used so that the result of evaluating the Image
			will survive the coercion to an InputForm string in MostlyInputForm, below.
		*)
		If[fmt === "InputForm",
			{},
			HoldPattern[Image][a_, b___] /; Or[
				MatchQ[Unevaluated[a], _RawArray | _NumericArray],
				MatrixQ[Unevaluated[a]] && Depth[Unevaluated[a]] == 3 && FreeQ[Unevaluated[a], Except[List, _Symbol], {2}]
			] :> With[{image = rawCompressedBoxes[Compress[ToBoxes[Image[a, b]]]]}, image /; True]
		],
		fmt
	]

AlphaQueryMakeBoxes[expr_, other_] := AlphaQueryMakeBoxes[expr, "InputForm"]


alphaQueryMakeBoxes[HoldComplete[expr_], "InputForm"] := ToString[Unevaluated[expr], InputForm]
alphaQueryMakeBoxes[HoldComplete[expr_], "StandardForm"] := BoxForm`MakeBoxesWithTextFormatting[expr, StandardForm]
alphaQueryMakeBoxes[HoldComplete[expr_], "TraditionalForm"] := BoxForm`MakeBoxesWithTextFormatting[expr, TraditionalForm]
alphaQueryMakeBoxes[HoldComplete[expr_], fmt: ("MostlyInputForm" | "WolframAlphaNotebookForm")] := 
	UsingFrontEnd[MathLink`CallFrontEnd @ FrontEnd`ReparseBoxStructurePacket @ 
		alphaQueryToString[HoldComplete[expr], fmt]
	] /. {
		(* Some exprs are considered safe to evaluate and then typeset in "MostlyInputForm" -- 373561 *)
		boxes:RowBox[{$MostlyInputFormEvaluateAndTypesetHeads, "[", _, "]"}] :> 
			ToBoxes[ToExpression[boxes, StandardForm], StandardForm] /; (fmt === "MostlyInputForm"),
		(* Some exprs should always be typeset as if they were in StandardForm *)
		boxes:RowBox[{$MostlyInputFormTypesetHeads, "[", _, "]"}] :>
			Replace[BoxForm`ConvertForm[boxes, StandardForm, StandardForm, "Output"], BoxData[{a_}, ___] :> a],
		boxes:RowBox[{RowBox[{$MostlyInputFormTypesetCurriedHeadsANB, "[", _, "]"}], "[", _, "]"}] :>
			Replace[BoxForm`ConvertForm[boxes, StandardForm, StandardForm, "Output"], BoxData[{a_}, ___] :> a] /; (fmt === "WolframAlphaNotebookForm"),
		(* Other exprs should be typeset only if they are inside of something else, and even then only if they are sufficienty verbose *)
		box_[x___, gboxes:RowBox[{$MostlyInputFormTypesetInnerHeads, "[", _, "]"}], y___] /; (ByteCount[gboxes] > $MostlyInputFormTypesetInnerThreshhold) :>
			box[x, Replace[BoxForm`ConvertForm[gboxes, StandardForm, StandardForm, "Output"], BoxData[{a_}, ___] :> a], y],
		(* ResourceFunction[str] is handled specially *)
		boxes: RowBox[{"ResourceFunction", "[", _, "]"}] :> resourceFunctionBoxes[boxes]
	}


rawCompressedBoxes /: MakeBoxes[rawCompressedBoxes[str_String], fmt_] := Uncompress[str]


(* resourceFunctionBoxes contributed by RichardH *)
resourceFunctionBoxes[boxes:RowBox[{"ResourceFunction", "[", idboxes_, "]"}]] :=
	Module[{name, nameboxes, display},
		name = Replace[
			MakeExpression[idboxes, StandardForm],
			{
				(* The ResourceFunction is only specified by name *)
				HoldComplete[s_String] :> s,
				(* A full ResourceFunction, but association is uninitialized *)
				HoldComplete[ResourceObject[Association[___, "Name" -> s_String, ___], ___]] :> s,
				(* A full ResourceFunction with an initialized association *)
				HoldComplete[ResourceObject[KeyValuePattern["Name" -> s_String], ___]] :> s,
				(* Something else *)
				___ :> Return[boxes]
			}
		];
		nameboxes = ToBoxes[name];
		ResourceFunction; (* controlled autoloding *)
		
		display = Replace[
			(* InertResourceFunctionBoxes will be in a forthcoming FunctionResource paclet update *)
			FunctionResource`InertResourceFunctionBoxes[nameboxes],
			_FunctionResource`InertResourceFunctionBoxes :> Replace[
				(* templateBoxFunction is in the currently released FunctionResource paclet, but not base 12.0 *)
				FunctionResource`Private`templateBoxFunction[
					FunctionResource`Private`$publishedIconColor,
					nameboxes
				],
				_FunctionResource`Private`templateBoxFunction :> With[{name = name},
					(* makeResourceFunctionBoxes is in the released 12.0 and later *)
					FunctionResource`Private`makeResourceFunctionBoxes[name]
				]
			]
		];
		With[{display = display, name = name},
			InterpretationBox[display, ResourceFunction[name], Selectable -> False]
		]
	]


(* ::Subsection::Closed:: *)
(*AlphaQueryPreferences[]*)


(*
Note that this dialog affects the behavior of AlphaQuery[] (aka '=='), not the general
behavior of WolframAlpha[]. WolframAlpha[] respects the dialog's "Server" setting, but
nothing else.
*)


setServer["other"] := Block[{other = InputString["Enter a URL:"]}, If[StringQ[other], setPreference["BaseURL", other], other]];

setBehavior["default"] := (
	$AlphaQueryAppearanceElements = $AutomaticAlphaQueryAppearanceElements;
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"cell", "minput", "msound", "dataformats"};
	$AlphaQueryPodWidth = Automatic;
	$AlphaQueryRawParameters = {};
)
setBehavior["CDF"] := (
	$AlphaQueryAppearanceElements = {"CDFWarnings", "CDFAssumptions", "CDFBrand", "ContentPadding", "Pods", "CDFPodMenus", "PodCornerCurl", "Unsuccessful", "Sources"};
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"cell", "minput", "msound", "dataformats", "imagemap"};
	$AlphaQueryPodWidth = Automatic;
	$AlphaQueryRawParameters = {};
)
setBehavior["no inputs"] := (
	$AlphaQueryAppearanceElements = {"Warnings", "Assumptions", "Brand", "Pods", "PodMenus", "Unsuccessful", "Sources"};
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"cell", "msound", "dataformats"};
	$AlphaQueryPodWidth = Automatic;
	$AlphaQueryRawParameters = {};
)
setBehavior["only inputs"] := (
	$AlphaQueryAppearanceElements = {"UseInputLeft"};
	setPreference["Asynchronous", False];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"minput"};
	$AlphaQueryPodWidth = Automatic;
	$AlphaQueryRawParameters = {};
)
setBehavior["images"] := (
	$AlphaQueryAppearanceElements = {"Warnings", "Assumptions", "Brand", "Pods", "Unsuccessful", "Sources"};
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"image", "msound"};
	$AlphaQueryPodWidth = Automatic;
	$AlphaQueryRawParameters = {};
)
setBehavior["iPhone images"] := (
	$AlphaQueryAppearanceElements = {"Warnings", "Assumptions", "Brand", "Pods", "Unsuccessful", "Sources"};
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"image"};
	$AlphaQueryPodWidth = {308, 468};
	$AlphaQueryRawParameters = {};
)
setBehavior["iPhone landscape images"] := (
	$AlphaQueryAppearanceElements = {"Warnings", "Assumptions", "Brand", "Pods", "Unsuccessful", "Sources"};
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"image"};
	$AlphaQueryPodWidth = {468, 468};
	$AlphaQueryRawParameters = {};
)
setBehavior["tear off buttons"] := (
	$AlphaQueryAppearanceElements = Flatten[{DeleteCases[$AutomaticAlphaQueryAppearanceElements, "UseInput" | "UseInputLeft" | "PodMenus" | "SubpodMenus"], "TearOffButtons"}];
)
setBehavior["pod menus"] := (
	$AlphaQueryAppearanceElements = Flatten[{DeleteCases[$AutomaticAlphaQueryAppearanceElements, "UseInput" | "UseInputLeft" | "TearOffButtons" | "SubpodMenus"], "PodMenus"}];
)
setBehavior["pod and subpod menus"] := (
	$AlphaQueryAppearanceElements = Flatten[{DeleteCases[$AutomaticAlphaQueryAppearanceElements, "UseInput" | "UseInputLeft" | "TearOffButtons"], "PodMenus", "SubpodMenus"}];
)
setBehavior["no tear offs"] := (
	$AlphaQueryAppearanceElements = Flatten[{DeleteCases[$AutomaticAlphaQueryAppearanceElements, "UseInput" | "UseInputLeft" | "TearOffButtons" | "PodMenus" | "SubpodMenus"]}];
)



openerPanel[{label_, expr_}, open_:True] := OpenerView[{label, Panel[expr]}, open]



AlphaQueryPreferences[] :=
CreateDialog[{
	openerPanel[{
		"Server",
		Column[{
			RadioButtonBar[dynamicPreference["BaseURL", "Automatic"], {"Automatic", "English" -> "Production-English", "Japanese" -> "Production-Japanese", "Spanish" -> "Production-Spanish"}, Appearance -> ("Horizontal" -> {1, Automatic}), Method -> "Active"],
			RadioButtonBar[dynamicPreference["BaseURL", "Automatic"], DeleteCases[Last /@ $AlphaQueryBaseURLs, "Maps" | "Automatic" | "Production" | "English" | "Japanese" | "Spanish"], Appearance -> ("Horizontal" -> {2, Automatic}), Method -> "Active"],
			Button["Other\[Ellipsis]", setServer["other"], Method -> "Queued", BaseStyle -> {}, ImageSize -> Automatic]
		}, Dividers -> {False, {False, LightGray}}]
	}],
	openerPanel[{
		"Front End Preferences (changes will persist in future sessions)",
		Grid[{
			{"Wolfram|Alpha server", InputField[dynamicPreference["BaseURL", "Automatic"], String, FieldSize -> {{20,20},{1,Infinity}}]},
			{"AppID", InputField[dynamicPreference["AppID", "Automatic"], String, FieldSize -> {{20,20},{1,Infinity}}]},
			{"Language code", InputField[dynamicPreference["Language", "Automatic"], String, FieldSize -> {{20,20},{1,Infinity}}]},
			{"", Row[{Checkbox[dynamicPreference["Asynchronous", True]], "Asynchronous"}]},
			{"", Row[{Checkbox[dynamicPreference["Interactive", True]], "Interactive"}]},
			{"", Row[{Checkbox[dynamicPreference["Reinterpret", True]], "Reinterpret"}]},
			{"", Row[{Checkbox[dynamicPreference["Recalculate", True]], "Recalculate"}]},
			{"", Row[{Checkbox[dynamicPreference["MathematicaFormsFormatType", "MostlyInputForm"], {"MostlyInputForm", "StandardForm"}], "Typeset Mathematica forms"}]},
			{"Clicking in the extrusion", Row[{Checkbox[dynamicPreference["ExtrusionClickClose", False]], "closes the extrusion  ", Checkbox[dynamicPreference["ExtrusionClickEvaluate", False]], "evaluates the cell"}]},
			{"Mathematica Session Info",
				ActionMenu[
					Dynamic[Switch[{
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}, $sendSessionInfoDefault],
							$AlphaQuerySendMathematicaSessionInfo},
						{True, _}, "Always send",
						{False, _}, "Never send",
						{_, True}, "Send for this session",
						{_, False}, "Don't send for this session",
						{_, _}, "Ask before sending"
					]],
					{
						"Ask before sending" :> (
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}] = Automatic;
							$AlphaQuerySendMathematicaSessionInfo = Automatic;),
						Delimiter,
						"Never send" :> (
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}] = False;
							$AlphaQuerySendMathematicaSessionInfo = False;),
						"Don't send for this session" :> (
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}] = Automatic;
							$AlphaQuerySendMathematicaSessionInfo = False;),
						"Send for this session" :> (
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}] = Automatic;
							$AlphaQuerySendMathematicaSessionInfo = True;),
						"Always send" :> (
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}] = True;
							$AlphaQuerySendMathematicaSessionInfo = True;)
					},
					Appearance -> "PopupMenu",
					DefaultBaseStyle -> {},
					DefaultMenuStyle -> {}
				]
			},			
			{"", Item[Row[{Button["Help\[Ellipsis]", SystemOpen["paclet:ref/WolframAlpha"], BaseStyle -> {}], Button["Reset", clearPreferences[], BaseStyle -> {}]}], Alignment -> Right]}
		}, Alignment ->{{Right, Left}}]
	}, False],
	openerPanel[{
		"Kernel Preferences (changes will reset in future sessions)",
		Grid[{
			(* {"Query JSP", InputField[Dynamic[$AlphaQueryJSP], String, FieldSize -> {{40,40},{1,Infinity}}]}, *)
			{"protocol", InputField[Dynamic[$AlphaQueryBaseURLProtocol], String, FieldSize -> {{40,40},{1,Infinity}}]},
			{Row[{Checkbox[Dynamic[$AlphaQueryPrefixQ]], "Query prefix"}], InputField[Dynamic[$AlphaQueryPrefix], String, FieldSize -> {{40,40},{1,Infinity}}, Enabled -> Dynamic[$AlphaQueryPrefixQ]]},
			{"", openerPanel[{
				"Timeouts",
				Grid[{
					{"scan timeout", InputField[Dynamic[$AlphaQueryScanTimeout], Expression, FieldSize -> {{20,20},{1,Infinity}}]},
					{"pod timeout", InputField[Dynamic[$AlphaQueryPodTimeout], Expression, FieldSize -> {{20,20},{1,Infinity}}]},
					{"format timeout", InputField[Dynamic[$AlphaQueryFormatTimeout], Expression, FieldSize -> {{20,20},{1,Infinity}}]},
					{"async timeout", InputField[Dynamic[$AlphaQueryAsynchoronousTimeout], Expression, FieldSize -> {{20,20},{1,Infinity}}]}}, Alignment ->{{Right, Left}}]}, False]},
			{"", Row[{Checkbox[Dynamic[$AlphaQueryExtrusionEvaluateCachedMForms]], "Evaluate cached minputs and moutputs"}]},
			{"", openerPanel[{"Formats", CheckboxBar[Dynamic[$AlphaQueryFormats], {"image", "cell", "minput", "moutput", "sound", "msound", "dataformats", "computabledata", "formatteddata", "imagemap"}, Appearance -> "Vertical" -> {Automatic, 2}]}, False]},
			{"", openerPanel[{"AppearanceElements", CheckboxBar[Dynamic[$AlphaQueryAppearanceElements], $AllAlphaQueryAppearanceElements, Appearance -> "Vertical" -> {Automatic, 2}]}, False]},
			{"", Row[{Checkbox[Dynamic[$AlphaQueryExtrusionShowMOutputs]], "Show moutputs in the extrusion"}]},
			{"", Row[{Checkbox[Dynamic[$AlphaQueryShowLinksByDefault]], "Show links by default"}]},
			{"", Row[{Checkbox[Dynamic[$AlphaQueryAdditionalInformation]], "Show additional information"}]},
			{"", Row[{Checkbox[Dynamic[$AlphaQueryUseURLFetch]], "Use URLFetch+ImportString instead of Import"}]},
			{"", Row[{Checkbox[Dynamic[$AlphaQueryBodyByteArrayXML]], "Fetch the BodyByteArray when importing XML"}]},
			{"", openerPanel[{"control-equal", Column[{
				Row[{Checkbox[Dynamic[$LinguisticAssistantTemplateBox]], "Use TemplateBox for control-equal"}],
				Row[{"\t", Checkbox[Dynamic[$LinguisticAssistantTemplateBoxOptions], Enabled -> Dynamic[$LinguisticAssistantTemplateBox]], "Include TemplateBox options"}],
				Row[{Checkbox[Dynamic[$LinguisticAssistantCompact]], "Use compact display for control-equal"}],
				Row[{Checkbox[Dynamic[$LinguisticAssistantUsesURLSaveAsynchronous]], "Use custom async features for control-equal"}],
				Row[{Checkbox[Dynamic[$LinguisticAssistantTrackQueryState]], "Track failure states in control-equal"}],
				Row[{Checkbox[Dynamic[$LinguisticAssistantEntityDisplay]], "Custom control-equal display for Entity and EntityClass"}],
				Row[{Checkbox[Dynamic[$LinguisticAssistantSideEffects]], "Allow mparse.jsp to evaluate side effects"}],
				Row[{Checkbox[Dynamic[$CloudControlEqualOverride]], "Cloud control-equal"}] }] }, False]},
			{"", openerPanel[{"W|A Notebook Edition", Column[{
				Row[{Checkbox[Dynamic[$WolframAlphaNotebookWANParse]], "Use wanparse.jsp for NaturalLanguageInputParse"}],
				Row[{Checkbox[Dynamic[$TryClientParseRawWL]], "Try parsing the query string locally if wanparse fails"}],
				Row[{Checkbox[Dynamic[$IncludeSCUTags]], "Include usage tags in DeployedWLInput cells"}],
				Row[{Checkbox[Dynamic[$ShowStepsQueryUsesParse]], "Use the parse when building show steps queries"}],
				Row[{Checkbox[Dynamic[$ShowStepsAllowTypesetting]], "Allow typesetting in show steps queries"}],
				Row[{Checkbox[Dynamic[$ShowStepsTypesetQuery], Enabled -> Dynamic[$ShowStepsAllowTypesetting]], "Typeset the show steps query"}]}] }, False]},
			{"", openerPanel[{"session info", Grid[{
				{"Send semantic types", SetterBar[Dynamic[$AlphaQueryIncludeSemanticTypes], {True, False, Automatic}]},
				{"Skeleton encoding", SetterBar[Dynamic[$AlphaQuerySkeletonEncoding], {None, 1}]}}, Alignment ->{{Right, Left}}]}, False]},
			{"", Row[{Checkbox[Dynamic[WolframAlphaClient`Internal`$EchoMode]], "Echo mode"}]},
			{"", openerPanel[{
				"Assumptions",
				Grid[{
					{"Default assumptionsversion", SetterBar[Dynamic[$AlphaQueryDefaultAssumptionsVersion], {None, "2"}]},
					{"Use assumption templates", Checkbox[Dynamic[$AlphaQueryUseAssumptionTemplates]]},
					{"Debug assumption templates", Checkbox[Dynamic[$DebugAssumptionTemplates]]}
					}, Alignment ->{{Right, Left}}]}, False]},
			{"Quiet", SetterBar[Dynamic[$AlphaQueryQuiet], {True, False, "Log", Automatic}]},
			{"Pod width", InputField[Dynamic[$AlphaQueryPodWidth], Expression, FieldSize -> {{40,40},{1,Infinity}}]},
			{"Time constraint", InputField[Dynamic[$AlphaQueryTimeConstraint], Expression, FieldSize -> {{40,40},{1,Infinity}}]},
			{"Other parameters", InputField[Dynamic[$AlphaQueryRawParameters], Expression, FieldSize -> {{40,40},{1,Infinity}}]},
			{"", Button["Sample URL\[Ellipsis]", CreateDocument[ExpressionCell[WolframAlpha["sinx", "URL", alphaQueryOptions[]], "Input"]], Method -> "Queued", ImageSize -> Automatic, BaseStyle -> {}]}
		}, Alignment ->{{Right, Left}}]
	}],
	openerPanel[{
		"Display behavior",
		Row[{
			Button["Default", setBehavior["default"], BaseStyle -> {}],
			Button["CDF", setBehavior["CDF"], BaseStyle -> {}],
			Button["No inputs", setBehavior["no inputs"], BaseStyle -> {}],
			Button["Only inputs", setBehavior["only inputs"], BaseStyle -> {}], 
			Button["Use images", setBehavior["images"], BaseStyle -> {}],
			Button["images, iPhone", setBehavior["iPhone images"], BaseStyle -> {}],
			Button["images, iPhone landscape", setBehavior["iPhone landscape images"], BaseStyle -> {}]
		}]
	}, False],
	openerPanel[{
		"Tear off behavior",
		Row[{
			Button["Default", setBehavior["pod menus"], BaseStyle -> {}],
			Button["Pod and subpod menus", setBehavior["pod and subpod menus"], BaseStyle -> {}],
			Button["Buttons", setBehavior["tear off buttons"], BaseStyle -> {}],
			Button["None", setBehavior["no tear offs"], BaseStyle -> {}]
		}]
	}, False]
	},
	WindowTitle -> "AlphaQuery Preferences",
	NotebookEventActions -> {},
	WindowElements -> {"VerticalScrollBar"},
	WindowFrameElements -> {"CloseBox", "ResizeArea"}
]


alphaQueryOptions[] := Sequence @@ {
	AppearanceElements -> $AlphaQueryAppearanceElements,
	Asynchronous -> Block[{async, asynctimeout},
		async = $AlphaQueryAsync[];
		asynctimeout = $AlphaQueryAsynchoronousTimeout;	
		(* Interpret True as All *)
		If[ async === True, async = All];
		(* Interpret All as $AlphaQueryAsynchoronousTimeout if it's set *)
		If[ async === All && NumericQ[$AlphaQueryAsynchoronousTimeout],
			async = $AlphaQueryAsynchoronousTimeout
		];
		async
	],
	TimeConstraint -> {$AlphaQueryTimeConstraint, $AlphaQueryScanTimeout, $AlphaQueryPodTimeout, $AlphaQueryFormatTimeout},
	Method -> {
		"Server" -> $AlphaQueryBaseURL[],
		"Formats" -> $AlphaQueryFormats,
		"RawParameters" -> $AlphaQueryRawParameters
	}
}


alphaQueryInputsOptions[] := Sequence @@ {
		AppearanceElements -> {"UseInputLeft"},
		Asynchronous -> False,
		Method -> {
			"Server" -> $AlphaQueryBaseURL[],
			"Formats" -> {"minput"}
		}
	}


(* ::Subsection::Closed:: *)
(*WolframAlpha[]*)


(* ::Subsubsection::Closed:: *)
(*Options[WolframAlpha]*)


Options[WolframAlpha] = Options[WolframAlphaResult] = {
	AppearanceElements -> Automatic,
	Asynchronous -> False,
	ExcludePods -> None,
	IgnoreCase -> False,
	IncludePods -> All,
	InputAssumptions -> {},
	Method -> {},
	PodStates -> {},
	PodWidth -> Automatic,
	TimeConstraint -> 30
}


$WolframAlphaMethodOptions = {
	"AdditionalInformation" :> $AlphaQueryAdditionalInformation,
	"ButtonLabelFormats" -> Automatic,
	"CellOptions" -> {},
	"DelayedBoxes" -> False,
	"ExpandAsyncContent" -> Automatic,
	"ExpandImageURLs" -> Automatic,
	"ExpandRecalculateContent" -> Automatic,
	"ExtrusionChosen" -> Automatic,
	"ExtrusionOpen" -> False,
	"Formats" -> Automatic,
	"GeoLocation" -> Automatic,
	"Interactive" -> Automatic,
	"Language" -> Automatic,
	"NewQuery" -> None,
	"NotebookOptions" -> {},
	"OriginalURL" -> None,
	"RawParameters" -> {},
	"Reinterpret" -> Automatic,
	"SBSDetails" -> Automatic,
	"Server" -> Automatic,
	"SubstituteURL" -> None,
	"UserAgentString" -> Automatic,
	"UserIPString" -> Automatic,
	"WANParse" -> False
};


$WolframAlphaDefaultOptions = Options[WolframAlpha] /. (Method -> {}) -> (Sequence @@ $WolframAlphaMethodOptions);


(* 
The resolveOptions[] utility takes a sequence of option settings and returns the sequence
of all WolframAlpha[] options that are set to something other than their default value.
This is redundant for uses of WolframAlpha[] that go through "URL", "RawXML", or
"ProcessedXML", but these options are also used by the formatQueryResults[] utility that
takes the "ProcessedXML" result and decides how to format it.

Thus, we only use this utility when calling formatQueryResults[].
*)


resolveOptions[opts___] := 
Block[{mainOptions, methodOptions},

	mainOptions = First[Cases[Flatten[{opts, Options[WolframAlpha]}], _[#, _]]]& /@ 
		{AppearanceElements, Asynchronous, ExcludePods, IncludePods, InputAssumptions, PodStates, PodWidth, TimeConstraint};
	
	(* Don't include "CellOptions" or "NotebookOptions" here, since they're already being stored in the cell or notebook. *)
	methodOptions = First[Cases[allMethodOptions[opts], _[#, _]]]& /@ {
		"AdditionalInformation",
		"ExpandAsyncContent",
		"ExpandImageURLs",
		"ExpandRecalculateContent",
		"ExtrusionChosen",
		"ExtrusionOpen",
		"Formats",
		"NewQuery",
		"RawParameters",
		"SBSDetails",
		"Server",
		"UserAgentString",
		"UserIPString",
		"WANParse"
	};

	mainOptions = DeleteCases[mainOptions, Alternatives @@ $WolframAlphaDefaultOptions];
	
	methodOptions = DeleteCases[methodOptions, Alternatives @@ $WolframAlphaDefaultOptions];
	
	Sequence @@ Flatten[{mainOptions, If[methodOptions === {}, {}, Method -> methodOptions]}]
]


allMethodOptions[opts___] :=
	Cases[
		Flatten[{
			Cases[Flatten[{opts}], _[Method, a_] :> a],
			Cases[Options[WolframAlpha], _[Method, a_] :> a],
			$WolframAlphaMethodOptions
		}],
		_Rule | _RuleDelayed
	];



(* ::Subsubsection::Closed:: *)
(*No arguments*)


WolframAlpha[OptionsPattern[]] := Null /; Message[WolframAlpha::argt, WolframAlpha, 0, 1, 2]


(* ::Subsubsection::Closed:: *)
(*First argument contains only whitespace*)


WolframAlpha["", rest___] := (Message[WolframAlpha::blnulst, ToString["", InputForm], 1]; $Failed)


WolframAlpha[str_String, rest___] := (Message[WolframAlpha::blnulst, ToString[str, InputForm], 1]; $Failed) /; StringTrim[str] === ""


(* ::Subsubsection::Closed:: *)
(*Process a Typeset`Query in the first argument*)


(* Expand the one-arg form to the two-arg form *)
WolframAlpha[Typeset`Query[arg_], rest___] := 
	WolframAlpha[QueryExpand @ Typeset`Query[arg], rest]

(* If there is no actual typesetting in the query, revert to a plaintext query *)
WolframAlpha[Typeset`Query[_, "plaintext" -> str_String], rest___] := 
	WolframAlpha[str, rest]



(* ::Subsubsection::Closed:: *)
(*One-argument form*)


ValidQuery[input_String] := StringQ[input];
ValidQuery[Typeset`Query[arg_, "json" -> input_]] := StringQ[input];
ValidQuery[Typeset`Query[arg_, "json64" -> input_]] := StringQ[input];
ValidQuery[Typeset`Query[arg_, "plaintext" -> input_]] := StringQ[input];
ValidQuery[___] := False;

$DefaultWolframAlphaFormat := If[$Notebooks, "FullOutput", {All, {"Plaintext", "Input"}}]


WolframAlpha[str_?ValidQuery, opts:OptionsPattern[]] := WolframAlpha[str, $DefaultWolframAlphaFormat, opts]


(* ::Subsubsection::Closed:: *)
(*"FullResult"*)


WolframAlpha[str_?ValidQuery, "FullResult" | "FullOutput", opts:OptionsPattern[]] := 
	formatAlphaXML[
		WolframAlpha[str, "ProcessedXML", opts, AppearanceElements -> {"Warnings", "Assumptions", "Brand", "Pods", "PodMenus", "Unsuccessful", "Sources"}],
		str, opts, AppearanceElements -> {"Warnings", "Assumptions", "Brand", "Pods", "PodMenus", "Unsuccessful", "Sources"}
	]


(* ::Subsubsection::Closed:: *)
(*"FullInput"*)


WolframAlpha[str_String, "FullInput", opts:OptionsPattern[]] :=
	Block[{replaceEvaluationCell},
		replaceEvaluationCell[expr_] := CellPrint[queryBlobCell[expr]];
		AlphaIntegration`ExtrusionEvaluate[str, InputForm, opts]
	]


(* ::Subsubsection::Closed:: *)
(*"Result"*)


WolframAlpha[str_String, "Result", opts:OptionsPattern[]] := 
	Block[{replaceEvaluationCell, errorBlobQ},
		errorBlobQ[True, ___] := (Message[WolframAlpha::conopen]; False);
		AlphaIntegration`ExtrusionEvaluate[str, InputForm, opts]
	]


WolframAlpha[str_String, "HeldResult", opts:OptionsPattern[]] :=
	Block[{replaceEvaluationCell, errorBlobQ, suppressPlaceholderMInputs, minputResult, cell},
		replaceEvaluationCell[expr_] := (cell = queryBlobCell[expr]);
		errorBlobQ[True, ___] := (Message[WolframAlpha::conopen]; False);
		(*
			"HeldResult" returns even those expressions that contain Placeholders, even
			though they're not typically evaluated by AlphaIntegration`ExtrusionEvaluate.
		*)
		suppressPlaceholderMInputs[heldexpr_] := heldexpr;
		minputResult[string_] := HoldComplete @@ minputInput[string];

		AlphaIntegration`ExtrusionEvaluate[str, InputForm, opts];
		MakeExpression[First[cell], StandardForm]
	]


(* ::Subsubsection::Closed:: *)
(*"Boxes"*)


WolframAlpha[str_String, "Boxes", opts:OptionsPattern[]] := 
	Switch["DelayedBoxes" /. allMethodOptions[opts],
		True,
		DynamicBox[WolframAlpha[str, "Boxes", Method -> {"DelayedBoxes" -> False}, opts],
			System`DestroyAfterEvaluation -> True,
			SynchronousUpdating -> False,
			Evaluate[CachedValue -> ToBoxes[Column[{
				Style["Computing", FontSize -> 24, FontFamily -> "Times", FontColor -> GrayLevel[0.65]],
				Animator[Appearance -> "Necklace"]},
				Alignment -> Center,
				ItemSize -> Scaled[1],
				Spacings -> 1]]
			]
		],
		"PaneSelector",
		(* When saving to a file, we need to avoid using CachedValue. That's what this does. *)
		DynamicModule[{Typeset`done = False, Typeset`content = "None"},
			PaneSelector[{
				False ->
					Column[{
						Style["Computing", FontSize -> 24, FontFamily -> "Times", FontColor -> GrayLevel[0.65]],
						Animator[Appearance -> "Necklace"]},
						Alignment -> Center,
						ItemSize -> Scaled[1],
						Spacings -> 1
					],
				True -> Dynamic[RawBoxes[Typeset`content], System`DestroyAfterEvaluation -> True]
				},
				Dynamic[Typeset`done],
				ImageSize -> Automatic
			],
			Initialization :> (Typeset`content = WolframAlpha[str, "Boxes", Method -> {"DelayedBoxes" -> False}, opts]; Typeset`done = True),
			SynchronousInitialization -> False
		] // ToBoxes,
		(* else *)
		_,
		formatAlphaXMLWithGeneralization[str, opts] // ToBoxes
	]


formatAlphaXMLWithGeneralization[str_, opts___] :=
	Block[{xml, generalization, values, topic, desc, url, async, delayed},
		xml = WolframAlpha[str, "ProcessedXML", opts];		
		generalization = Replace[xml, {
			XMLObject["Document"][_, XMLElement["queryresult", _, {___, g:XMLElement["generalization", _, _], ___}], _] :> g,
			_ -> None}
		];
		
		If[generalization === None, Return @ formatAlphaXML[xml, str, opts]];

		values = Replace[generalization, {XMLElement["generalization", values_, data_] :> values, _ -> $Failed}];
		topic = "topic" /. Flatten[{values, "topic" -> "unknown"}];
		desc = "desc" /. Flatten[{values, "desc" -> "General results for:"}];
		url = "url" /. Flatten[{values, "url" -> $Failed}];
		If[url === $Failed, Return @ formatAlphaXML[xml, str, opts]];
		
		(*
		In the future, we might want to have ontological promotion handled in the main
		formatAlphaXML or formatQueryResults utilities. So as a bet against future compatibility, we
		remove the "generalization" element from the first round of xml results before formatting
		them.
		*)
		xml = Replace[xml, 
			XMLObject["Document"][a_, XMLElement["queryresult", values_, {x___, XMLElement["generalization", _, _], y___}], b_] :>
			XMLObject["Document"][a, XMLElement["queryresult", values, {x, y}], b]
		];
		
		(* delay the new output only if Asynchronous is set to All or a number *)
		async = Asynchronous /. Flatten[{opts, Options[WolframAlpha]}];		
		delayed = If[(async === All) || NumericQ[async], "PaneSelector", False];
		
		Column[{
			formatAlphaXML[xml, str, opts],
			innerFrame[
				Row[{desc, " ", Style[topic, Bold]}],
				Background -> RGBColor[1., 1., 0.92],
				BaseStyle -> "DialogStyle",
				FrontEnd`BoxFrame -> 2,
				FrameMargins -> 15,
				FrameStyle -> RGBColor[0.98, 0.52, 0.33],
				RoundingRadius -> 8
			],
			RawBoxes[WolframAlpha[topic, "Boxes", opts,  Method -> {"OriginalURL" -> url, "DelayedBoxes" -> delayed}]]
			},
			Spacings -> {1.5, 1.5}
		]
	]


(* ::Subsubsection::Closed:: *)
(*"Cell", "Notebook"*)


WolframAlpha[str_String, "Cell", opts:OptionsPattern[]] := 
	Block[{cellopts},
		cellopts = "CellOptions" /. allMethodOptions[opts];
		cellopts = Sequence @@ Flatten[{cellopts}];
		Cell[
			BoxData[WolframAlpha[str, "Boxes", opts]],
			"Output",
			cellopts,
			CellMargins -> {{20, 10}, {Inherited, Inherited}}
		]
	]


WolframAlpha[str_String, "Notebook", opts:OptionsPattern[]] :=
	Block[{nbopts},
		nbopts = "NotebookOptions" /. allMethodOptions[opts];
		nbopts = Sequence @@ Flatten[{nbopts}];
		Notebook[{
			WolframAlpha[str, "Cell", opts]
			},
			nbopts
		]
	]


WolframAlpha[str_String, {"Notebook", nbfile_}, opts:OptionsPattern[]] := 
	Put[WolframAlpha[str, "Notebook", opts], nbfile]


(* ::Subsubsection::Closed:: *)
(*"ServerNotebook"*)


$CDFNotebookOptions = Sequence[
	Editable -> False,
	Evaluatable -> False,
	Selectable -> False,
	ScrollingOptions -> {"VerticalScrollRange" -> Fit},
	ShowCellBracket -> False,
	ShowSelection -> False
]

$CDFCellOptions = Sequence[
	CellEditDuplicate -> False,
	CellMargins -> {{4, 4}, {4, 4}},
	Evaluatable -> False,
	Selectable -> True,
	ShowSelection -> True
]


WolframAlpha[str_String, "ServerNotebook", opts:OptionsPattern[]] := 
	Block[{nbopts},
		nbopts = "NotebookOptions" /. allMethodOptions[opts];
		nbopts = Sequence @@ Flatten[{nbopts}];
		Notebook[{
			WolframAlpha[str, "Cell", opts,
				AppearanceElements -> {"CDFWarnings", "CDFAssumptions", "CDFBrand", "ContentPadding", "Pods", "CDFPodMenus", "PodCornerCurl", "Unsuccessful", "Sources", "PluginAndPacletCheck"},
				Asynchronous -> All,
				Method -> {"CellOptions" -> {$CDFCellOptions}, "Formats" -> {"cell", "msound", "dataformats", "imagemap"}}
			]},
			nbopts,
			$CDFNotebookOptions
		]
	]


WolframAlpha[str_String, {"ServerNotebook", nbfile_}, opts:OptionsPattern[]] := 
	WolframAlpha[str, {"ServerNotebook", nbfile, False}, opts]	


WolframAlpha[str_String, {"ServerNotebook", nbfile_, cacheQ:False}, opts:OptionsPattern[]] := 
	Put[WolframAlpha[str, "ServerNotebook", opts], nbfile]


WolframAlpha[str_String, {"ServerNotebook", nbfile_, cacheQ:True}, opts:OptionsPattern[]] := 
	exportNotebook[nbfile, WolframAlpha[str, "ServerNotebook", opts]]


WolframAlpha[str_String, {"ServerNotebook", nbfile_, context_String}, opts:OptionsPattern[]] := 
	exportNotebook[nbfile, WolframAlpha[str, "ServerNotebook", opts], context]


WolframAlpha[str_String, {"ServerNotebook", nbfile_, cacheQ_, pathToSigningBinary_}, opts:OptionsPattern[]] := 
	Block[{link},
		WolframAlpha[str, {"ServerNotebook", nbfile, cacheQ}, opts];
		link = Install[ pathToSigningBinary ];
		ToExpression["NotebookSign`PlayerProSecure"][nbfile];
		Uninstall[link]
	]



exportNotebook[args__] :=
	Block[{dynamicupdating},
		UsingFrontEnd[
			dynamicupdating = CurrentValue[$FrontEndSession, DynamicUpdating];
			CurrentValue[$FrontEndSession, DynamicUpdating] = False;
			doExportNotebook[args];
			CurrentValue[$FrontEndSession, DynamicUpdating] = dynamicupdating;
		]
	];


doExportNotebook[nbfile_, nbexpr_] := Export[nbfile, nbexpr]

doExportNotebook[nbfile_, nbexpr_, context_] :=
	If[TrueQ[$LocalServerWithContext] || useWithContext[],
		Block[{result, stm},
			result = Block[
				{$Context = "hiddenContext`", $ContextPath = {"hiddenContext`", "System`"}},
				MathLink`CallFrontEnd[FrontEnd`ExportPacket[FEPrivate`WithContext[context, nbexpr], "NotebookString"]]
			];
			If[MatchQ[result, {_String, ___}],
				stm = OpenWrite[nbfile];
				WriteString[stm, First[result]];
				Close[stm];
				nbfile,
				(* else *)
				$Failed
			]
		],
		Export[nbfile, nbexpr]
	]



useWithContext[] := useWithContext[] =
Block[{version, release, minorrelease},
	version = MathLink`CallFrontEnd[FrontEnd`Value["$NotebookVersionNumber"]];
	release = MathLink`CallFrontEnd[FrontEnd`Value["$NotebookReleaseNumber"]];
	minorrelease = MathLink`CallFrontEnd[FrontEnd`Value["$NotebookMinorReleaseNumber"]];
	(* The required FEPrivate`WithContext behavior was added in version 8.0.1.1 *)
	Or[
		version > 8.0,
		version === 8.0 && release > 1,
		version === 8.0 && release === 1 && minorrelease > 0
	] // TrueQ
]



(* ::Subsubsection::Closed:: *)
(*"FormattedPods"*)


WolframAlpha[str_String, "FormattedElements", opts:OptionsPattern[]] :=
	Block[{xml, pods, nonpods, elements, style},
		xml = WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"cell"}, "ExpandRecalculateContent" -> True, "ExpandAsyncContent" -> True}];
		If[!MatchQ[xml, XMLObject["Document"][_, XMLElement["queryresult", _, _], _]], Return[$Failed]];
		
		pods = Cases[xml[[2,3]], XMLElement["pod", _, _]];
		nonpods = DeleteCases[xml[[2,3]], XMLElement["pod", _, _]];
		
		elements = OptionValue[AppearanceElements];
		Switch[elements,
			None, elements = {},
			{__String}, Null,
			All, elements = $AllAlphaQueryAppearanceElements,
			Automatic | _, elements = $AutomaticAlphaQueryAppearanceElements
		];
		
		style[list_List] := style /@ list;
		style[expr_] := Style[expr, "DialogStyle", AutoItalicWords -> {"Mathematica"}, Enabled -> False];
		
		Flatten[{
			If[!MemberQ[elements, "Warnings"], {}, "Warnings" -> style[FormatIndependentWarnings[nonpods, str, opts]]],
			If[!MemberQ[elements, "Assumptions"], {}, "Assumptions" -> style[FormatIndependentAssumptions[nonpods, str, opts]]],
			If[!MemberQ[elements, "Pods"], {}, "Pods" -> ( ("id" /. Part[#, 2]) -> style[FormatIndependentPod[#, str, opts]]& /@ pods )],
			If[!MemberQ[elements, "Pods"], {}, "PodStates" -> getPodAndSubpodStates[pods]],
			If[!MemberQ[elements, "Unsuccessful"], {}, "Unsuccessful" -> If[pods =!= {}, {}, style[FormatIndependentUnsuccesses[nonpods, str, opts]]]]
		}]
	]


getPodAndSubpodStates[pods_] := Flatten[{getPodStates[pods], getSubpodStates[pods]}]

getPodStates[pods_List] := 
	Cases[pods, XMLElement["pod", {___, "id" -> id_, ___}, {___, states:XMLElement["states", _, _], ___}] :> (id -> states)]

getSubpodStates[pods_List] := DeleteCases[Flatten[getSubpodStates /@ pods], _ -> {}]

getSubpodStates[XMLElement["pod", {___, "id" -> id_, ___}, data_]] :=
	MapIndexed[{id -> First[#2]} -> # &, Cases[data, subpod:XMLElement["subpod", _, _] :> getSubpodStates[subpod]]];

getSubpodStates[XMLElement["subpod", _, {___, states:XMLElement["states", _, _], ___}]] := states

getSubpodStates[other_] := {}



(* FormatIndependentWarnings[] needs to reach deeper than FormatAllWarnings[] does to get the warnings separately *) 
FormatIndependentWarnings[nonpods_, query_, opts___] :=
	DeleteCases[
		Map[
			DynamicModule[{Typeset`q = query, Typeset`newq = None},
				formatWarning[#, "Warnings", Dynamic[Typeset`newq], Dynamic[Typeset`q], opts],
				Initialization :> Quiet[WolframAlpha[]] (* trigger autoloading *)
			]&,
			Flatten[Cases[nonpods, XMLElement["warnings", values_, data_] :> data]]
		],
		_[_, {}]
	]


FormatIndependentUnsuccesses[nonpods_, query_, opts___] := {
	(* there's always exactly one of these, which is disincluded if there are any pods *)
	DynamicModule[{Typeset`q = query, Typeset`newq = None, Typeset`opts = {opts}},
		Dynamic[First[FormatAllUnsuccesses[nonpods, Dynamic[Typeset`newq], Dynamic[Typeset`q], Dynamic[Typeset`opts]]]],
		Initialization :> Quiet[WolframAlpha[]] (* trigger autoloading *)
	]
}


(* FormatIndependentAssumptions[] is based closely on FormatAllAssumptions[] *)
FormatIndependentAssumptions[nonpods_, query_, opts___] :=
Block[{assumptions, nonformula, formula},
	assumptions = Cases[nonpods, XMLElement["assumptions", values_, list_] :> list] // Flatten;
	formula = normalizeAssumption /@ Cases[assumptions,
		XMLElement["assumption", {___, "type" -> (Alternatives @@ $FormulaAssumptionTypes), ___}, _] ];
	nonformula = normalizeAssumption /@ DeleteCases[assumptions,
		XMLElement["assumption", {___, "type" -> (Alternatives @@ $FormulaAssumptionTypes), ___}, _] ];
	
	With[{nonformula = nonformula, formula = formula},
		Flatten[{
			If[nonformula === {}, {},
				DynamicModule[{Typeset`q = query, Typeset`newq = None, Typeset`opts = {opts}},
					Dynamic[formatNonFormulaAssumptions[nonformula, "Assumptions", Dynamic[Typeset`newq], Dynamic[Typeset`q], Dynamic[Typeset`opts]]],
					Initialization :> Quiet[WolframAlpha[]] (* trigger autoloading *)
				]
			],
			If[formula === {}, {},
				DynamicModule[{Typeset`q = query, Typeset`newq = None, Typeset`opts = {opts}},
					Dynamic[formatFormulaAssumptions[formula, "Assumptions", Dynamic[Typeset`newq], Dynamic[Typeset`q], Dynamic[Typeset`opts]]],
					Initialization :> Quiet[WolframAlpha[]] (* trigger autoloading *)
				]
			]
		}]
	]
]







(* ::Subsubsection::Closed:: *)
(*"PodCells"*)


WolframAlpha[str_String, "PodCells", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"cell"}}],
		XMLElement["subpod", _, {___, XMLElement["cell", _, {cell_}], ___}] :> prepareSubpodCell[cell, Automatic, Identity],
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodContents"*)


WolframAlpha[str_String, "PodContents", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"cell"}}],
		XMLElement["subpod", _, {___, XMLElement["cell", _, {cell_}], ___}] :> prepareSubpodCellContent[cell, Automatic, Identity],
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodImages"*)


WolframAlpha[str_String, "PodImages", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"image"}}],
		XMLElement["subpod", _, {___, XMLElement["img", {___, "ImportedData" -> img_,___}, _], ___}] :> img,
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodPlaintext"*)


WolframAlpha[str_String, "PodPlaintext", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"plaintext"}}],
		XMLElement["subpod", _, {___, XMLElement["plaintext", _, {text_}], ___}] :> text,
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodTitles"*)


WolframAlpha[str_String, "PodTitles", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"plaintext"}}],
		XMLElement["pod", {___, "title" -> title_, ___}, _] :> title,
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodIDs"*)


WolframAlpha[str_String, "PodIDs", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"plaintext"}}],
		XMLElement["pod", {___, "id" -> id_, ___}, _] :> id,
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodStates"*)


WolframAlpha[str_String, "PodStates", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"plaintext"}}],
		XMLElement["pod", {___, "id" -> id_, ___}, {___, XMLElement["states", _, states_], ___}] :>
			(id -> Flatten[Map[getStates, states]]),
		Infinity
	]


getStates[XMLElement["state", {___, "input" -> input_, ___}, _]] := input

getStates[XMLElement["statelist", _, states_]] := Map[getStates, states]

getStates[other_] := {}



(* ::Subsubsection::Closed:: *)
(*"PodInformation"*)


WolframAlpha[str_?ValidQuery, "PodInformation" | "PropertyRules", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "ProcessedXML", opts] // xmlDocumentData,
		pod: XMLElement["pod", {___, "id" -> id_, ___}, _] :> podXMLToPodProperties[id, pod]
	] // Flatten;



xmlDocumentValues[XMLObject["Document"][_, XMLElement["queryresult", values_, data_], _]] := values
xmlDocumentValues[_] := {}

xmlDocumentData[XMLObject["Document"][_, XMLElement["queryresult", values_, data_], _]] := data
xmlDocumentData[_] := {}


podXMLToPodProperties[id_, XMLElement["pod", values_, data_]] := Flatten[{
	{{id, 0}, "Title"} -> getValue["title", values],
	{{id, 0}, "Scanner"} -> getValue["scanner", values],
	{{id, 0}, "ID"} -> getValue["id", values],
	{{id, 0}, "Position"} -> getValue["position", values],
	(* {{id, 0}, "PodStates"} -> getData["states", data], *)
	(* {{id, 0}, "PodInfos"} -> getData["infos", data], *)
	(* {{id, 0}, "HTML"} -> getData["markup", data], *)
	(* {{id, 0}, "XML"} -> XML["pod", values, data], *)
	MapIndexed[subpodXMLToPodProperties[id, First[#2], #1]&, Cases[data, subpod:XMLElement["subpod", _, _]]]
}] // DeleteCases[#, _ -> (_Missing | "")]&


subpodXMLToPodProperties[id_, c_, xml:XMLElement["subpod", values_, data_]] := {
	(* {{id, c}, "XML"} -> xml, *)
	{{id, c}, "Title"} -> getValue["title", values],
	{{id, c}, "Plaintext"} -> getData["plaintext", data],
	{{id, c}, "Input"} -> getData["minput", data],
	{{id, c}, "Output"} -> getData["moutput", data],
	{{id, c}, "MathML"} -> getData["mathml", data],
	{{id, c}, "Cell"} -> getData["cell", data],
	{{id, c}, "Content"} -> getData["cellcontent", data],
	{{id, c}, "Image"} -> getData["img", data],
	{{id, c}, "Sound"} -> getData["sound", data],
	{{id, c}, "MSound"} -> getData["msound", data],
	{{id, c}, "DataFormats"} -> getData["dataformats", data],
	{{id, c}, "ComputableData"} -> getData["computabledata", data],
	{{id, c}, "FormattedData"} -> getData["formatteddata", data],
	{{id, c}, "FormulaData"} -> getData["formuladata", data],
	{{id, c}, "NumberData"} -> getData["numberdata", data],
	{{id, c}, "QuantityData"} -> getData["quantitydata", data],
	{{id, c}, "SoundData"} -> getData["sounddata", data],
	{{id, c}, "TimeSeriesData"} -> getData["timeseriesdata", data]
}


getValue[name_, values_] := name /. values /. name -> Missing["NotAvailable"]


getData["cell", {___, XMLElement["cell", _, {cell_}], ___}] := prepareSubpodCell[cell, Automatic, Identity]
getData["cellcontent", {___, XMLElement["cell", _, {cell_}], ___}] := prepareSubpodCellContent[cell, Automatic, Identity]
getData["minput", {___, XMLElement["minput", _, {minput_String}], ___}] := ToExpression[minput, InputForm, HoldComplete]
getData["moutput", {___, XMLElement["moutput", _, {moutput_String}], ___}] := ToExpression[moutput, InputForm, HoldComplete]
getData["mathml", {___, XMLElement["mathml", _, {mathml_}], ___}] := mathml
getData["img", {___, XMLElement["img", {___, "ImportedData" -> img_, ___}, _], ___}] := img
getData["img", {___, XMLElement["img", {___, "src" -> src_, ___}, _], ___}] := src
getData["msound", {___, XMLElement["sound", _, {sound_String}], ___}] := ToExpression[sound]
getData["sound", {___, XMLElement["sound", {___, "ImportedData" -> sound_, ___}, _], ___}] := sound
getData["sound", {___, XMLElement["sound", {___, "src" -> src_, ___}, _], ___}] := src
getData["dataformats", {___, XMLElement["dataformats", _, {data_String}], ___}] := Join[parseTextFormatsString[data], parseDataFormatsString[data]]
getData["dataformats", {___, XMLElement["dataformats", _, {}], ___}] := {}
getData["computabledata", {___, XMLElement["computabledata", {}, {data_String}], ___}] := ToExpression[data]
getData["formatteddata", {___, XMLElement["formatteddata", {}, {data_String}], ___}] := ToExpression[data]
getData["formuladata", {___, XMLElement["formuladata", {}, {data_String}], ___}] := ToExpression[data]
getData["numberdata", {___, XMLElement["numberdata", {}, {data_String}], ___}] := ToExpression[data]
getData["quantitydata", {___, XMLElement["quantitydata", {}, {data_String}], ___}] := ToExpression[data]
getData["sounddata", {___, XMLElement["sounddata", {}, {data_String}], ___}] := ToExpression[data]
getData["timeseriesdata", {___, XMLElement["timeseriesdata", {}, {data_String}], ___}] := ToExpression[data]
getData[name_, {___, XMLElement[name_, values_, {data_}], ___}] := data
getData[name_, data_] := Missing["NotAvailable"]


parseDataFormatsString[str_] :=
	DeleteDuplicates[Flatten[StringSplit[str, ","]]] /.
		Flatten[{
			Thread[ToLowerCase[$AlphaQueryDataFormats] -> $AlphaQueryDataFormats],
			_String :> Unevaluated[Sequence[]]
		}]

parseTextFormatsString[str_] := 
	DeleteDuplicates[Flatten[StringSplit[str, ","]]] /.
		Flatten[{
			Thread[ToLowerCase[$AlphaQueryTextFormats] -> $AlphaQueryTextFormats],
			"minput" -> "Input",
			"moutput" -> "Output",
			_String :> Unevaluated[Sequence[]]
		}]


(* ::Subsubsection::Closed:: *)
(*"PodProperties"*)


WolframAlpha[str_?ValidQuery, "PodProperties" | "Properties", opts:OptionsPattern[]] :=
	First /@ WolframAlpha[str, "PodInformation", opts]


(* ::Subsubsection::Closed:: *)
(*"DataRules"*)


WolframAlpha[str_String, "DataRules", opts:OptionsPattern[]] := 
	WolframAlpha[str, {All, $AlphaQueryDataFormats}, opts]


WolframAlpha[str_String, "DataFormats", opts:OptionsPattern[]] :=
	WolframAlpha[str, {All, "DataFormats"}, opts]


WolframAlpha[str_String, "ComputableData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "computabledata"}],
		XMLElement["computabledata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "FormattedData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "formatteddata"}],
		XMLElement["formatteddata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "FormulaData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "formuladata"}],
		XMLElement["formuladata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "NumberData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "numberdata"}],
		XMLElement["numberdata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "QuantityData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "quantitydata"}],
		XMLElement["quantitydata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "SoundData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "sounddata"}],
		XMLElement["sounddata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "TimeSeriesData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "timeseriesdata"}],
		XMLElement["timeseriesdata", _, {data_}] :> ToExpression[data],
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"InputAssumptions"*)


WolframAlpha[str_String, "InputAssumptions" | "Assumptions", opts:OptionsPattern[]] := 
	Block[{assumptions},
		assumptions =
			Cases[
				WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"plaintext"}}],
				XMLElement["assumptions", _, list_] :> list,
				Infinity
			];
		Map[getAssumptions, Flatten[assumptions]]
	]


getAssumptions[XMLElement["value", {___, "input" -> input_, ___}, _]] := input

getAssumptions[XMLElement["assumption", {___, "type" -> type_, ___}, values_]] := Map[getAssumptions, values]

getAssumptions[other_] := {}


(* ::Subsubsection::Closed:: *)
(*"Sources"*)


WolframAlpha[str_String, "Sources", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"plaintext"}}],
		XMLElement["sources", _, sources_List] :> Map[getSource, sources],
		Infinity
	] // Flatten


getSource[XMLElement["source", {___, "url" -> url_String, ___, "text" -> text_, ___}, _]] := text -> url
getSource[XMLElement["source", {___, "text" -> text_, ___, "url" -> url_String, ___}, _]] := text -> url

getSource[other_] := {}


(* ::Subsubsection::Closed:: *)
(*"RelatedQueries"*)


WolframAlpha[str_String, "RelatedQueries", opts:OptionsPattern[]] :=
	Block[{url, related},
		url = Replace[WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"plaintext"}}],
			{
				XMLObject["Document"][_, XMLElement["queryresult", {___, "related" -> url_, ___}, _], _] :> url,
				other_ -> None
			}
		];
		If[!StringQ[url], Return[{}]];
		Cases[qImport[url, "XML"], XMLElement["relatedquery", _, {relatedQuery_String}] :> relatedQuery, Infinity]
	]


(* ::Subsubsection::Closed:: *)
(*"WolframResult", "MathematicaResult"*)


WolframAlpha[str_String, "MathematicaResult" | "WolframResult", opts:OptionsPattern[]] := 
	ReleaseHold[ WolframAlpha[str, "HeldWolframResult", opts] ]


WolframAlpha[str_String, "HeldMathematicaResult" | "HeldWolframResult", opts:OptionsPattern[]] := 
	Block[{result},
		result = Cases[
			WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"mresult"}}],
			XMLElement["mresult", _, {mresult_String}] :> ToExpression[mresult, InputForm, HoldComplete],
			Infinity
		];
		result = DeleteCases[result, HoldComplete[None]];
		If[result === {}, Missing["NoResult"], First[result]]
	]


(* ::Subsubsection::Closed:: *)
(*"WolframForms", "MathematicaForms"*)


WolframAlpha[str_String, "MathematicaForms" | "WolframForms", opts:OptionsPattern[]] := 
	ToExpression[#, InputForm, HoldComplete]& /@ WolframAlpha[str, "WolframStrings", opts]


WolframAlpha[str_String, "MathematicaStrings" | "WolframStrings", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"minput", "moutput"}}],
		XMLElement["minput" | "moutput", _, {mform_String}] :> mform,
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PrimaryWolframForm", "PrimaryMathematicaForm"*)


WolframAlpha[str_String, "PrimaryMathematicaForm" | "PrimaryWolframForm", opts:OptionsPattern[]] :=
	If[# === $Failed, $Failed, ToExpression[#, InputForm, HoldComplete]]& @
		WolframAlpha[str, "PrimaryWolframString", opts]


WolframAlpha[str_String, "PrimaryMathematicaString" | "PrimaryWolframString", opts:OptionsPattern[]] :=
	Block[{xml, result},
		xml = WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"minput", "moutput"}}];
		
		(* if there's a fatal error when called from within control-equal, complain *)
		If[$AlphaQueryMMode === "inline" && errorBlobQ[False, xml, str, opts],
			CellPrint[formatErrorBlob[False, xml, str, opts]]
		];
		
		(* If there is a primary result, use its minput or moutput, in that order *)
		result = Flatten[Cases[xml, XMLElement["pod", {___, "primary" -> "true", ___}, data_] :> extractMathematicaForms[data], Infinity]];
		If[result =!= {}, Return[First[result]]];
		(* If there's no primary result, use the first pod that has an minput or moutput, in pod order *)
		result = Flatten[Cases[xml, XMLElement["pod", _, data_] :> extractMathematicaForms[data], Infinity]];
		If[result =!= {}, Return[First[result]]];
		(* Otherwise, return $Failed *)
		$Failed
	]


extractMathematicaForms[data_] := Flatten[{
	Cases[data, XMLElement["minput", _, {minput_String}] :> minput, Infinity],
	Cases[data, XMLElement["moutput", _, {moutput_String}] :> moutput, Infinity]}]




(* ::Subsubsection::Closed:: *)
(*"SessionInfo"*)


WolframAlpha[str_String, "SessionInfo", opts:OptionsPattern[]] :=
	mathematicaSessionInfo[str]


(* ::Subsubsection::Closed:: *)
(*"XML", "RawXML", "ProcessedXML"*)


WolframAlpha[str_?ValidQuery, "XML", opts:OptionsPattern[]] := WolframAlpha[str, "RawXML", opts]


WolframAlpha[str_?ValidQuery, "RawXML", ___, Method -> {___, "RawXML" -> xml_, ___}, ___] := xml


WolframAlpha[str_?ValidQuery, "ProcessedXML", ___, Method -> {___, "ProcessedXML" -> xml_, ___}, ___] := xml


WolframAlpha[str_?ValidQuery, type:( "RawXML" | "ProcessedXML" | "WolframAlphaNotebookParseRawXML" | "WolframAlphaNotebookParseXML"), opts:OptionsPattern[]] := 
	Block[{url, xml, recalculate, expandrecalculate, async, expandasync, expandimage, additionalinformation, timecontraint},
		timeconstraint = First[N[Flatten[{ OptionValue[TimeConstraint] }]]];
		If[!TrueQ[timeconstraint > 0], timeconstraint = TimeConstraint /. $WolframAlphaDefaultOptions];
		TimeConstrained[
			(
				url = If[
					MatchQ[type, "WolframAlphaNotebookParseRawXML" | "WolframAlphaNotebookParseXML"],
					WolframAlpha[str, "WolframAlphaNotebookParseURL", opts],
					WolframAlpha[str, "URL", opts]
				];
				xml = qImport[url, "XML"];
				
				async = OptionValue[Asynchronous];
				{additionalinformation, expandrecalculate, expandasync, expandimage} =
					{"AdditionalInformation", "ExpandRecalculateContent", "ExpandAsyncContent", "ExpandImageURLs"} /. allMethodOptions[opts];

				If[additionalinformation,
					xml = Replace[xml,
						XMLObject["Document"][a_, XMLElement["queryresult", {values__}, data_], b_] :>
						XMLObject["Document"][a, XMLElement["queryresult", {values, "queryurl" -> url}, data], b]
					]
				];
				
				(*
				Interpret "ExpandRecalculateContent" -> Automatic, "ExpandAsyncContent" ->
				Automatic, and "ExpandImageURLs" -> Automatic to mean always do the expansion,
				unless Asynchronous -> All or Asynchronous -> number. That gives us a simple switch
				to throw to get un-expanded results.
				*)

				If[expandrecalculate === True ||
					( expandrecalculate === Automatic && $AlphaQuerySimpleRecalculate[] && (async =!= All && !NumericQ[async]) ),
					recalculate = DeleteCases[Cases[xml, XMLElement["queryresult", {___, "recalculate" -> url_, ___}, _] :> url, Infinity], ""];
					If[Length[recalculate] > 0,
						recalculate = Cases[qImport[extendedParams @@ splitURL[First[recalculate]], "XML"], XMLElement["pod", _, _], Infinity];
						xml = Replace[xml,
							XMLObject["Document"][a_, XMLElement["queryresult", values_, data_List], b_] :>
							XMLObject["Document"][a, XMLElement["queryresult", addRecalculateValues[values], addRecalculateData[data, recalculate]], b]
						]
					]
				];
		
				If[ expandasync === True || ( expandasync === Automatic && (async =!= All && !NumericQ[async]) ),
					xml = xml /. XMLElement["pod", {___, "async" -> url_, ___}, _] :> expandAsyncPod[url, False]
				];
				If[MatchQ[type, "RawXML" | "WolframAlphaNotebookParseRawXML"],
					(* For raw forms, don't expand images or cells *)
					xml,
					(* For "ProcessedXML", only expand images when the settings call for it *)
					If[ expandimage === True || ( expandimage === Automatic && (async =!= All && !NumericQ[async]) ),
						xml = expandData[xml],
						(* even when not expanding other data, always expand the cells *)
						xml = expandCells[xml]
					];
					xml
				]
			),
			timeconstraint,
			Message[WolframAlpha::timeout, HoldForm[WolframAlpha[str]], timeconstraint];
			$TimedOut
		]
			
	] // dontRefresh


expandAsyncPod[url_String, expandDataQ_] := 
	Block[{pods},
		pods = Cases[qImport[url, "XML"], XMLElement["pod", values_, data_] :> XMLElement["pod", SortBy[values, First], data], Infinity];
		If[pods === {}, Unevaluated[Sequence[]], If[expandDataQ, expandData[First[pods]], First[pods]]]
	]



addRecalculateValues[values_] := values /. ("recalculate" -> url_) :> ("recalculated" -> url)


addRecalculateData[olddata_, {}] := olddata

addRecalculateData[olddata_, newdata_] := 
Block[{oldpods, nonpods, newpods, joinedpods},
	oldpods = Cases[olddata, XMLElement["pod", _, _]];
	nonpods = DeleteCases[olddata, XMLElement["pod", _, _]];
	newpods = Cases[newdata, XMLElement["pod", _, _]];

	joinedpods = {If[DigitQ[#], ToExpression[#], #]&["position" /. #[[2]]], #}& /@ Join[newpods, oldpods];
	Join[#[[1,2]]& /@ SplitBy[SortBy[joinedpods, {First}], First], nonpods]
]


addRecalculateQueryInfo[oldinfo_, newinfo_] := {oldinfo, newinfo}

addRecalculateNonPods[olddata_, newdata_] := Flatten[{olddata, newdata}]


(* ::Subsubsection::Closed:: *)
(*"URL"*)


WolframAlpha[str_?ValidQuery, "URL", opts:OptionsPattern[]] := 
	Block[{baseurl, wanparseQ, baseparams, input, i2d, async, format, assumption, includepodid, excludepodid,
			podstate, podwidth, ignorecase, timeouts, msessinfo, latlong, reinterpret, interactive, sbsdetails,
			dpparse, languagecode, userip, rawparameters, params, methodOptions, buttonlabelformats},

		methodOptions = allMethodOptions[opts];
		
		If[MatchQ["SubstituteURL" /. methodOptions, _String | File[_String]],
			Return["SubstituteURL" /. methodOptions]];
		
		wanparseQ = TrueQ["WANParse" /. methodOptions];
		
		baseurl = "OriginalURL" /. methodOptions;
		If[StringQ[baseurl],
			{baseurl, baseparams} = splitURL[baseurl],
			(* else *)			
			baseurl = "Server" /. methodOptions;
			baseurl = $AlphaQueryBaseURL[baseurl] <> If[wanparseQ, $AlphaNotebookParseJSP, $AlphaQueryJSP];
			baseparams = {};
		];
		
		Replace[str, {
			a_String :> (input = a; i2d = Nothing),
			Typeset`Query[_, "json" -> a_String] :> Replace[queryJSONToQueryList[a], {
				{{"t"->0,"v"->plaintext_String}} :> (input = plaintext; i2d = Nothing),
				_ :> (input = queryJSONToQueryJSON64[a]; i2d = "i2d" -> "true")
			}],
			Typeset`Query[_, "json64" -> a_String] :> (input = a; i2d = "i2d" -> "true"),
			Typeset`Query[_, "plaintext" -> a_String] :> (input = a; i2d = Nothing)
		}];
		
		async = OptionValue[Asynchronous];
		async = Switch[async,
			_Integer, "async" -> ToString[async, InputForm],
			_?NumericQ, "async" -> ToString[Re[N[async]], InputForm],
			False, "async" -> "false",
			True | All | _, "async" -> "true"
		];
		
		assumption = OptionValue[InputAssumptions];
		assumption = Switch[assumption,
			_String, {assumption},
			{__String}, assumption,
			_, {}
		] // preprocessAssumptionsParameters;
		
		includepodid = OptionValue[IncludePods];
		includepodid = Switch[includepodid,
			_String, "includepodid" -> urlencode[includepodid],
			{__String}, Map["includepodid" -> urlencode[#] &, includepodid],
			_, {}
		];
		
		excludepodid = OptionValue[ExcludePods];
		excludepodid = Switch[excludepodid,
			_String, "excludepodid" -> urlencode[excludepodid],
			{__String}, Map["excludepodid" -> urlencode[#] &, excludepodid],
			_, {}
		];
		
		If[includepodid =!= {} && excludepodid =!= {},
			Message[WolframAlpha::notboth, includepodid, excludepodid];
			excludepodid = {}
		];
		
		podstate = OptionValue[PodStates];
		podstate = Switch[podstate,
			_String, "podstate" -> urlencode[podstate],
			{__String}, "podstate" -> StringJoin[Riffle[urlencode /@ podstate, ","]],
			_, {}
		];
		
		podwidth = Flatten[{ OptionValue[PodWidth] }];
		podwidth = MapThread[If[NumericQ[#], #, #2]&,
			{ PadRight[podwidth, 4, Automatic], PadRight[Flatten[{$AlphaQueryPodWidth}], 4, Automatic] }
		];
		podwidth = Flatten[MapThread[
			Which[
				IntegerQ[#], #2 -> ToString[#, InputForm],
				NumericQ[#], #2 -> ToString[Re[N[#]], InputForm],
				True, {}
			]&,
			{ podwidth, {"width", "maxwidth", "plotwidth", "infowidth"} }
		]];
		
		format = "Formats" /. methodOptions;
		If[format === Automatic, format = {"cell", "minput", "msound", "dataformats"}];
		format = Switch[format,
			_String, "format" -> format,
			{__String}, "format" -> StringJoin[Riffle[format, ","]],
			_, {}
		];
		
		buttonlabelformats = "ButtonLabelFormats" /. methodOptions;
		buttonlabelformats = Switch[buttonlabelformats,
			Automatic, "buttonlabelformats" -> "MathML",
			_String, "buttonlabelformats" ->  buttonlabelformats,
			_, {}
		];
		
		reinterpret = "Reinterpret" /. methodOptions;
		If[reinterpret === Automatic, reinterpret = $AlphaQueryReinterpret[]];
		reinterpret = Switch[reinterpret,
			True, "reinterpret" -> "true",
			False, "reinterpret" -> "false",
			_, {}
		];
		
		interactive = "Interactive" /. methodOptions;
		If[interactive === Automatic, interactive = $AlphaQueryInteractive[]];
		interactive = Switch[interactive,
			True, "interactive" -> "true",
			False, "interactive" -> "false",
			_, {}
		];
		
		sbsdetails = "SBSDetails" /. methodOptions;
		If[sbsdetails === Automatic, sbsdetails = wanparseQ];
		sbsdetails = Switch[sbsdetails,
			True, "sbsdetails" -> "true",
			False, "sbsdetails" -> "false",
			_, {}
		];
		
		dpparse = Switch[
			"DataPacletParse" /. ("DataOptions" /. SystemOptions[]),
			True, "dpparse" -> "true",
			False, "dpparse" -> "false",
			_, {}
		];
		
		languagecode = "Language" /. methodOptions;
		languagecode = $AlphaQueryLanguageCode[languagecode];
		languagecode = Switch[languagecode,
			"en", {},
			_String, "languagecode" -> languagecode,
			_, {}
		];
		
		ignorecase = OptionValue[IgnoreCase];
		ignorecase = Switch[ignorecase,
			True, "ignorecase" -> "true",
			_, {}
		];
		
		timeouts = Flatten[{ OptionValue[TimeConstraint] }];
		timeouts =
			Which[
				IntegerQ[#], ToString[#, InputForm],
				NumericQ[#], ToString[Re[N[#]], InputForm],
				True, Automatic
			]& /@ PadRight[timeouts, 4, Automatic];
		timeouts = Cases[Thread[{Automatic, "scantimeout", "podtimeout", "formattimeout"} -> timeouts], _[_String, _String]];
		
		msessinfo = getMathematicaSessionInfo[
			If[StringQ[str], str, StringRiffle @ str["allUserStrings"]], True, False];
		msessinfo = Switch[msessinfo,
			{__Rule}, "msessinfo" -> urlencode @ ToString[msessinfo, InputForm],
			_, {}
		];
		
		latlong = Replace["GeoLocation" /. methodOptions, Automatic :> $AlphaQueryGeoLocation];
		latlong = Switch[latlong, 
			_GeoPosition, "latlong" -> urlencode @ iGeoLocationtoLatLong @ latlong,
			_String, "latlong" -> urlencode @ latlong,
			_, {}
		];
		
		userip = "UserIPString" /. methodOptions;
		userip = Switch[userip,
			_String, "ip" -> urlencode[userip],
			Automatic,
				(* include the user's $RequesterAddress *)
				Replace[$RequesterAddress, {ip_String :> ("ip" -> urlencode[ip]), _ :> {}}],
			_, {}
		];
		
		rawparameters = rawParametersToRules["RawParameters" /. methodOptions];
		
		params = Flatten[{
			baseparams,
			"input" -> urlencode[
				If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> input
            ],
			i2d,
			ignorecase,
			async,
			timeouts,
			format,
			buttonlabelformats,
			assumption,
			includepodid,
			excludepodid,
			podstate,
			reinterpret,
			podwidth,
			interactive,
			sbsdetails,
			dpparse,
			"releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]],
			"patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]],
			"systemid" -> urlencode[$SystemID],
			"mclient" -> $AlphaQueryMClient,
			"mmode" -> $AlphaQueryMMode,
			msessinfo,
			latlong,
			languagecode,
			userip,
			rawparameters
		}];
		
		extendedParams[baseurl, params]
	]


preprocessAssumptionsParameters[inputs: {___String}] := Which[
	(* if there are no assumptions present, set the default assumptionsversion. *)
	inputs === {},
		Replace[$AlphaQueryDefaultAssumptionsVersion, {
			v_String :> {"assumptionsversion" -> v},
			_ :> {}
		}],
	(* if there are assumptions present, set the matching assumptionsversion *)
	AnyTrue[inputs, StringContainsQ[" -> "]],
		Append[
			Map["assumption" -> urlencode[#]&, inputs],
			"assumptionsversion" -> "2"
		],
	True,
		Map["assumption" -> #&, inputs]
]


rawParametersToRules[rawparams_] := 
	Block[{rawparameters},
		rawparameters = DeleteCases[Cases[Flatten[{rawparams}], _String], ""];
		rawparameters = Flatten[StringSplit[#, "&"]& /@ rawparameters];
		rawparameters = StringSplit[#, "="]& /@ rawparameters;
		Flatten[If[Length[#] === 2, Rule @@ #, {}]& /@ rawparameters]
	]


extendedParams[baseurl_, params_] :=
	Block[{id, newparams = Append[params,"uuid"->ToString[$WolframUUID]]},
		id = If[TrueQ[$LocalServer], $AlphaQueryLocalServerID, If[StringQ[#], #, None]& @ $AlphaQueryAppID[]];
		If[!StringQ[id], Return[Internal`HouseKeep[baseurl, newparams]]];
		newparams = Flatten[{"?appid" -> id, newparams, "mlicense" -> $ActivationKey, "machineid" -> $MachineID}];
		StringJoin[baseurl, Riffle[{First[#], "=", Last[#]}& /@ Cases[newparams, _[_String, _String]], "&"]]
	]


(* ::Subsubsection::Closed:: *)
(*"FullSiteURL", "CDFURL"*)


WolframAlpha[str_String, format: ("FullSiteURL" | "CDFURL" | "ExampleURL"), opts:OptionsPattern[]] := 
	Block[{baseurl, jsp, assumption, rawparameters, params, methodOptions},

		methodOptions = allMethodOptions[opts];
				
		baseurl = "Server" /. methodOptions;
		If[!StringQ[baseurl], baseurl = getPreference["BaseURL", "Automatic"]];
		If[!StringQ[baseurl], baseurl = "Automatic"];
		baseurl = baseurl /. $AlphaQueryBaseURLs;
		baseurl = $AlphaQueryBaseURLProtocol <> Switch[ ToLowerCase[baseurl],
				"devel", "www.devel.wolframalpha.com/",
				"test", "www.test.wolframalpha.com/",
				"test2", "www.test2.wolframalpha.com/",
				"current", "www.current.wolframalpha.com/",
				"preview", "preview.wolframalpha.com/",
				"japanese" | "ja", "ja.wolframalpha.com/",
				"spanish" | "es", "es.wolframalpha.com/",
				"current-spanish", "es.current.wolframalpha.com/",
				"production" | "public" | "api" | "english" | "en", "www.wolframalpha.com/",
				"centos6-dev", "www.centos6-dev.wolframalpha.com/",
				"centos6-cur", "www.centos6-cur.wolframalpha.com/",
				"centos6-test", "www.centos6-test.wolframalpha.com/",
				"centos6-test2", "www.centos6-test2.wolframalpha.com/",
				"automatic",
					Switch[$Language,
						"Japanese", "ja.wolframalpha.com/",
						"English" | _, "www.wolframalpha.com/"
					],
				_, "www.wolframalpha.com/" (* any other string is assumed to be invalid *)
		];
		
		If[format === "ExampleURL", Return[StringJoin[baseurl, "examples/", str]]];
		jsp = If[format === "CDFURL", "cdf/", "input/"];
				
		assumption = OptionValue[InputAssumptions];
		assumption = Switch[assumption,
			_String, "a" -> assumption,
			{__String}, Map["a" -> # &, assumption],
			_, {}
		];
		
		rawparameters = rawParametersToRules["RawParameters" /. methodOptions];
		
		params = Flatten[{
			"i" -> urlencode[If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> str],
			assumption,
			rawparameters
		}];
		
		StringJoin[baseurl, jsp, "?", Riffle[{First[#], "=", Last[#]}& /@ Cases[params, _[_String, _String]], "&"]]
	]




(* ::Subsubsection::Closed:: *)
(*"WolframParse", "MathematicaParse"*)


(*
The set of calls for parsing are much like those for querying, but they
call a different jsp and have different import characteristics.
*)


WolframAlpha[str_String, "MathematicaParse" | "WolframParse", opts:OptionsPattern[]] := 
	Block[{result,$AlphaQueryMMode = "inline"},
		result = WolframAlpha[str, "RawWolframParse", opts];
		Switch[result,
			{___Rule, "Parse" -> _String, ___Rule},
			mparseMakeExpression["Parse" /. result],
			_,
			$Failed
		]
	]


WolframAlpha[str_String, "RawMathematicaParse" | "RawWolframParse", opts:OptionsPattern[]] := 
	Block[{result, timeconstraint, sideeffects},
		timeconstraint = First[N[Flatten[{ OptionValue[TimeConstraint] }]]];
		If[!TrueQ[timeconstraint > 0], timeconstraint = TimeConstraint /. $WolframAlphaDefaultOptions];
		TimeConstrained[
			(
				result = qImport[WolframAlpha[str, "WolframParseURL", opts], "Text"];
				If[StringQ[result], 
					Block[{$ContextPath = {"Internal`MWASymbols`", "System`"}, $Context = "Internal`MWASymbols`Temporary`"},
						With[{res=ToExpression[StringTrim[result]] /. echoTiming["$FromMWARules", $FromMWARules]},
							If[TrueQ[$LinguisticAssistantSideEffects] && MemberQ[res, "SideEffects" -> {__}],
								If[TrueQ[WolframAlphaClient`Internal`$EchoMode] && DownValues[Internal`CacheEntityNames] === {},
									echoTiming["Loading EntityFramework", Internal`CacheEntityNames]
								];
								echoTiming["Internal`CacheEntityNames", 
									Quiet[
										Internal`CacheEntityNames[
											ReleaseHold["SideEffects"/.res] 
									]];
								];
							];
							res
						]
					]
					, 
 					$Failed
				]
			),
			timeconstraint,
			Message[WolframAlpha::timeout, HoldForm[WolframAlpha[str, "WolframParse", opts]], timeconstraint];
			$TimedOut
		]
	] // dontRefresh


WolframAlpha[str_String, "MathematicaParseURL" | "WolframParseURL", opts:OptionsPattern[]] := 
	Block[{baseurl, assumption, dpparse, msessinfo, latlong, userip, rawparameters, params, methodOptions},
	
		methodOptions = allMethodOptions[opts];

		baseurl = "Server" /. methodOptions;
		baseurl = $AlphaQueryBaseURL[baseurl] <> $AlphaParseJSP;
		
		assumption = OptionValue[InputAssumptions];
		assumption = Switch[assumption,
			_String, {assumption},
			{__String}, assumption,
			_, {}
		] // preprocessAssumptionsParameters;
		
		dpparse = Switch[
			"DataPacletParse" /. ("DataOptions" /. SystemOptions[]),
			True, "dpparse" -> "true",
			False, "dpparse" -> "false",
			_, {}
		];
		
		msessinfo = getMathematicaSessionInfo[str, False, False];
		msessinfo = Switch[msessinfo,
			{__Rule}, "msessinfo" -> urlencode @ ToString[msessinfo, InputForm],
			_, {}
		];
		
		latlong = Replace["GeoLocation" /. methodOptions, Automatic :> $AlphaQueryGeoLocation];
		latlong = Switch[latlong, 
			_GeoPosition, "latlong" -> urlencode @ iGeoLocationtoLatLong @ latlong,
			_String, "latlong" -> urlencode @ latlong,
			_, {}
		];
		
		userip = "UserIPString" /. methodOptions;
		userip = Switch[userip,
			_String, "ip" -> urlencode[userip],
			Automatic,
				(* include the user's $RequesterAddress *)
				Replace[$RequesterAddress, {ip_String :> ("ip" -> urlencode[ip]), _ :> {}}],
			_, {}
		];

		rawparameters = rawParametersToRules["RawParameters" /. methodOptions];
		
		params = Flatten[{
			"input" -> urlencode[If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> str],
			assumption,
			dpparse,
			"releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]],
			"patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]],
			"systemid" -> urlencode[$SystemID],
			"mclient" -> $AlphaQueryMClient,
			"mmode" -> $AlphaQueryMMode,
			msessinfo,
			latlong,
			userip,
			rawparameters
		}];
		
		extendedParams[baseurl, params]
	]



(* ::Subsubsection::Closed:: *)
(*"WolframAlphaNotebookParse"*)


WolframAlpha[str_?ValidQuery, "WolframAlphaNotebookParse", opts: OptionsPattern[]] := 
	Block[{xml, queryvalues, querydata, inputstring, parse, podid, pod, sbsCount},
		xml = WolframAlpha[str, "WolframAlphaNotebookParseXML", opts];
		If[xml === $TimedOut, Return[$TimedOut]];
		Replace[xml, {
			XMLObject["Document"][_, XMLElement["queryresult", values_, data_], _] :>
				(queryvalues = values; querydata = data),
			_ :> (queryvalues = None; querydata = None)
		}];
		
		(* check for the presence of SBS pods *)
		sbsCount = Count[xml,
			XMLElement["pod", values_, data_] /; Or[
				(* in W|A NE *)
				MemberQ[values, "stepbystepcontenttype" -> "StepByStep"],
				(* otherwise *)
				MemberQ[data, XMLElement["states", _, states_] /;
					Not[FreeQ[states, "name" -> ("Hide steps" | "Show steps" | "Start over")]]]
			],
			Infinity
		];

		(* extract the inputstring returned by the server, if any *)
		inputstring = FirstCase[queryvalues, ("inputstring" -> a_String) :> a, None];
		
		(* grab the parse if there is one *)
		parse = FirstCase[querydata, XMLElement["mathematicaparse", _, {a_String}] :> mparseMakeExpression[a], None];
		
		If[parse === None,
			(* look for a pod result *)
			podid = FirstCase[querydata, XMLElement["pod", {___, "id" -> id_, ___}, _] :> id, None];
			If[podid =!= None,
				parse = With[{podid = podid, assumptions = OptionValue[InputAssumptions]},
					If[StringQ[str] && StringContainsQ[str, p: RegularExpression["^" <> $sbsRegEx <> "|" <> $sbsRegEx <> "$"] /; p =!= ""],
						(* If steps were requested, there's no parse, and there is a pod that's not detectably SBS
						content, return a WolframAlphaResult that references the full pod, not just one subpod *)
						$cachedWAResult = {{str, podid}, xml};
						HoldComplete[WolframAlphaResult[str, podid, InputAssumptions -> assumptions, Method -> {"WANParse" -> True}]],
						$cachedWAResult = {{str, {{podid, 1}, "Content"}}, xml};
						HoldComplete[WolframAlphaResult[str, {{podid, 1}, "Content"}, InputAssumptions -> assumptions, Method -> {"WANParse" -> True}]]
					]
				],
				(* if there are no pods, look for example page information *)
				parse = FirstCase[querydata, XMLElement["examplepage", _, _], None]
			]
		];
		
		If[parse === None && $TryClientParseRawWL,
			writeToLogWithTimestamp[{
					"No parse found. Trying local client parse...",
					Defer[clientParseRawWL[str]],
					AbsoluteTiming[parse = clientParseRawWL[str]]
				},
				CellDingbat -> TooltipBox[StyleBox["\[FilledSmallSquare]", FontSize -> 18, FontColor -> RGBColor[1, 0.5, 0]], "clientParseRawWL"]
			];
			If[MatchQ[parse, $Failed | HoldComplete[Inactivate[_]]], parse = None];
			parse = Replace[parse, HoldComplete[expr_] :> ("ClientParse" :> expr)];
		];
		
		(*
			FIXME: If there are errors of any kind, include those somehow in the
			result, possibly by returning a parse that is a Failure object.
		*)
		
		(* Return just the {parse, assumptions} as a pair, for now *)
		{
			If[sbsCount > 0,
				(* 
				If there are SBS pods, return the query and xml wrapped in a way that
				downstream code can format the result without repeating the query.
				*)
				With[{xml = xml}, Defer[AlphaIntegration`WolframAlphaStepByStep[str, xml]]],
				(* otherwise, return the parse, pod reference, example page info, or None *)
				Replace[parse, HoldComplete[expr_] :> Defer[expr]]
			],
			Cases[querydata, XMLElement["assumptions", _, _]]
		}
	]



(*
We use a Method option to trigger the difference between "URL" and
"WolframAlphaNotebookParseURL" because the formatting of W|A notebook outputs
shares some code with the formatting of double-equal output. Because of that, we
need something in the formatted output that remembers which kind of query
generated it, so that any callbacks (like pod state changes) can continue to use
the same jsp.

This Method option setting is used as that something.
*)

WolframAlpha[str_?ValidQuery, "WolframAlphaNotebookParseURL", opts: OptionsPattern[]] := 
	Block[{query},
		(*
			Replace any sequence of newline characters in the user's query
			string with a space character, for now. This will need to be
			reassessed if the W|A parser starts interpreting newlines in
			interesting ways (e.g. for entering grids).
			
			Note that this replacement is only done here for string queries.
			For typeset queries, this needs to be done before the JSON is
			constructed. We do this in the queryBoxesToQueryList utility.
		*)
		query = If[StringQ[str], StringReplace[str, ("\n" | "\r" | "\[IndentingNewLine]").. -> " "], str];
		WolframAlpha[query, "URL", opts, Method -> {"WANParse" -> True}]
	]



(*
The clientParseRawWL utility uses string analysis and ToExpression to determine
if there is a natural WL expression to extract from a string. In the context of
"WolframAlphaNotebookParse", this utility is only called after all parsing
attempts involving the server have failed.
*)


$TryClientParseRawWL = True;


$rawWLRequiredPatterns = ("[" ~~ ___ ~~"]") | ("<|" ~~ ___ ~~ "|>") | "@" | "&" | "//" | "/." | "/;";


$rawWLInactivatedPatterns := $rawWLInactivatedPatterns = Alternatives @@ Join[
	(* symbols *)
	ToExpression[#, InputForm, HoldPattern]& /@ Union[
		Replace[UsingFrontEnd[FrontEndResource["DangerousSymbols"]], Except[_List] :> {}],
		{
			(* killing session *)
			"Exit", "Quit",
			(* meddling with attributes *)
			"Attributes", "ClearAttributes", "SetAttributes", "Unprotect", "Protected",
			(* potentially concealing something nefarious *)
			"Symbol", "ToExpression",
			(* writing to cloud *)
			"CloudDeploy", "CloudExport", "CloudPut", "CloudSave",
			(* modifying files *)
			"CopyFile", "CreateFile", "DeleteDirectory", "DeleteFile", "DumpSave", "Encode", "Export", "OpenAppend", "OpenWrite", "OutputStream", "Put", "PutAppend", "RenameFile", "Save"
		}
	],
	{
		(* global symbols with name length > 1 char and no definitions *)
		x_Symbol /; Context[x] == "Global`" && OwnValues[x] === {} && DownValues[x] === {} && SubValues[x] === {} && StringLength[SymbolName[Unevaluated[x]]] > 1
	}
];

(* convert bound symbols x (from Module, With) into "x", so they won't trigger the Global symbol check in $rawWLInactivatedPatterns (bug 379084) *)
stringifyBoundSymbols[expr_] := Module[{syms, rules},
	syms = Join @@ Cases[expr, (Module | With)[{vars__}, _] :> Replace[Hold[vars], HoldPattern[Set][s_, _] :> s, {1}], Infinity];
	syms = List @@ (Hold /@ syms);
	rules = Replace[syms, Hold[x_] :> With[{xs = SymbolName[Unevaluated[x]]}, HoldPattern[x] :> xs], {1}];
	expr /. rules
]

clientParseRawWL[query_?ValidQuery] := Module[{parse, querystrings, checks},
	querystrings = Replace[query, {
		a_String :> {a},
		(* For a typeset query, check every one of the user's strings *)
		a_Typeset`Query :> a["allUserStrings"]
	}];
	checks = Table[
		If[
			StringQ[str] && SyntaxQ[str] && MatchQ[parse = ToExpression[str, InputForm, HoldComplete], HoldComplete[_]],
			If[MatchQ[parse, HoldComplete[Times[(_Symbol) ..]]] && StringFreeQ[str, $rawWLRequiredPatterns],
				(* exclude trivial Times parse for sequences of words *)
				False,
				If[
					(*
					Note: the third arg and option to FreeQ here seem redundant as they
					are the defaults; however, mtrott included them in his outline so
					maybe he has his reasons
					*)
					FreeQ[stringifyBoundSymbols[parse], $rawWLInactivatedPatterns, {0, Infinity}, Heads -> True],
					True,
					(* potentially dangerous code detected -- wrap in Inactivate *)
					Inactivate
				]
			],
			False
		],
		{str, querystrings}
	];
	(*
		For a typeset query, if all the individual strings passed those checks, then
		assemble the full expression string and check that it also parses cleanly.
	*)
	If[MatchQ[query, _Typeset`Query] && MatchQ[checks, {(True|Inactivate)..}],
		Replace[query, Typeset`Query["boxes" -> boxes_, ___] :> 
			With[{str = queryBoxesToRowBoxString[boxes]},
				parse = If[StringQ[str], ToExpression[str, InputForm, HoldComplete], $Failed];
				If[!MatchQ[parse, HoldComplete[_]] || !FreeQ[parse, _ErrorBox], parse = $Failed];
			]
		]
	];
	Switch[checks,
		{True..},
			parse,
		{(True|Inactivate)..},
			Replace[parse, HoldComplete[expr_] :> HoldComplete[Inactivate[expr]]],
		_,
			$Failed
	]
]



(* ::Subsubsection::Closed:: *)
(*"RawMInterface"*)


WolframAlpha[str_String, "RawImageEditingQuery", opts:OptionsPattern[]] :=
	Block[{result, minput, interactiveVersion},
		result = WolframAlpha[str, "RawMInterface", opts];
		If[!MatchQ[result, {___Rule}],
			Return[$Failed]
			,
			minput = "MInput" /. result /. "MInput" -> None;
			If[StringQ[minput], minput = ToExpression[minput, InputForm, Hold]];
			interactiveVersion = "InteractiveVersion" /. result /. "InteractiveVersion" -> None;
			If[StringQ[interactiveVersion], interactiveVersion = ToExpression[interactiveVersion, InputForm, Hold]];
		];
		{minput, interactiveVersion}
	]


WolframAlpha[str_String, "RawMInterface", opts:OptionsPattern[]] := 
	Block[{resultstr, resultexpr, timeconstraint},
		timeconstraint = First[N[Flatten[{ OptionValue[TimeConstraint] }]]];
		If[!TrueQ[timeconstraint > 0], timeconstraint = TimeConstraint /. $WolframAlphaDefaultOptions];
		TimeConstrained[
			(
				Which[
					(* If there is a kernel cache, use it *)
					StringQ @ kernelCacheRawMInterface[str],
					resultstr = kernelCacheRawMInterface[str];
					resultexpr = ToExpression[resultstr],
					
					(* Otherwise, use the default cache if possible *)
					TrueQ[$MInterfaceUseCachedQueriesFirst] && defaultCacheRawMInterface[str] =!= "$Failed",
					resultstr = defaultCacheRawMInterface[str];
					resultexpr = ToExpression[resultstr],
					
					(* Otherwise, if there is a front end cache, use it *)
					StringQ @ frontendCacheRawMInterface[str],
					resultstr = frontendCacheRawMInterface[str];
					resultexpr = ToExpression[resultstr],
					
					(* Otherwise, perform a fresh query *)
					resultstr = If[TrueQ[$MInterfaceFallbackToWeb], qImport[WolframAlpha[str, "MInterfaceURL", opts], "Text"], $Failed];
					resultexpr = If[StringQ[resultstr], ToExpression[resultstr], $Failed];
					
					(* If that returned a good interface, use it, and remember it in the front end cache *)
					MatchQ[resultexpr, {___, "MInput" -> Except["", _String], ___, "InteractiveVersion" -> Except["", _String], ___}],
					updateFrontEndCache[str, resultstr],
					
					(* If all else failed, check the cache built into this file *)
					defaultCacheRawMInterface[str] =!= "$Failed",
					resultstr = defaultCacheRawMInterface[str];
					resultexpr = ToExpression[resultstr];
				];
				(* Cache the resultstr for this kernel session, and return the resultexpr *)
				If[StringQ[resultstr], kernelCacheRawMInterface[str] = resultstr];
				resultexpr
			),
			timeconstraint,
			Message[WolframAlpha::timeout, HoldForm[WolframAlpha[str, "RawMInterface", opts]], timeconstraint];
			$TimedOut
		]
	] // dontRefresh


WolframAlpha[str_String, "MInterfaceURL", opts:OptionsPattern[]] :=
	Block[{methodOptions, baseurl, params, msessinfo, languagecode},
		
		methodOptions = allMethodOptions[opts];
		
		baseurl = "Server" /. methodOptions;
		baseurl = $AlphaQueryBaseURL[baseurl] <> $AlphaMInterfaceJSP;
		
		msessinfo = getMathematicaSessionInfo[str, False, True];
		msessinfo = Switch[msessinfo,
			{__Rule}, "msessinfo" -> urlencode @ ToString[msessinfo, InputForm],
			_, {}
		];

		languagecode = "Language" /. methodOptions;
		languagecode = $AlphaQueryLanguageCode[languagecode];
		languagecode = Switch[languagecode,
			"en", {},
			_String, "languagecode" -> languagecode,
			_, {}
		];
		
		params = Flatten[{
			"input" -> urlencode[If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> str],
			"releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]],
			"patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]],
			"systemid" -> urlencode[$SystemID],
			"mclient" -> $AlphaQueryMClient,
			"mmode" -> $AlphaQueryMMode,
			msessinfo,
			languagecode
		}];
		
		extendedParams[baseurl, params]
	]


(* ::Subsubsection::Closed:: *)
(*"ToQuantity"*)


WolframAlpha[str_String, "ToQuantity", opts:OptionsPattern[]] :=
	Block[{$AlphaQueryMMode = "qparse"}, WolframAlpha[str, "WolframParse", opts]]


(* ::Subsubsection::Closed:: *)
(*"LinguisticAssistant"*)


WolframAlpha[str_String, "LinguisticAssistant" | "InlineInput", OptionsPattern[]] :=
	AlphaIntegration`LinguisticAssistant[str, OptionValue[InputAssumptions], Automatic]


(* ::Subsubsection::Closed:: *)
(*"LinguisticExpression"*)


WolframAlpha[str_String, "LinguisticExpression" | "ControlEqualExpression", opts:OptionsPattern[]] :=
	ReleaseHold[WolframAlpha[str, "LinguisticHeldExpression", opts]]


WolframAlpha[str_String, "LinguisticHeldExpression" | "ControlEqualHeldExpression", opts:OptionsPattern[]] :=
Block[{expr},
	expr = WolframAlpha[str, "LinguisticAssistant", opts];
	Replace[MakeExpression[ToBoxes[expr]], Except[_HoldComplete] -> $Failed]
]


(* ::Subsubsection::Closed:: *)
(*"LinguisticAssistantReport"*)


WolframAlpha[str_String, "LinguisticAssistantReport" | "ControlEqualReport", opts:OptionsPattern[]] :=
Block[{
		parseurl, parseimport, parseimporttiming, parseresult, parseassumptions, 
		queryurl, queryimport, queryimporttiming, queryresult, queryassumptions },

	parseurl = WolframAlpha[str, "WolframParseURL", opts];

	parseimporttiming = AbsoluteTime[];
	parseimport = Import[parseurl, "Text"];
	parseimporttiming = AbsoluteTime[] - parseimporttiming;

	queryurl = WolframAlpha[str, "URL", opts, Method -> {"Formats" -> {"minput", "moutput"}}];

	queryimporttiming = AbsoluteTime[];
	queryimport = Import[queryurl, "XML"];
	queryimporttiming = AbsoluteTime[] - queryimporttiming;

	parseresult = "Parse" /. Replace[parseimport, {a_String :> ToExpression[a], _ :> {}}] /. "Parse" -> $Failed;
	queryresult = Cases[queryimport, XMLElement["minput" | "moutput", _, _], Infinity];

	parseassumptions = "Assumptions" /. Replace[parseimport, {a_String :> ToExpression[a], _ :> {}}] /. "Parse" -> $Failed;
	queryassumptions = Cases[queryimport, XMLElement["assumptions", _, _], Infinity];

	Grid[{
		{Tooltip["  ", Grid[{
			{"", DateString[]},
			{"Version", $Version},
			{"ReleaseID", SystemInformation["Kernel", "ReleaseID"]},
			{"Paclet info", PacletManager`PacletInformation /@ PacletManager`PacletFind["WolframAlphaClient"]}},
			Alignment -> Left]],"mparse.jsp", "query.jsp"},
		{"Import timing", parseimporttiming, queryimporttiming},
		{"results", parseresult, queryresult},
		{"assumptions", parseassumptions, queryassumptions},
		{"url", parseurl, queryurl},
		{"raw results",
			OpenerView[{HoldForm[Import["mparse.jsp", "Text"]], parseimport}],
			OpenerView[{HoldForm[Import["query.jsp", "XML"]], queryimport}]}
		},
		Alignment -> Left,
		Frame -> All,
		FrameStyle -> LightGray,
		Background -> {Automatic, {{GrayLevel[1], GrayLevel[0.97]}}},
		BaseStyle -> {ShowStringCharacters -> True},
		ItemStyle -> {
			{Directive["Text", Bold, ShowStringCharacters -> False]},
			{Directive["Text", Bold, ShowStringCharacters -> False]}}
	]
]



(* ::Subsubsection::Closed:: *)
(*"LinguisticAssistantEchoLog"*)


WolframAlpha[str_String, "LinguisticEchoLog", opts:OptionsPattern[]] :=
	Block[{ WolframAlphaClient`Internal`$EchoMode = True, doEcho, log = {}},
		doEcho[expr_] := (AppendTo[log, expr]; expr);
		doEcho[expr_, label_] := (AppendTo[log, Row[{Style[label, "EchoLabel"], " ", expr}]]; expr);
		doEcho[expr_, label_, f_] := (AppendTo[log, Row[{Style[label, "EchoLabel"], " ", f[expr]}]]; expr);		
		WolframAlpha[str, "LinguisticAssistant", opts];
		Column[log]
	]


WolframAlpha[str_String, "LinguisticEchoAssociation", opts:OptionsPattern[]] :=
	Block[{ WolframAlphaClient`Internal`$EchoMode = True, doEcho, log = {}},
		doEcho[expr_] := (AppendTo[log, expr]; expr);
		doEcho[expr_, label_] := (AppendTo[log,label -> expr]; expr);
		doEcho[expr_, label_, f_] := (AppendTo[log, label -> f[expr]]; expr);		
		WolframAlpha[str, "LinguisticAssistant", opts];
		Association[log //. {Style[x_, ___] :> x, Tooltip[x_, ___] :> x}]
	]


(* ::Subsubsection::Closed:: *)
(*"Validate"*)


WolframAlpha[str_String, "Validate", opts:OptionsPattern[]] := 
	Switch[
		WolframAlpha[str, "ValidateXML", opts],
		XMLObject["Document"][_, XMLElement["validatequeryresult", {___, "success" -> "true", ___}, _], _],
		True,
		XMLObject["Document"][_, XMLElement["validatequeryresult", {___, "success" -> "false", ___}, _], _],
		False,
		_,
		$Failed
	]


WolframAlpha[str_String, "ValidateXML", opts:OptionsPattern[]] := 
	Block[{timeconstraint},
		timeconstraint = First[N[Flatten[{ OptionValue[TimeConstraint] }]]];
		If[!TrueQ[timeconstraint > 0], timeconstraint = TimeConstraint /. $WolframAlphaDefaultOptions];
		TimeConstrained[
			qImport[WolframAlpha[str, "ValidateURL", opts], "XML"],
			timeconstraint,
			Message[WolframAlpha::timeout, HoldForm[WolframAlpha[str, "Validate", opts]], timeconstraint];
			$TimedOut
		]
	] // dontRefresh


WolframAlpha[str_String, "ValidateURL", OptionsPattern[]] :=
	Block[{baseurl, params},
		
		baseurl = "Server" /. allMethodOptions[opts];
		baseurl = $AlphaQueryBaseURL[baseurl] <> $AlphaValidateJSP;
		
		params = {
			"input" -> urlencode[If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> str],
			"releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]],
			"patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]],
			"systemid" -> urlencode[$SystemID],
			"mclient" -> $AlphaQueryMClient,
			"mmode" -> $AlphaQueryMMode
		};
		
		extendedParams[baseurl, params]
	]


(* ::Subsubsection::Closed:: *)
(*"Image"*)


WolframAlpha[str_String, "Image", opts: OptionsPattern[]] := 
	Block[{raw = WolframAlpha[str, "RawImage", opts]},
		Switch[raw,
			{200, {__Integer}, {{_String, _String}...}}, Replace[rawImageDataToImage[raw], Except[_Image] :> $Failed],
			{501, _, _}, Missing["NoImage"],
			{_, _, _}, $Failed,
			_, $Failed
		]
	]


WolframAlpha[str_String, "RawImage", opts: OptionsPattern[]] := 
	Block[{timeconstraint},
		timeconstraint = First[N[Flatten[{ OptionValue[TimeConstraint] }]]];
		If[!TrueQ[timeconstraint > 0], timeconstraint = TimeConstraint /. $WolframAlphaDefaultOptions];
		TimeConstrained[
			qURLFetch[WolframAlpha[str, "ImageURL", opts], {"StatusCode", "ContentData", "Headers"}],
			timeconstraint,
			Message[WolframAlpha::timeout, HoldForm[WolframAlpha[str, "Image", opts]], timeconstraint];
			$TimedOut
		]
	] // dontRefresh


WolframAlpha[str_String, "ImageURL", opts: OptionsPattern[]] :=
	Block[{baseurl, params, rawparameters, methodOptions},
		
		methodOptions = allMethodOptions[opts];

		baseurl = "Server" /. methodOptions;
		baseurl = $AlphaQueryBaseURL[baseurl] <> $AlphaSimpleJSP;
				
		rawparameters = rawParametersToRules["RawParameters" /. methodOptions];
		
		params = Flatten[{
			"i" -> urlencode[If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> str],
			"releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]],
			"patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]],
			"systemid" -> urlencode[$SystemID],
			"mclient" -> $AlphaQueryMClient,
			"mmode" -> $AlphaQueryMMode,
			rawparameters
		}];
		
		extendedParams[baseurl, params]
	]


(* rawImageDataToImage utility from Jeremy Michelson *)
rawImageDataToImage[{status_, data_, headers_}] := 
	Block[{contentType, charset, result},
		contentType = StringTrim @ FirstCase[headers, {"Content-Type", t_} :> t, "image/gif"];
		charset = "Unicode";
		contentType = StringReplace[contentType,
			RegularExpression["\\s*;\\s*charset=(\\S*)\\s*$"] :>
				(charset = CloudObject`ToCharacterEncoding["$1"]; "")
		];
		contentType = StringReplace[contentType, {
			RegularExpression["^image/(\\w*)$"] :> With[{format = ToUpperCase["$1"]},
				If[!MemberQ[$ImportFormats, format], "String", format]],
			RegularExpression["^.*$"] -> "String"
		}];
		result = FromCharacterCode[data, charset];
		If["String" =!= contentType,
			Image[ImportString[result, contentType], Magnification -> 1],
			$Failed
		]
	]


(* ::Subsubsection::Closed:: *)
(*"ShortAnswer"*)


WolframAlpha[str_String, "ShortAnswer", opts:OptionsPattern[]] := 
	Block[{raw = WolframAlpha[str, "RawShortAnswer", opts]},
		Switch[raw,
			{200, _String}, Last @ raw,
			{501, _}, Missing["NoShortAnswer"],
			{_, _}, $Failed,
			_, $Failed
		]
	]


WolframAlpha[str_String, "RawShortAnswer", opts:OptionsPattern[]] := 
	Block[{timeconstraint},
		timeconstraint = First[N[Flatten[{ OptionValue[TimeConstraint] }]]];
		If[!TrueQ[timeconstraint > 0], timeconstraint = TimeConstraint /. $WolframAlphaDefaultOptions];
		TimeConstrained[
			qURLFetch[WolframAlpha[str, "ShortAnswerURL", opts], {"StatusCode", "Content"}],
			timeconstraint,
			Message[WolframAlpha::timeout, HoldForm[WolframAlpha[str, "ShortAnswer", opts]], timeconstraint];
			$TimedOut
		]
	] // dontRefresh


WolframAlpha[str_String, "ShortAnswerURL", opts: OptionsPattern[]] :=
	Block[{baseurl, params, methodOptions, rawparameters},
		
		methodOptions = allMethodOptions[opts];
		
		baseurl = "Server" /. methodOptions;
		baseurl = $AlphaQueryBaseURL[baseurl] <> $AlphaResultJSP;
		
		rawparameters = rawParametersToRules["RawParameters" /. methodOptions];
		
		params = Flatten[{
			"i" -> urlencode[If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> str],
			"releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]],
			"patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]],
			"systemid" -> urlencode[$SystemID],
			"mclient" -> $AlphaQueryMClient,
			"mmode" -> $AlphaQueryMMode,
			rawparameters
		}];
		
		extendedParams[baseurl, params]
	]


(* ::Subsubsection::Closed:: *)
(*"SpokenResult"*)


WolframAlpha[str_String, "SpokenResult", opts:OptionsPattern[]] := 
	Block[{raw = WolframAlpha[str, "RawSpokenResult", opts]},
		Switch[raw,
			{200, _String}, Last @ raw,
			{501, _}, Missing["NoSpokenResult"],
			{_, _}, $Failed,
			_, $Failed
		]
	]


WolframAlpha[str_String, "RawSpokenResult", opts:OptionsPattern[]] := 
	Block[{timeconstraint},
		timeconstraint = First[N[Flatten[{ OptionValue[TimeConstraint] }]]];
		If[!TrueQ[timeconstraint > 0], timeconstraint = TimeConstraint /. $WolframAlphaDefaultOptions];
		TimeConstrained[
			qURLFetch[WolframAlpha[str, "SpokenResultURL", opts], {"StatusCode", "Content"}],
			timeconstraint,
			Message[WolframAlpha::timeout, HoldForm[WolframAlpha[str, "SpokenResult", opts]], timeconstraint];
			$TimedOut
		]
	] // dontRefresh


WolframAlpha[str_String, "SpokenResultURL", opts: OptionsPattern[]] :=
	Block[{baseurl, params, methodOptions},
		
		methodOptions = allMethodOptions[opts];
		
		baseurl = "Server" /. allMethodOptions[opts];
		baseurl = $AlphaQueryBaseURL[baseurl] <> $AlphaSpokenJSP;
		
		rawparameters = rawParametersToRules["RawParameters" /. methodOptions];
		
		params = Flatten[{
			"i" -> urlencode[If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> str],
			"releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]],
			"patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]],
			"systemid" -> urlencode[$SystemID],
			"mclient" -> $AlphaQueryMClient,
			"mmode" -> $AlphaQueryMMode,
			rawparameters
		}];
		
		extendedParams[baseurl, params]
	]


(* ::Subsubsection::Closed:: *)
(*Notes for when the second argument is a list*)


(*
When the second argument to WolframAlpha[] is a list, it overrides any settings
given for IncludePods and ExcludePods.

This list can have one or two elements.

The first element is a podid, {podid, subpodid}, or list of such. Podids can be
strings, lists of strings, or All. Subpodids can be integers, lists of integers,
or All. The 0th subpod is used to indicate pod-level properties.

The second element is a property name, a list of property names, or All.

When the given list has only a first element (that is, only has a podid/subpodid
or a list of such), the default second argument is "Properties". The return
value in this case is a list of LHSs only; no actual data is returned.

When the given list has a unique first element and a unique second element other
than "Properties", the return value is just the single piece of data being
requested. If that piece of data doesn't exist, the return value is
Missing["NotAvailable"].

In the remaining cases, where either there is more than one podid/subpodid or
more than one property given, the result will be of the form {prop -> val, prop
-> val, ...}.

Any Missing["NotAvailable"] results will be removed if they came from a
speculative specification of All, and will be left alone if they came from a
literal request for a specific piece of information. The goal is for the result
to contain a rule for every specifically requested property. So this:

WolframAlpha["foobar", { {{"Result", {1,2}}, "fish"}, "Plaintext"}]

might return something like this (note the lack of "fish" in the result):

{
	{{"Result", 1}, "Plaintext"} -> "This is foobar",
	{{"Result", 2}, "Plaintext"} -> Missing["NotAvailable"]
}

*)



(*
The expandIDsAndProperties utility canonicalizes the user's second argument to
its fully expanded form, where the first element is a list of pairs of the form
{All | _String, All | _Integer}, and the second element is All or a list of
property names.
*)

expandIDsAndProperties[{a_}] := expandIDsAndProperties[{a, "Properties"}]

expandIDsAndProperties[{a_, b_}] := {expandIDs[a], expandProperties[Flatten[{b}]]}

expandIDsAndProperties[other_] := InvalidIDAndProperty[other]


expandIDs[a_String] := {{a, All}}

expandIDs[All] := {{All, All}}

expandIDs[{a : (All | _String), b : (All | _Integer)}] := {{a, b}}

expandIDs[{a : (All | _String), b : {__Integer}}] := Map[{a, #}&, b]

expandIDs[{a : {__String}, b : (All | _Integer)}] := Map[{#, b}&, a]

expandIDs[{a : {__String}, b : {__Integer}}] := Tuples[{a, b}]

expandIDs[list_List] := Join @@ Map[expandIDs, list]

expandIDs[other_] := {{InvalidPodID[other], InvalidSubpodID[]}}


expandProperties[{All}] := All

expandProperties[{a__String}] := {a}

expandProperties[other_] := InvalidProperty[other]


validExpandedIDsAndPropertiesQ[expr_] :=
	MatchQ[expr, { {{All | _String, All | _Integer}..}, All | {__String}}]


(*
Note that every list that ends in an actual property makes targeted, efficient
use of the API, requesting only those formats related to that property.
*)


propertyToFormat[a_List] := Union[Flatten[propertyToFormat /@ a]]
propertyToFormat["Plaintext"] = "plaintext"
propertyToFormat["Cell"] = "cell"
propertyToFormat["Content"] = "cell"
propertyToFormat["Input"] = "minput"
propertyToFormat["Output"] = "moutput"
propertyToFormat["MathML"] = "mathml"
propertyToFormat["HTML"] = "html"
propertyToFormat["Image"] = "image"
propertyToFormat["Sound"] = "msound"
propertyToFormat["DataFormats"] = "dataformats"
propertyToFormat["ComputableData"] = "computabledata"
propertyToFormat["FormattedData"] = "formatteddata"
propertyToFormat["FormulaData"] = "formuladata"
propertyToFormat["NumberData"] = "numberdata"
propertyToFormat["QuantityData"] = "quantitydata"
propertyToFormat["SoundData"] = "sounddata"
propertyToFormat["TimeSeriesData"] = "timeseriesdata"
propertyToFormat["Properties"] := $allFormats
propertyToFormat[All] := $allFormats
propertyToFormat[other_] = "plaintext"


$allFormats = {"plaintext", "minput", "moutput", (* "mathml", "html", *) "cell", "image",
	"sound", "msound", "dataformats", "computabledata", "formatteddata", "formuladata", "numberdata", "quantitydata", "sounddata", "timeseriesdata"};


(* ::Subsubsection::Closed:: *)
(*Second argument is a list of podids + subpodids + properties*)


WolframAlpha[str_?ValidQuery, list_List, opts:OptionsPattern[]] :=
	Block[{idp = expandIDsAndProperties[list]},
		getPropertyInformation[str, idp, opts] /; validExpandedIDsAndPropertiesQ[idp]
	]


getPropertyInformation[str_?ValidQuery, {ids:{{_,_}..}, props_}, opts___] :=
	Block[{includepods, result, fmt},

		fmt = If[props === {"Properties"}, "PodProperties", "PodInformation"];
		includepods = DeleteDuplicates[First /@ ids];
		includepods = If[MatchQ[includepods, {__String}], IncludePods -> includepods, Unevaluated[Sequence[]]];

		result = WolframAlpha[str, fmt, Method -> {"Formats" -> propertyToFormat[props]}, includepods, opts];

		extractProperties[result, ids, props]
	];


(* One specific result *)
extractProperties[result_, {{podid:Except[All], subpodid:Except[All]}}, {prop:Except["Properties"]}] := 
	{{podid, subpodid}, prop} /. result /. {{podid, subpodid}, prop} -> Missing["NotAvailable"]


(* List of LHSs, no actual data *)
extractProperties[result_, ids_, {"Properties"}] := Cases[result, {idsToPattern[ids], _}]


(* List of rules *)
extractProperties[result_, ids_, props_] := 
	Map[
		If[
			FreeQ[#, All],
			(* If the request is for a specific piece of information, return Missing[...] if it's missing *)
			# -> (# /. result /. # -> Missing["NotAvailable"]),
			(* If the request contains an All, then allow it to be absent from the return value *)
			Cases[result, _[{idToPattern[First[#]], propToPattern[Last[#]]}, _]]
		]&,
		Tuples[{ids, Flatten[{props}]}]
	] // Flatten


(* Utilities *)
idsToPattern[ids_] := Alternatives @@ Map[idToPattern, ids]
idToPattern[{podid:All, subpodid:All}] := {_, _}
idToPattern[{podid:All, subpodid_}] := {_, subpodid}
idToPattern[{podid_, subpodid:All}] := {podid, _}
idToPattern[{podid_, subpodid_}] := {podid, subpodid}

propToPattern[All] := _String
propToPattern[prop_String] := prop

propsToPattern[All] := _String
propsToPattern[{props__String}] := Alternatives[props]


(* ::Subsubsection::Closed:: *)
(*Held first argument*)


WolframAlpha[Hold[expr_], rest___] := WolframAlpha[ToString[Unevaluated[expr], InputForm], rest]


WolframAlpha[HoldPattern[Unevaluated][expr_], rest___] := WolframAlpha[ToString[Unevaluated[expr], InputForm], rest]


(* ::Subsubsection::Closed:: *)
(*Non-string first argument*)


(* Emit a message for now, although it may someday be possible to let W|A handle these natively *)


WolframAlpha[expr : Except[_String], rest___] := Null /; Message[WolframAlpha::string, 1, HoldForm[WolframAlpha[expr, rest]]]


(* ::Subsubsection::Closed:: *)
(*Unsupported second argument*)


WolframAlpha[str_String, fmt_, OptionsPattern[]] := Null /; Message[WolframAlpha::format, HoldForm[fmt]]




(* ::Subsubsection::Closed:: *)
(*Utility: mathematicaSessionInfo*)


getMathematicaSessionInfo[query_String, tryDialog_, imageEditingMode_] := 
Module[{imageRule},

	If[tryDialog, mathematicaSessionInfoPermissionDialogIfNecessary[query, {}]];

	imageRule = If[TrueQ[imageEditingMode], "ImageEditingMode" -> True, Unevaluated[Sequence[]]];

	Switch[
		{$AlphaQueryGlobalSendMathematicaSessionInfo[], $AlphaQuerySendMathematicaSessionInfo, $Notebooks},
		
		(* In a kernel-only session, always opt out *)
		{_, _, False},
		{"Allowed" -> False, imageRule},
		
		(* Otherwise, we're in a front end session. *)
		(* If the user has opted in or out globally, respect that setting *)
		{False, _, _},
		{"Allowed" -> False, imageRule}, 

		{True, _, _},
		Join[{"Allowed" -> True, imageRule}, mathematicaSessionInfo[query]],
		
		(* Otherwise, if the user has opted in or out locally, respect that setting *)
		{_, False, _},
		{"Allowed" -> False, imageRule},
		
		{_, True, _},
		Join[{"Allowed" -> True, imageRule}, mathematicaSessionInfo[query]],
		
		(* Otherwise, the user hasn't opted in or out yet. *)
		(* Use the session default on Cloud, which does not yet store these options reliably; *)
		(* else, don't get the session info for now. *)
		{_, _, _},
		If[TrueQ[$CloudEvaluation && $sendSessionInfoDefault],
			Join[{"Allowed" -> True, imageRule}, mathematicaSessionInfo[query]],
			{"Allowed" -> "Unknown", imageRule}
		]
	]
]


(*
Put up the permission dialog if and only if at least one of the following is true:
- There is a variable name in the user's query is a symbol with OwnValues in their session, or
- There is an MInput Scanner result which contains an minput string which contains a 
	subexpression with head Out or head Placeholder.

Never send msessinfo from a kernel-only session.
*)


mathematicaSessionInfoPermissionDialogIfNecessary[query_String, xml_] :=
	If[mathematicaSessionInfoPermissionDialogNecessaryQ[query, xml], mathematicaSessionInfoPermissionDialog[query], None]

mathematicaSessionInfoPermissionDialogNecessaryQ[query_String, xml_] :=
Block[{percentlist, symbolnames, minputs},
	Which[
		(* In a kernel-only session, don't ask. *)
		$Notebooks === False,
		False,
		
		(* In a Cloud session, there's nowhere to store the answer, so don't ask *)
		TrueQ[$CloudEvaluation],
		False,
		
		(* If the user has opted in or out globally, don't ask again *)
		MemberQ[{True, False}, $AlphaQueryGlobalSendMathematicaSessionInfo[]],
		False,
		
		(* If the user has opted in or out locally, don't ask again *)
		MemberQ[{True, False}, $AlphaQuerySendMathematicaSessionInfo],
		False,
		
		(* If the user is running in the plugin or in player, don't ask, just opt out *)
		CurrentValue["PluginEnabled"] || MemberQ[$ProductInformation, "ProductIDName" -> "MathematicaPlayer"],
		False,
		
		(* During a synchronous dynamic evaluation, don't ask, just opt out *)
		FrontEnd`$DynamicEvaluation === True && FrontEnd`$SynchronousEvaluation === True,
		False,
		
		(* If we're in a W|A notebook, ask *)
		TrueQ[$WolframAlphaNotebook],
		True,
		
		(* If the query string contains a symbol in the current context that has OwnValues, ask *)
		symbolnames = extractSymbolNames[query];
		symbolnames = Select[symbolnames, MatchQ[ToExpression[#, InputForm, OwnValues], {_RuleDelayed}]&];
		symbolnames =!= {},
		True,
		
		(* If the query string contains explicit references to %, ask *)
		percentlist = extractPercentReferences[query];
		percentlist =!= {},
		True,
		
		(* *)
		query==="MWACalculateData",
		True,
		
		(*
		If the xml query results contain an MInput Scanner pod which contains an minput string which
		contains a subexpression with head Out or Placeholder, ask.
		*)
		minputs = Cases[xml, XMLElement["pod", {___, "scanner" -> "MInput", ___}, data_] :> data, Infinity];
		minputs = Cases[minputs, XMLElement["minput", _, {str_String}] :> str, Infinity];
		minputs = !FreeQ[ToExpression[#, InputForm, HoldComplete]& /@ minputs, _Out | _Placeholder];
		minputs,
		True,
		
		(* otherwise, don't ask *)
		True,
		False
	]
];


mathematicaSessionInfoPermissionDialog[query_] :=
DialogInput[
	Pane[
		Column[
			{
				TextCell[Row[{Style[cachedFrontEndResource["WAStrings", "SessionInfoQuestion"], "DialogBody"]}]]
				,
				OpenerView[
						{
							Dynamic[FEPrivate`FrontEndResource["WAStrings", "SessionInfoDetails"]],
							Panel[
								Pane[
									Grid[List @@@ mathematicaSessionInfo[query], Alignment -> {{Right, Left}, Baseline}],
									FrameMargins -> 5,
									ImageSize -> {Automatic, {1, 300}},
									Scrollbars -> Automatic,
									AppearanceElements->{}
								],
								DefaultBaseStyle -> {},
								FrameMargins -> 3
							]
						},
						BaseStyle ->  "DialogBody"
					]
				,
				Item[
					Grid[{
						If[SystemInformation["FrontEnd", "OperatingSystem"] === "MacOSX", #, RotateLeft[#]]& @ {
						Button[Dynamic[FEPrivate`FrontEndResource["WAStrings", "SessionInfoNo"]], 
							$AlphaQuerySendMathematicaSessionInfo = False;
							setPreference[$FrontEnd, "SendMathematicaSessionInfo", Automatic];
							DialogReturn[False],
							Appearance -> "CancelButton",
							ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]]
						],
						Button[Dynamic[FEPrivate`FrontEndResource["WAStrings", "SessionInfoYesAlways"]],
							$AlphaQuerySendMathematicaSessionInfo = True;
							setPreference[$FrontEnd, "SendMathematicaSessionInfo", True];
							DialogReturn[True],
							Appearance -> "DialogBox",
							ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]]
						],
						Button[Dynamic[FEPrivate`FrontEndResource["WAStrings", "SessionInfoYesSession"]],
							$AlphaQuerySendMathematicaSessionInfo = True;
							setPreference[$FrontEnd, "SendMathematicaSessionInfo", Automatic];
							DialogReturn[True],
							Appearance -> "DefaultButton",
							ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]]
						]
					}}],
					Alignment -> Right
				]
			},
			Alignment -> Left,
			Spacings -> 2
		],
		ImageMargins -> {{30,30},{10,10}}
	],
	Background :> CurrentValue[{"SystemColors","Window"}],
	WindowSize -> {500, FitAll},
	WindowTitle -> cachedFrontEndResource["WAStrings", "SessionInfoWindowTitle"]
]


mathematicaSessionInfo[query_String : ""] :=
Block[{percentlist, outlist, symbolnames, cloudurl},

	percentlist = Union[Select[{$Line - 1}, Positive], extractPercentReferences[query]];

	outlist = extractOtherOutReferences[query];

	symbolnames = extractSymbolNames[query];

	cloudurl = If[TrueQ[$AlphaQueryIncludeEvaluationCloudObjectURL && $CloudEvaluation],
		Replace[$EvaluationCloudObject, {CloudObject[url_String, ___] :> url, _ -> None}],
		None
	];

	Flatten[{
		"$Line" -> $Line,
		"$VersionNumber" -> $VersionNumber,
		If[StringQ[cloudurl], "CloudObjectURL" -> cloudurl, Nothing],
		"TryRealOnly" -> TrueQ[CurrentValue[FrontEnd`EvaluationNotebook[], {TaggingRules, "TryRealOnly"}, False]],
		getSkeleton[Union[percentlist, outlist], "Inputs"],
		getSkeleton[Union[percentlist, outlist], "Outputs"],
		getSkeleton[Union[symbolnames], "Symbols"]
	}]
]


$cjkCharacterPat := $cjkCharacterPat = RegularExpression[
	"[" <>
	Map[FromCharacterCode, {
		{12352, 45, 12447}, (* Hiragana : http://www.unicode.org/charts/PDF/U3040.pdf *)
		{12448, 45, 12543}, (* Katakana : http://www.unicode.org/charts/PDF/U30A0.pdf *)
		{13312, 45, 19893}, (* CJK Unified Ideographs Extension A : http://www.unicode.org/charts/PDF/U3400.pdf *)
		{19968, 45, 40917}, (* CJK Unified Idographs : http://www.unicode.org/charts/PDF/U4E00.pdf *)
		{44032, 45, 55215}, (* Hangul Syllables : http://www.unicode.org/charts/PDF/UAC00.pdf *)
		{65280, 45, 65519}  (* Halfwidth and Fullwidth Forms : http://www.unicode.org/charts/PDF/UFF00.pdf *)
	}] <>
	"]"
];


extractSymbolNames[str_String] := 
With[{query =
		(* Treat some characters as if they were surrounded by whitespace. 420161 *)
		StringReplace[str, $cjkCharacterPat :> " $0 "]},
	Join[
		Select[
			Replace[
				StringCases[" " <> query <> " ",
					{
						StringExpression[
							Except[{LetterCharacter, "$"}],
							a:{LetterCharacter, "$"},
							b:{WordCharacter, "$"}...,
							Except[{WordCharacter, "$"}]
						] :> StringJoin[a, b],
						StringExpression[
							Except[{LetterCharacter, "$"}],
							"D" | "d",
							a : LetterCharacter,
							"/"
						] :> a
					},
					Overlaps -> True
				],
				{
					"C" -> "\[ScriptCapitalC]",
					"D" -> "\[ScriptCapitalD]",
					"E" -> "\[ScriptCapitalE]",
					"I" -> "\[ScriptCapitalI]",
					"K" -> "\[ScriptCapitalK]",
					"N" -> "\[ScriptCapitalN]",
					"O" -> "\[ScriptCapitalO]"
				},
				{1}
			],
			NameQ[#] && Context[#] === $Context &
		],
		With[{subscriptSymbols = Replace[DownValues[Subscript][[All, 1]], _[s_Subscript] :> ToString[Unevaluated[s], InputForm], {1}]},
			If[Length[subscriptSymbols] > 0,
				Intersection[
					subscriptSymbols,
					StringCases[" " <> query <> " ", {
						StringExpression[
							Except[{LetterCharacter, "$"}],
							a:LetterCharacter.., "_", b:DigitCharacter..,
							Except[DigitCharacter]
						] :> StringJoin["Subscript[", a, ", ", b, "]"],
						StringExpression[
							"Subscript[",
							a:LetterCharacter.., ",", WhitespaceCharacter..., b:DigitCharacter..,
							"]"
						] :> StringJoin["Subscript[", a, ", ", b, "]"]
					}]
				],
				{}
			]
		]
	]
];


extractPercentReferences[query_String] := 
Block[{percent, percentpercent, percentn},
	percentpercent = StringCases[
		" " <> query <> " ",
		Except["%"] ~~ a:("%"..) ~~ Except["%" | DigitCharacter] :> StringLength[a]];
	percentn = StringCases[
		" " <> query <> " ",
		Except["%"] ~~ "%" ~~ a:(DigitCharacter..) ~~ Except[DigitCharacter] :> a];
	Select[Union[Flatten[{$Line - percentpercent, ToExpression /@ percentn}]], Positive]
]


(*
The extractOtherOutReferences utility supports context sensitive queries new in 12.
Rough spec:
* out/Out/output/Output treated as equivalent.
* parens/brackets/space/no space.
* absolute line numbers always supported.
* relative line numbers supported for delimited and spaced cases.
* Also support #17 as a synonym for %17.

Some examples:
"plot out 17"
"plot Output(-3)"
"plot #17"

Not supported:
"plot out-17"
"plot %[17]"
"plot # 17"
"plot #-17"

The patterns here correspond to regular expressions defined in the following W|A file:

	CalculateParse/JavaSource/com/wolfram/alpha/parser/tokenizer/domaintokenizers/patterns/MInputSessionOutputPattern1.java
*)

extractOtherOutReferences[query_String] := 
Block[{out, secondToLast},
	out = ("out" | "Out" | "output" | "Output");
	secondToLast = RegularExpression[
		(* Out[-2] (PREVIOUS_OUTPUT_WORDPHRASE_GROUP from MInputSessionOutputPattern1.java) *)
		"(?<![A-Za-z])(?i)" <>
		"(" <>
		"(?:the )?(?:pen[ -]?ultimate |(?:second|2nd)[ -](?:to[ -])?last )(?:expression|output|result)(?:'?s)?" <>
		"|" <>
		"(?:the )?(?:expression |one |output |result )(?:before(?: last)?)(?:'?s)?" <>
		")" <>
		"|" <>
		(* Out[-1 | -2] (PREVIOUS_OR_CURRENT_OUTPUT_WORDPHRASE_GROUP from MInputSessionOutputPattern1.java) *)
		"(?<![A-Za-z])(?i)" <>
		"(" <>
		"(?:the )?(?:above |last |previous )(?:expression|output|result)(?:'?s)?" <>
		"|" <>
		"(?:the )?(?:expression |output |result )(?:above)(?:'?s)?" <>
		"|" <>
		"(?:the )?(?:above|expression|output|result)(?:'?s)?" <>
		")"
	];
	Union[Flatten[{
		StringCases[" " <> query <> " ", {
			("#" ~~ a: (DigitCharacter..) ~~ Except[DigitCharacter]) :> ToExpression[a],
			(out ~~ WhitespaceCharacter... ~~ "[" ~~ a: (DigitCharacter..) ~~ "]") :> ToExpression[a],
			(out ~~ WhitespaceCharacter... ~~ "(" ~~ a: (DigitCharacter..) ~~ ")") :> ToExpression[a],
			(out ~~ WhitespaceCharacter... ~~ "{" ~~ a: (DigitCharacter..) ~~ "}") :> ToExpression[a],
			(out ~~ WhitespaceCharacter... ~~ a: (DigitCharacter..) ~~ Except[DigitCharacter]) :> ToExpression[a],
			(out ~~ WhitespaceCharacter... ~~ "[-" ~~ a: (DigitCharacter..) ~~ "]") :> $Line - ToExpression[a],
			(out ~~ WhitespaceCharacter... ~~ "(-" ~~ a: (DigitCharacter..) ~~ ")") :> $Line - ToExpression[a],
			(out ~~ WhitespaceCharacter... ~~ "{-" ~~ a: (DigitCharacter..) ~~ "}") :> $Line - ToExpression[a],
			(out ~~ WhitespaceCharacter.. ~~ "-" ~~ a: (DigitCharacter..) ~~ Except[DigitCharacter]) :> $Line - ToExpression[a],
			secondToLast :> Max[$Line - 2, 0]
		}]
	}]]
]


getSkeleton[list_, type_] := 
Block[{result, f},
	f = Switch[type, "Inputs", toInputSkeleton, "Outputs", toOutputSkeleton, "Symbols", toSymbolSkeleton];
	result = (# -> f[#])& /@ list;
	result = Flatten[DeleteCases[result, _ -> None]];
	If[result === {} || result === None, {}, type -> result]
]


toInputSkeleton[n_] :=
	Replace[DownValues[In], { {___, _[_[n]] :> val_, ___} :> toSkeleton[val, "Input"], _ -> None }]


toOutputSkeleton[n_] :=
	Replace[DownValues[Out], { {___, _[_[n]] :> val_, ___} :> toSkeleton[val, "Output"], _ -> None }]


SetAttributes[toSymbolSkeleton, HoldFirst];

toSymbolSkeleton[a_String] := ToExpression[a, InputForm, toSymbolSkeleton]

(* Only return information for symbols that have a single OwnValue. Other values are not handled. *)

toSymbolSkeleton[a_Symbol] := 
	Replace[{OwnValues[a], DownValues[a]}, {
		{{_ :> val_}, _} :> toSkeleton[val, "Output"],
		{_, list: Except[{}]} :> toSkeleton[list, "DownValues"],
		_ -> None
	}]

toSymbolSkeleton[a : Subscript[_, _]] :=
	Replace[DownValues[Subscript], { {___, HoldPattern[_[a]] :> val_, ___} :> toSkeleton[val, "Output"], _ -> None }]


$MaxByteCount = 5000;
$MaxStringLength = 78;
$MaxDownValuesStringLength = 200;

(* Don't send subexpressions matching this pattern as part of msessinfo: *)
$redactValuePattern = HoldPattern[Alternatives[
	_AuthenticationDialog,
	_BlockchainKeyEncode,
	_BlockchainTransactionSign,
	_CloudConnect,
	_Decrypt,
	_DecryptFile,
	_DerivedKey,
	_DigitalSignature,
	_Encode,
	_Encrypt,
	_EncryptFile,
	_GenerateDerivedKey,
	_GenerateDigitalSignature,
	_GenerateSymmetricKey,
	_Get,
	_MailServerConnect,
	_PermissionsKey,
	_PrivateKey,
	_RemoteConnect,
	_SecuredAuthenticationKey,
	_SendMail,
	_SendMessage,
	_SymmetricKey,
	_VerifyDerivedKey,

	_[ Authentication | "Password" | "SSHKey" | "SSHKeyPassword", _],
	_[ Authentication | "Password" | "SSHKey" | "SSHKeyPassword", _] // KeyValuePattern
]];


(*
Note that we block Dataset throughout the toSkeleton utility below, because
Dataset has lots of up values, and not all of them respect Unevaluated. This
should systemically avoid evaluation leaks like 389860.
*)

SetAttributes[toSkeleton, HoldAllComplete]

toSkeleton[list_List, mode: "DownValues"] :=
	Block[{str, Dataset},
		str = ToString[Unevaluated[list], InputForm];
		If[StringLength[str] <= $MaxDownValuesStringLength,
			{ "DownValues" -> str },
			{ "DownValues" -> "Length" -> Length[Unevaluated[list]] }
		] // encodeSkeleton
	]

toSkeleton[expr_, mode: "Input" | "Output"] := Block[{Dataset},
	DeleteCases[
		{
			"Dimensions" -> Dimensions[Unevaluated[expr]],
			"Head" -> Replace[HoldComplete[expr], {
				HoldComplete[a_Symbol[___]] :> ToString[Unevaluated[a], InputForm],
				HoldComplete[a_ /; AtomQ[Unevaluated[a]]] :> ToString[Head[Unevaluated[a]], InputForm],
				_ -> None}],
			"StringLength" -> If[StringQ[Unevaluated[expr]], StringLength[expr], None],
			"PlotRange" -> Replace[HoldComplete[expr], {
				HoldComplete[(Graphics|Graphics3D)[__, PlotRange -> a_, ___]] |
				HoldComplete[(Graphics|Graphics3D)[_, {___, PlotRange -> a_, ___}]] :> ToString[Unevaluated[a], InputForm], _ -> None}],
			Block[{str, value, heldexpr},
				value = Which[ (* Send the whole value if it's small, unredacted, and converts to a small string *)
					ByteCount[Unevaluated[expr]] <= $MaxByteCount &&
					FreeQ[Unevaluated[expr], $redactValuePattern] && 
					StringLength[str = ToString[Unevaluated[expr], InputForm]] <= $MaxStringLength,
						"Value" -> str,
					(* else, send a skeleton, if it converts to a small string *)
					heldexpr = HoldForm[expr] /. {
						$redactValuePattern :> Missing["Redacted"],
						(g : Graphics | Graphics3D)[__] :> g[__],
						a_List :> {{}} /; MatrixQ[Unevaluated[a]],
						a_List :> {} /; VectorQ[Unevaluated[a]]
					};
					str = Replace[heldexpr, HoldForm[x_] :> ToString[Unevaluated[x], InputForm]];
					StringLength[str] <= $MaxStringLength,
						"Skeleton" -> str,
					(* else, send nothing *)
					True,
						"Value" -> None
				];
				Which[
					!MemberQ[{True, Automatic}, $AlphaQueryIncludeSemanticTypes],
						{value, "SemanticTypes" -> None},
					$AlphaQueryIncludeSemanticTypes === Automatic && !MatchQ[value, "Value" -> None],
						(* Automatic: Send semantic types only when not sending the value *)
						{value, "SemanticTypes" -> None},
					True,
						(* True: Send the semantic types. FIXME: Should we cap the size? *)
						{
							value,
							"SemanticTypes" -> Replace[expressionSemanticTypes[expr, mode], Except[{__}] :> None]
						}
				]
			]
		} // Flatten,
		_ -> None
	] // encodeSkeleton
]



encodeSkeleton[list: {__Rule}] :=
	Prepend[encodeRule[$AlphaQuerySkeletonEncoding, #]& /@ list, 0 -> $AlphaQuerySkeletonEncoding] /; $AlphaQuerySkeletonEncoding > 0;
encodeSkeleton[arg_] := arg

encodeRule[enc_, key: "SemanticTypes" -> types_List] :=
	Replace[key, skeletonKeyRules[enc]] -> Replace[types, skeletonSemanticTypeRules[enc], {1}];
encodeRule[enc_, key_ -> val_] := Replace[key, skeletonKeyRules[enc]] -> val


skeletonKeyRules[enc_] := Replace[skeletonKeys[enc], {
	keys_List :> (skeletonKeyRules[enc] = Dispatch[MapIndexed[# -> First[#2]&, keys]]),
	_ :> {}
}];

skeletonSemanticTypeRules[enc_] := Replace[skeletonSemanticTypes[enc], {
	types_List :> (skeletonSemanticTypeRules[enc] = Dispatch[MapIndexed[# -> First[#2]&, types]]),
	_ :> {}
}];


skeletonKeys[1] = {
"Dimensions",
"Head",
"StringLength",
"PlotRange",
"Value",
"Skeleton",
"SemanticTypes",
"DownValues"
};

skeletonKeys[_] := {}


(*
Predictions`MakePredictions[Null]; (* forced autoloading *)
	Cases[
		DownValues[Predictions`SemanticTypeNamePredictionTypes],
		_[a_, b_List] \[RuleDelayed] b[[All, 1]]
	] // Flatten // Union//Compress
*)

skeletonSemanticTypes[1] := skeletonSemanticTypes[1] = Uncompress @ 
"1:eJyVnW1z3DaSx1N1V7W1lfJmHcfxQ2zHjmk7z9nN1n0AWw+\
O6iR7VqPYVfcOmoFHPHGICUjKmvv0BzRAEmj8MTN5YQtE/wgCzUajAZKYJ+\
fq9MOz//jkk+Y/P/nkk+Oyaae3TeLlrC1VfSLrrhiT00+tpF5UsqD/\
pzeH43250FK63BtD7qTk3KmYl6J2uZ9RbrkUrS2P/nrSppUu+\
oSrUdOUi3op67YYk9M7TqJmpbC1LIL09DHJWq1qtVwXe7KSTVuKaqKa0sqP6lXXTr+\
MoINZVa4a6UR3I9GJUvXkQvTCJyRc17MLK++aM9FcFjxj+\
pWl2laX510rrXKL6IhU9aprW1Nz92f6nckxVa26SuiXXauMAkTBMlT9z/1pa9W1G/3G0z/\
sRBM71M6KizN53doE3Z69Czm7PFfXRZ+gm7BXqW7+9vx/5cy0cXJ02NVkNqTfUEZpp0J+\
WpCePs/LjuoPSi/pFu+bBpCOMyi14gGT78tVpdbivJL+WncZcGiKH+\
pPlVSVMULZtlIXQTqQVeV8kFGaugod+9yb7NiV8IRya2Nkoqyl00vBM6ijmMzW3IXC/\
53eSvOotU99fll3cn6oBbWiSHKmzwxn1TcRs0qaWzZflnVpL9uWV3K/vCqbXr/\
f70ruVaJppr/8Kdw18B9/6py3XWtP+\
mHXkw5M29v17hdxvKvZP//cSb5qd9hZpZ5p8aEldT7Iy5wCn2wBXMW+\
2UL5mjzMY14tGwoKFfF0G+Yv+GXCrZRuh46KRa7hjzfLR7+7AfK1SJXcU77V+\
WLCRqe6iSl/Nd4wP2aUM1FRw3nDmNy1/tvN0L6Uq+nlOlN9Bnvqxy31UsbtT9dNK5f+\
BO4U+Amt0J68HZOvdDlfSGLu5ySumV9vFDu1P97IYKWPkK9htpDwBj/\
ZDPlLJXVWYj4Txtm24yhUbGecAr7bEXQV/H5H2tf02Xbcq2eHgkNF/\
bArjj3gq66s5mW9QB4wlEEPmACwcyYU9oAhhj1gSkAPmGK46SZKGjvQg7wMNj0BYNMTCjc\
9xHDTUwI2PcX8BW8xzkio2fdwvq/\
Eo01SVwHekgihuOfrTYSv3heMsYEYrF8vcPfkfkbqq5+\
UupQtLtULXKm81ZHUtZq3KUJ8m5LKecZXLldEqNzHGxk8rhtoJeo1NOpRhEerBMBGzSk8t\
FOwbKYmoydOrsYAp/\
7nu1CuYi92QX3tkkYw1mtkW5GhXr7difXXT6xKaeOkzIRu70L4MCypYo+\
caVE3dm5FWHLDu7rV7oZ/lRXBQI7L4dDLId8e7nkCyiW9QhMlYc5d+rvdYF+FxFIG+\
u1Mijos+PMY3RdOlXdgttPVg7wMur4Q8BW8CwmvGHx6WOlHGwh/\
hdsc6ZNJ8TZKXYmZnGh1LpG5pYTTw4udMNgjMiy2oBTGFpTjoAXlYOyjD0xPvDgy3m3mOu\
TXG8VOPcV2xtXs2XbQ1+rxRhLPCCDkLvx8B9Jf+R5A/+\
jEpTOYR5ukThtPtyKuTkhrMYcj/Bj0qthYWKgJpDMG4kH1oJJ2URP52EAEfSyXQx/\
LITyiBhQevxMAjt8JhX1JEMJxsyCJ6VErqdMoMZY6nXB1XovlqnJGxe/\
JKHKLzGYs9Y3lih9J6LADuS+Ad4QReC1taKXX/\
9r3KLfjET1aioUcOH4DGOepn7LUcVlLoY2yFlosl2bSgsP/\
4IwT0eryGrvGAHsj249KX77WYnWxta5TM3LOtyqJZpRNW84aPJEPULssvbWKFuq0DBV1l8\
NqVYnaR+0PNwidBSRa4wScNKUYDtkiLmcnCYL9XcrhMPXQDF3nSl1O7bOT6kTOS0G6YIo6\
NGFibQnUpSKhD9599X/\
eRHZay3q2PrieXYh6uEVPN5xyVM9lb5r3E65s5WutuhUaWpkYelLG4HEQQnAchCR2//a5Q\
1c5zTMv+VpU4toF3/dzEri+xsRwwscYvL42QrhbcjkcgDiElw1ey1pCJcja+\
E9ke6Nk70KrpWrMETaQgITqGsS2EtivjsybzmSotpzLqfyjM1bcn/\
ElP0PNZeNuH9fIKDL/\
un4h9pfN1DBTCweun7PnnMoPUlP1Kvt4U5W9F05aryq1sCs7E6lLNacKF9sZuLCZBeHCZp\
bGC5sIxwubeRIubOZxHCwZ3gyr9rFhZvkhAeAYklA45nyt1KKSk6pruKdmi1A0IJPgXkYA\
F6FiKVyEihE8wRmYjJnFYuyTYiaj/\
B4agkR2pd9MIKH0ZoNGDDToLAgNOktjg0Y4Nug8CQ06j+P58VHdtMKGidzCmDM+\
akxkMUcj0iiBLpaJ4d1nDB6RRgiPSFwORyQO4ZH5qFGtsTBqbVKLQQTDCS7P1CKGsLkHFI\
61EwBOzBIKj8L/XSsXEN/F+a61DzcI4fJSROD1gB7BjwdiKfRRMYKbd9wvN9zF+bB5x+\
kSw6MNBG7ecbyskClgQ/\
OO0VLCbc6YaZ5A3XOUwO7JxLB7MgZ3zxHC3ZPLYcfgEH7EdmwmDp3wz58f5GXwEVsCwE6T\
UPgRW4jhOXZKwNliimHPdCxaM1WGnikQQc/E5ZkbEEPYMwUU9kwJkFEyo/Bs9bisL+\
2rUXyEYtc8EXUt57Qse1I2w+tMz3ehnMJ+2Bl1zflxZ9437MUuJ+CXOTaxrjY/\
7X4CDuOsZk3ccCYbuE7NxHCdGjFwkRSBeJ2akXh+DiE4P4ck9mgnspVKTy/UR+keGyc1i+\
VOI0nNAJSpGSB9zbipMxQvsmEKPuPEKHZBJ2UttV8T4hobRdAFcTl0QRzCLiigsAtKAOiC\
EirTNcpa6cm4aMi7RizGXQMwuGsAMNM1YjLTNRCEDRCReAQ+\
sQ9FRVmjETiUwRE4AfCd4RQegUMMj8ApAUfgFMNvkZyoq9KNv/\
cyAjjVjqUwzouRjCH2DJ5qMzGM5hiD48k38rxfmryfk8B4kolhDRiDve8I4XiSy6Ev4VDu\
UuXi4lzpC+WXDJJLxXLo6CEE+\
xkksaPnKHb0mIKOHqP4qSgtdwp9cL2qVJN57xExcDUlC8LVlCyNV1MQjldT8iRcTcnj2Bt\
5/lSKWevfNftmGwHfh8hg8H2IDIvXe1IYP1HLcfB9iByMPSi9RoM86CCAHjSWQg8aI9iDD\
gz2oEwM/Rdj8ILDROhLauRdnA8XHCIhXHCICLzg0CN4wSGWQjXGCJ6UGcbM3Sr5cjaTlQm\
henN/sRPmGv/j7iyc4mw6wdf6253OwA9+\
NsKuQj//iTNwJNWfgiKpUAYjqQSAkVRCYd8VYjiSSgkYSaVY5oK0QF3Ozsqqf1WcXzAhoL\
PMYNBZZljsLFMYO8scB51lDsaBmKEbP/\
Tez0lgIMbE0JExBkdHI4QDMS6HgRiHMhZxsW7s7Nx9pAItIiGwRWAMWwRmMxaRwBmLyHDY\
IjJwxr3bGdpep6/g63OxFL4+BxD4OgngcKAYg/\
hRJmLg9BeBeAnATVWFXttPdUkX3Og4AF8FxxSMnjGaGSA56/\
WyrchQNdyoMJtxHONqBXccbKEC3U2+RsH7PFye4I6Dr0xkC9nkOPB6xBcpBT/\
CGAQwpIylOBaKEBxSDgwOKZk4q9CtL85OVLW+\
kHPtrZ03JpLinp8iuOenXKbnR2Cm5wMG93wA4iXIiVatLMfPwbEILkFyOTY6BmX8z0jhJc\
gEwOEZpzL9uasaoWF/HiS4P8dibH4xk+nPA5Tpz0yOVcsg/\
E3DVNhPXsrWDXEPNwjh6zUpAQPVFMOvaEYcfmERILBnAQ6HQlO5NB6hnDnmbL2S/\
GuT6ZsJsoYtT8mCj4APpbBvyhL1fBcKPiXbgMKnZBt4/\
JQMn4Cfkm1i4RRy0wnYCdMTtWz4FUuhEwYINpWUw044BrETRgx0wgjETni6krNSNsgJByL\
ohLkcegoOYSccUNgJJwB0wgmFR3r7Rfxe1RlzcZ6Y34lYDJ8DIQbfCQDi50CMxM+\
BIATXpyGJF7ksSorg/\
rsNByTu2tpkOOK9qE0HI94N22goyhSwYZErRjLDULeSulZXAg5DoRAPQwmBh6EEywxDIZc\
ZhlIE+5aUy3TyTn8Q+\
H2TQIQ7OZPjTs6gTCcfqUwn5wDu5JzCVzu7kHqp5utaLMsZtfybLYCvFC9HqxVN69v+\
G9snWwA4P8UUnJ9iFM9PExbPTzMYnJ9mWBzPnnV1LSsUwYwSGM8yMYxnGYPj2RHC8SyXQw\
PmEH6Od/axpP2fWGzGOvjv9VzqRoowMHu6FXFK+nY3Dq4/5WDflmIr7fW3vdhQkd/\
vSGPH9E5VM1Er5JgCEXRMXA7vK4ewqwgo7JgSADqmhMLD0XszeTAerKrQcBQJ4XCUEnA4S\
jE8HEUcHo4AAocjwOG7/t4Yx4WPvb7KilzrH2TlvrIslnmv9BzFMn2+K/UeFvoiWXX/\
52iyp+Zw9AxE0Ei5HBoph7CRBhQ20gSARppQ/mp/\
dZi02wS4TR7p6Kxc9hvukc7sLmp2I0S7PV144Nx7TnqiaNvHZ5w56aq2vBK6NJeKSvuKk6\
einqvlO4ueV9JZaCj/vYbl/MVyJ9PC/HN3tVuupuLKtNInnI7s0HWgtdK/\
mcvY5znFYVnJN6o9tB+/\
UpeJxZNKtaf2w0d6KbNeUOF0jliawkst7SN094E0Ftkj2l5wEPUJuvFR7lsdn3aHADMUzU\
/MrayKIO2KrMrVu1J+LPoE7Ql5qM2588L9oSb32yUWr3rlDRso3gzFQ+\
5XYe6o8mjbxddS0Rc+5awpgnQv6/fxLII0GcZr+5Gz0KddJRu/pWOaRWaWJ8/\
UpazJONxHRqGFJDnTv/UcVVV0C3dDxqy+6rdQ7q/\
7OP9f++R6knz7cW0lr0nxOaEpc5PYFP1pL3YyWkEPju1D24as7vNBEH7tPRZfTC+\
UbmXTToxDHW7gX0lczgv7n7+Y6lYF/T/\
9e3xMe6qQeQeZb7UJAN50y3MTJ42iiQm1O7fpmCvKqu63tZm+mO50WQwpsn2Qf1Q3K+\
oAVAf6Krx4Wa0u3Jf0QSZt8+QyvxgyD8t6bj+\
W9u53FBhNrBsT6rrvzKm9TuCOb8TH5u6wHHNDPotzSPfWnI9qu2XnlTy4XmlJL24XaRZ1K\
KrUwR+d2wo3OqJom3IOS1nNizFJAw0d2iu+\
XdnXFOzp0REt8hF0Ym7yCPl9RVVnTGVZBvxPmN+XH+\
h76KO6lQuTJ8cznufO0MY52MaO6C8Ytd+FZ4t/ljsnIZ9gkjXwe0zZo/\
GujPhjjFvvP0LfYGhqVGD807YKTlUVKqkIKF1eR1UMj2lMItD1NaNxu+\
VjTRsvd0v7UfpYMY8c0ntdZupyXF4Gl7zDsCBN85GM7H3ZXpzpctH7DjfDi+\
ErOxcIIQp3Q8b8MXqavdSmtKVsy9loszERtM92NJPSa2rld1v5U6XaXif/\
RbSJU1fK7sVVDzUrYG7QVR/2Z16ZqbHbOTg+\
pMGjzxpl1tMdTV7O57agYkiR4zgW57IyY7L/Sxo8lgtp3FWv1YId0+\
2yvbzfgztIk7uwx5NydmnqNybdWWpm1TERtTlrTNNZJ6IuV11lCxyTdNZwqHQRpGkksaYs\
7efsdltBtRSlqa/\
7S5YTdyvaSNvtafKbWirTKkkeyDtMAKdZ1IsAaR862vttbduHXDFVvFKqksIFB0y0f6ilt\
P3QhpY99zXgOPMwZY7dZ+gbiPjQqzgmlGvKoOJQZiNmL36Riq1nailCZlW9B9hBZ0D/\
JuC25iYqVt1HKckvld75Ytj5wESpcXnp/Sysv8goyX5DVvQJHxHaXme3uaeAt0+\
420zBsbO48v9Mr443YKD53KcD58Lnv8fHLmC4RZldI5VxaMWQohjrjQmh5JyM8Nf9l9oEF\
FRGkO0yv4gz7d5jTnCPCcyEq7aNboZd/QOpc+\
EkuRNLfITiyrzLZHa4lNoNIcmJ3lENu64HsmOjqLaby2NVLyhBXisE7Em30jzKvxnn+\
8uzFo1JXmvvyP1pD6HwVCwWcu4a/QgSUxNKaekLYTWdiBLqsp+\
XDL4kkIVXZE05ldRdgI6jSrCzpiazXiDtO0l4QWZEZ6Kq1kjV71ysTLmqpU2Cij7hGuTGy\
pfVQp5rUc78cP/FKPJTDi/4bBQc1RStrUPYG5iHb4wCn3OH5+yXxp6asdFO5vR+\
JePibo9A75S85GYgkUOu5U10U0tNLmFMUsXcoc+ldr01XbkS68L/JZdgx8WqoP+\
pg9I4aYZoq9YiPKAZE42olGs9QjDXKfbWMzNJpfaH2UGabHKiVt2KfrZjSI351IohRZ6N9\
gSTdjnJfgZJdeI5VO1/\
d4LWeMgowgNSXZ8xSEh1Sa6Z57v3GuyvgCj3gxevhC7iQ2pjkBWKKY52KzimnmYe1sjGuk\
UzRaiGDOr+HNpT184v3wPCgytZj5ujcqmZOyoTnTRlM3wK+xBgJ0Jfqit/TKM+J/\
7dyU6ajtgzqJTomIJMTkxbNbsQdoe0/fIDbexjf9Wkn+\
htKnva6SurKaeIJ4Cwi3RuutGX8wBQ7+1HQ8ZDkCr+NgC+\
7u7l21MTvNXkOUXVOP9TgDxaVztVH43sI/UAuxZT2P+\
ojw1HwxDmamevXIxJMnH3kxrWnoYUleF/uMP/4oY1Vxdl/7pf9AnHuVDch+C3+hzqNEOK+\
iVtZFdMaYu5OR1QgJVmR7+CkoqpUZ+OIirps/\
jYOltykEHeRNu5qHFvZMz0qoKo3OqrsVYzpw/\
eC2HSabeiWEjOg6eAjPm9lsZIjDnJsQHuQrQo5u5leOCU0moplm5e59K0JpFmv7JdwHjZu\
dOxk/vauPNujPk+54nLsTsXBuEiy3D24apntzs2EY7Udom6cQp0kjf+B1RczW+\
MAp9zc8w5Gzm6S+3aWCP9Ty5mul6eq6p4Xalz+662PXDN/HwUhtn3YfZb7dt+\
k4uHkCjK9fQPlG/fU2K/h1PAXGcLJKHebwz9/fRkWtoN5wbGmt6ZOCeL93/pxp/\
J5Ur5r6KL8IDusHWKfvHUJk+l/Y0MH1JDeWNmHl5uFymsz3lZz0/kUml6sc3440qsjIm6+\
pJPerQRJOQeRlya2g+kXiUx4xyLC97ZMdmYybPm8fK8UVXXSndIw5aXHLj+Y/TqZTdGmc+\
hctRiUTmXNSbdXXCHfTYtp8VzjmG1AWeTX/399Lgw/\
2gseCc1Ta69l4sPiXgvrqR9rjOu6fmc4U73xH7ZzLRsZSj/LJD7v/8Pawg5Lg==";

skeletonSemanticTypes[_] := {}


(* ::Subsubsection::Closed:: *)
(*Utility: urlToWolframAlpha*)


(* This utility is basically the inverse of WolframAlpha[query, "URL", opts] *)



splitURL[url_String] := 
Block[{list},
	list = Flatten[MapAt[StringSplit[#, "?"] &, StringSplit[url, "&"], 1]];
	{First[list], Cases[StringSplit[#, "="]& /@ Rest[list], {param_String, value_String} :> param -> value]}
]


urlToWolframAlpha[url_String] := 
Block[{baseurl, list},

	list = Flatten[MapAt[StringSplit[#,"?"]&, StringSplit[url,"&"], 1]];
	baseurl = StringReplace[First[list], $AlphaQueryJSP -> ""];
	baseurl = If[baseurl === $AlphaQueryBaseURL[], {}, {"Server" -> baseurl /. $AlphaQueryBaseURLs}];

	list = Rest[list];
	list = StringSplit[list, "="];

	list = queryParameterToAlphaOption @@@ list;
	list = joinDuplicateOptions[list];

	list = Flatten[{
		"Input" /. list,
		"URL",
		Cases[list, _[_Symbol, _]],
		Method -> Join[baseurl, DeleteCases[Cases[list, _[_String, _]], _["Input", _]]]
	}];

	Hold[WolframAlpha[##]]& @@ list
]


(* Some parameters have no analog in Options[WolframAlpha], so discard them. *)

queryParameterToAlphaOption["appid", a_String] := Sequence[]

queryParameterToAlphaOption["machineid", a_String] := Sequence[]

queryParameterToAlphaOption["mclient", a_String] := Sequence[]

queryParameterToAlphaOption["mlicense", a_String] := Sequence[]

queryParameterToAlphaOption["mmode", a_String] := Sequence[]

queryParameterToAlphaOption["patchlevel", a_String] := Sequence[]

queryParameterToAlphaOption["releaseid", a_String] := Sequence[]

queryParameterToAlphaOption["sig", a_String] := Sequence[]

queryParameterToAlphaOption["systemid", a_String] := Sequence[]


(* msessinfo is useful to see, but it doesn't belong in the final WolframAlpha[] input, so just decode and print it *)

queryParameterToAlphaOption["msessinfo", a_String] := Sequence[] /; (Print[urldecode @ a]; True)


(* the remaining parameters to map to something sensible *)

queryParameterToAlphaOption["assumption", a_String] := InputAssumptions -> {a}

queryParameterToAlphaOption["async", a_String] := Asynchronous -> Switch[a, "true", True, "false", False, other_, ToExpression[a]]

queryParameterToAlphaOption["excludepodid", a_String] := ExcludePods -> {urldecode @ a}

queryParameterToAlphaOption["format", a_String] := "Formats" -> StringSplit[a, ","]

queryParameterToAlphaOption["formattimeout", a_String] := TimeConstraint -> {Automatic, Automatic, Automatic, ToExpression[a]}

queryParameterToAlphaOption["ignorecase", a_String] := IgnoreCase -> If[a === "true", True, False]

queryParameterToAlphaOption["includepodid", a_String] := IncludePods -> {urldecode @ a}

queryParameterToAlphaOption["infowidth", a_String] := PodWidth -> {Automatic, Automatic, Automatic, ToExpression[a]}

queryParameterToAlphaOption["input", a_String] := "Input" -> urldecode[a]

queryParameterToAlphaOption["interactive", a_String] := "Interactive" -> If[a === "true", True, False]

queryParameterToAlphaOption["maxwidth", a_String] := PodWidth -> {Automatic, ToExpression[a]}

queryParameterToAlphaOption["plotwidth", a_String] := PodWidth -> {Automatic, Automatic, ToExpression[a]}

queryParameterToAlphaOption["podstate", a_String] := PodStates -> urldecode /@ StringSplit[a, ","]

queryParameterToAlphaOption["podtimeout", a_String] := TimeConstraint -> {Automatic, Automatic, ToExpression[a]}

queryParameterToAlphaOption["reinterpret", a_String] := "Reinterpret" -> If[a === "true", True, False]

queryParameterToAlphaOption["scantimeout", a_String] := TimeConstraint -> {Automatic, ToExpression[a]}

queryParameterToAlphaOption["width", a_String] := PodWidth -> {ToExpression[a]}

queryParameterToAlphaOption[other_, value_] := "RawParameters" -> StringJoin[other, "=", value]

queryParameterToAlphaOption[other___] := Sequence[]


joinDuplicateOptions[opts_List] := Map[joindup[#, Cases[opts, _[#, val_] :> val]]&, Union[First /@ opts]]

joindup[opt_, {val_}] := opt -> val

joindup[Asynchronous, vals_] := Asynchronous -> False (* this is what the API does *)

joindup[PodWidth, vals_] := PodWidth -> ( First[Sort[#]]& /@ Flatten[vals, {{2}, {1}}] )

joindup[TimeConstraint, vals_] := TimeConstraint -> ( First[Sort[#]]& /@ Flatten[vals, {{2}, {1}}] )

(* default case *)
joindup[opt_, vals_] := opt -> Flatten[vals]


(* ::Subsubsection::Closed:: *)
(*Utility: qImport*)


Quiet[ (* protect against messages during repeated package loading *)
	sendWAEvent[type_] := Module[{eventtype},
		eventtype = Switch[type,
				"MWACalculateData"|"MWAEarthquakeData"|"MWAThermodynamicData"|"AstronomyConvenienceFunction","entity",
				"MWAInterpreter"|"MWASemanticInterpretation", "linguistics",
				"WAEqual"|"MWAFormulaNameLookup"|"MWAGeoEntityLookup"
				|"CurrencyConversionMean"|"Quantity"|"MWAUnitSystem"
				|"PLIParseAgain" | "PLIGrammarDeployAgain","free",
				"MWANames","entity"(*"name"*),
				_,"alpha"
			];
		If[TrueQ[$CloudEvaluation],
		If[SameQ[#,False],
			Switch[eventtype,
				"alpha",Message[WolframAlpha::creditlimit],
				_,Message[CloudSystem`Cloud::creditlimit]
			];Throw[$Failed,"WAE"]]&[
			If[TrueQ[EntityFramework`$SendWAEvents], 
				CloudSystem`Private`SendWAEvent[eventtype], 
				True
			]]
	]];
	SetAttributes[sendWAEvent, {ReadProtected, Protected, Locked}];
	,
	{SetDelayed::write, Attributes::locked}
];


(*
The default for Import[..., "XML"] is "NormalizeWhitespace" -> True, which is
currently too aggressive when it comes to getting rid of newlines, eg in
plaintext results from the W|A API. So we turn the built-in normalization off,
and instead use a custom recursive filter to strip only the truly superfluous
whitespace from API results.
*)

qImport[url_, "XML"] := Block[{$fetchBodyByteArray = $AlphaQueryBodyByteArrayXML},
	customNormalizeWhitespace[ qImport[url, "XML", "NormalizeWhitespace" -> False] ]
];

qImport[File[str_], args___] := Block[{$AlphaQueryUseURLFetch = False}, qImport[str, args]]

qImport[url_, args___] /; $AlphaQueryUseURLFetch := Catch[Module[{
	$RequestType = Switch[$AlphaQueryMMode,
		"inline","MWAInterpreter",
		"qparse", "Quantity",
		"input", If[!TrueQ[hasFlag],hasFlag=True;$RequestType,"WAEqual"],
		_,$RequestType], fetchdata,
		dataType, dataPattern, dataToString},
	{dataType, dataPattern, dataToString} = If[
		TrueQ[$fetchBodyByteArray],
		{"BodyByteArray", _ByteArray, ByteArrayToString},
		{"Content", _String, Identity}
	];
	sendWAEvent[$RequestType];
	Switch[$AlphaQueryQuiet,
		True, Quiet[Replace[URLFetch[url, {"StatusCode", dataType}], {
			{200, s: dataPattern} :> ImportString[dataToString[s], args],
			_ :> $Failed }]],
		False, Replace[URLFetch[url, {"StatusCode", dataType}], {
			{200, s: dataPattern} :> ImportString[dataToString[s], args],
			{code_, _} :> (Message[WolframAlpha::kbserr, Lookup[URLParse[url], "Domain", url], code]; $Failed),
			_ :> $Failed }],
		"Log", Replace[loggingImport[URLFetch, url, {"StatusCode", dataType}], {
			{200, s: dataPattern} :> ImportString[dataToString[s], args],
			_ :> $Failed }],
		Automatic | _,
		(* Replace the FetchURL::conopen message with the WolframAlpha::conopen message *)
		redirectMessages[
			echoTiming[Tooltip["URLFetch[...]", Defer[URLFetch][url, {"StatusCode", "Content"}]], fetchdata = URLFetch[url, {"StatusCode", dataType}]];
			If[MatchQ[fetchdata, {200, dataPattern}],
				echoOpener["ImportString results", echoTiming["ImportString", ImportString[dataToString @ Last @ fetchdata, args]]],
				If[MatchQ[fetchdata, {Except[200], _}],
					Message[WolframAlpha::kbserr, Lookup[URLParse[url], "Domain", url], First @ fetchdata] ];
				echoOpener["URLFetch results", fetchdata]; $Failed
			],
			Utilities`URLTools`FetchURL::conopen,
			Message[WolframAlpha::conopen]
		]
	]],"WAE"] // dontRefresh

qImport[args___] := Catch[Module[{
	$RequestType = Switch[$AlphaQueryMMode,
		"inline","MWAInterpreter",
		"qparse", "Quantity",
		"input", If[!TrueQ[hasFlag],hasFlag=True;$RequestType,"WAEqual"],
		_,$RequestType]},
	sendWAEvent[$RequestType];
	Switch[$AlphaQueryQuiet,
		True, Quiet[Import[args]],
		False, Import[args],
		"Log", loggingImport[Import, args],
		Automatic | _,
		(* Replace the FetchURL::conopen message with the WolframAlpha::conopen message *)
		redirectMessages[
			echoOpener["Import results", echoTiming[Tooltip["Import[...]", Defer[Import][args]], Import[args]]],
			Utilities`URLTools`FetchURL::conopen,
			Message[WolframAlpha::conopen]
		]
	]],"WAE"] // dontRefresh


qURLFetch[args___] := Catch[Module[{
	$RequestType = Switch[$AlphaQueryMMode,
		"inline","MWAInterpreter",
		"qparse", "Quantity",
		"input", If[!TrueQ[hasFlag],hasFlag=True;$RequestType,"WAEqual"],
		_,$RequestType]},
	sendWAEvent[$RequestType];
	Switch[$AlphaQueryQuiet,
		True, Quiet[URLFetch[args]],
		False, URLFetch[args],
		"Log", loggingImport[URLFetch, args],
		Automatic | _, URLFetch[args] (* FIXME?? *)
	]],"WAE"] // dontRefresh



dontRefresh = Function[{expr}, Refresh[expr, None], HoldFirst]



customNormalizeWhitespace[ XMLObject["Document"][a_, b_, c_] ] := XMLObject["Document"][a, customNormalizeWhitespace[b], c]

customNormalizeWhitespace[ XMLElement[a:("queryresult" | "pod" | "subpod" | "states" | "statelist" | "sources" | "assumptions" | "assumption" | "infos" | "info" | "warnings" | "units" | "definitions" | "notes" | "relatedexamples"), b_, c_List]] := 
	XMLElement[a, b, customNormalizeWhitespace /@ c]

customNormalizeWhitespace[ a_XMLElement] := a

customNormalizeWhitespace[ a_String] := Sequence[]

customNormalizeWhitespace[ other_] := other



SetAttributes[redirectMessages, HoldAll]

redirectMessages[expr_, msgs_, new_] :=
	Module[{result}, Quiet[Check[result = expr, new; result, msgs], msgs]]


loggingImport[head_, url_, args___] := 
	Block[{t0, t1, result},
		t0 = AbsoluteTime[];
		
		writeToLog[
			Column[
				Flatten[{
					Style[ToDate[t0]],
					Defer[head[url, args]],
					If[StringMatchQ[url, "*?*"],
						OpenerView[{"decoded", Column[urldecode /@ StringSplit[url, {"?", "&"}]]}, False],
						{}
					]
				}],
				Dividers -> {False, Center},
				FrameStyle -> Dotted,
				Spacings -> 1.5,
				BaselinePosition -> {1,1}
			],
			CellDingbat -> Which[
				StringMatchQ[url, "*query.jsp*"], TooltipBox[StyleBox["\[FilledCircle]", FontSize -> 18, FontColor -> RGBColor[0,0,1]], "query.jsp"],
				StringMatchQ[url, "*mparse.jsp*"], TooltipBox[StyleBox["\[FilledDiamond]", FontSize -> 18, FontColor -> RGBColor[0,1,0]], "mparse.jsp"],
				StringMatchQ[url, "*recalc.jsp*"], TooltipBox[StyleBox["\[Star]", FontSize -> 36, FontColor -> RGBColor[1,0,0]], "recalc.jsp"],
				StringMatchQ[url, "*wanparse.jsp*"], TooltipBox[StyleBox["\[FilledDiamond]", FontSize -> 18, FontColor -> RGBColor[1, 0.5, 0]], "wanparse.jsp"],
				StringMatchQ[url, "*compute.jsp*"], TooltipBox[StyleBox["\[DoubleStruckCapitalC]", FontSize -> 18, FontColor -> RGBColor[.5,0,.5], FontWeight -> Bold], "compute.jsp"],
				True, TooltipBox[StyleBox["\[EmptyCircle]", FontSize -> 18, FontColor -> RGBColor[0,0,1]], "other"]
			],
			CellMargins -> {{Inherited, Inherited}, {0, Inherited}}
		];
		
		result = head[url, args];
		t1 = AbsoluteTime[];
		
		writeToLog[
			OpenerView[{
				Row[{ByteCount[result], " bytes",
					" in ", t1 - t0, " seconds"}],
				(* Log a human-readable version of ByteArrays along with the original data *)
				Replace[result, ba_ByteArray :> Sequence[ba, ByteArrayToString[ba]], {1}]
				},
				False
			],
			CellMargins -> {{Inherited, Inherited}, {Inherited, 0}}
		];
		
		result
	]


writeToLogWithTimestamp[expr_, opts___] :=
	writeToLog[
		Column[
			Join[{ToDate[AbsoluteTime[]]}, Flatten[{expr}]],
			Dividers -> {False, Center},
			FrameStyle -> Dotted,
			Spacings -> 1.5,
			BaselinePosition -> {1,1}
		], opts]  /; ($AlphaQueryQuiet === "Log")

writeToLog[expr_, opts___] :=
(
	If[Head[$AlphaQueryLogNotebook] =!= NotebookObject || !MemberQ[Last /@ Notebooks[], Last @ $AlphaQueryLogNotebook],
		$AlphaQueryLogNotebook = NotebookPut[
			Notebook[{},
				WindowTitle -> "Alpha Query Log",
				WindowMargins -> {{Automatic,0},{Automatic,0}},
				DockedCells -> {Cell[BoxData[RowBox[{
					ButtonBox["close",
						Appearance -> "DialogBox",
						ButtonFunction :> FrontEndExecute[{
							FrontEnd`NotebookClose[FrontEnd`ButtonNotebook[], Interactive -> False]}]],
					ButtonBox["clear",
						Appearance -> "DialogBox",
						ButtonFunction :> FrontEndExecute[{
							FrontEnd`SelectionMove[FrontEnd`ButtonNotebook[], All, Notebook],
							FrontEnd`NotebookDelete[FrontEnd`ButtonNotebook[]]}]]
					}]], "DockedCell"]},
				Background -> GrayLevel[0.95]
			]
		]
	];
	SelectionMove[$AlphaQueryLogNotebook, After, Notebook];
	NotebookWrite[$AlphaQueryLogNotebook, Cell[BoxData[ToBoxes[expr]], "Output", Background -> White, opts]]
) /; ($AlphaQueryQuiet === "Log")




(* ::Subsection::Closed:: *)
(*AlphaQuery[]*)


doQuerySideEffects[query_String] :=
	If[ToLowerCase[query] === "developer preferences", AlphaQueryPreferences[]; True];

doQuerySideEffects[Typeset`Query["boxes" -> boxes_, ___]] :=
	If[boxes === RowBox[{"developer"," ","preferences"}], AlphaQueryPreferences[]; True];


queryBoxesToQueryString[str_String, fmt_] := str

queryBoxesToQueryString[BoxData[boxes_] | boxes_, fmt_] :=
	First[ MathLink`CallFrontEnd[ FrontEnd`ExportPacket[BoxData[boxes], "InputText"] ] ]


doubleEqualBoxesToTypesetQuery[BoxData[boxes_], fmt_] := doubleEqualBoxesToTypesetQuery[boxes, fmt]

doubleEqualBoxesToTypesetQuery[FormBox[boxes_, _], fmt_] := doubleEqualBoxesToTypesetQuery[boxes, fmt]

doubleEqualBoxesToTypesetQuery[TagBox[boxes_, Typeset`Query, ___], fmt_] := QueryExpand @ Typeset`Query["boxes" -> boxes]

doubleEqualBoxesToTypesetQuery[else_, fmt_] := None



returnNull = Null

returnNull /: MakeBoxes[returnNull, fmt_] := MakeBoxes[Null, fmt]


AlphaIntegration`AlphaQuery[boxes_, fmt_] := 
	Block[{opts = alphaQueryOptions[], query, xml, returnNull, conopen=False, $AlphaQueryMMode = "query"},
		query = doubleEqualBoxesToTypesetQuery[boxes, fmt];
		If[query === None,
			query = StringTrim[queryBoxesToQueryString[boxes, fmt]]
		];
		If[MatchQ[query, "" | Typeset`Query["boxes" -> "", ___]], Return[Null]];
		If[doQuerySideEffects[query], Return[Null]];
		redirectMessages[
			xml = WolframAlpha[query, "ProcessedXML", opts];
			If[mathematicaSessionInfoPermissionDialogIfNecessary[query, xml], xml = WolframAlpha[query, "ProcessedXML", opts]],
			WolframAlpha::conopen,
			conopen = True
		];
		CellPrint[
			If[errorBlobQ[conopen, xml, query, opts],
				formatErrorBlob[conopen, xml, query, opts],
				formatAlphaXMLCell[xml, query, opts]
			]
		];
		returnNull
	]


AlphaIntegration`AlphaQueryInputs[boxes_, fmt_] :=
	Block[{opts = alphaQueryInputsOptions[], query, xml, returnNull},
		query = StringTrim[queryBoxesToQueryString[boxes, fmt]];
		If[query === "", Return[Null]];
		xml = WolframAlpha[query, "RawXML", opts];
		If[ mathematicaSessionInfoPermissionDialogIfNecessary[query, xml], xml = WolframAlpha[query, "RawXML", opts]];
		CellPrint[formatAlphaXMLCell[xml, query, opts]];
		returnNull
	]


formatAlphaXMLCell[xml_, query_, opts___] :=
	Cell[
		BoxData[ToBoxes[formatAlphaXML[xml, query, opts]]],
		"Print",
		If[BoxForm`sufficientVersionQ[11.1], "WolframAlphaFullOutput", CellMargins -> {{20, 10}, {Inherited, Inherited}}]
	]


formatAlphaXML[XMLObject["Document"][docdata_, XMLElement["queryresult", values_, data_], _], query_, opts___] :=
	System`DynamicNamespace[
		"WolframAlphaQueryResults",
		formatQueryResults[values, data, query, resolveOptions[opts]],
		BaseStyle -> {Deployed -> True},
		Editable -> False,
		DeleteWithContents -> True,
		SelectWithContents -> True
	]

formatAlphaXML[other_, query_, opts___] := $Failed


errorBlobQ[True, xml_, query_, opts___] := True;

errorBlobQ[_, XMLObject["Document"][docdata_, XMLElement["queryresult", values_, data:{___, error:XMLElement["error", _, _], ___}], _], query_, opts___] := True

errorBlobQ[___] := False


formatErrorBlob[True, xml_, query_, opts___] := 
	formatErrorBlob[
		cachedFrontEndResource["WAStrings", "NoInternetTitle"],
		cachedFrontEndResource["WAStrings", "NoInternetBody"]
	]

formatErrorBlob[conopen_, XMLObject["Document"][docdata_, XMLElement["queryresult", values_, data:{___, error:XMLElement["error", _, _], ___}], _], query_, opts___] :=
	formatErrorBlob[conopen, error, query, opts]

formatErrorBlob[conopen_, error:XMLElement["error", _, {___, XMLElement["code", _, {code_}], ___}], query_, opts___] := 
	If[DigitQ[code] && 3000 <= ToExpression[code] <= 3999,
		formatErrorBlob[
			cachedFrontEndResource["WAStrings", "BlockedTitle"] <> " (" <> code <> ")",
			errorBlobMessage[code, error]
		],
		formatErrorBlob[
			cachedFrontEndResource["WAStrings", "ErrorTitle"] <> " (" <> code <> ")",
			errorBlobMessage[code, error]
		]
	]

formatErrorBlob[title_, msg_] := 
	Cell[
		BoxData[ToBoxes[
			innerFrame[
				Column[{
					Grid[{{Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "WAErrorIcon"]]], TextCell[title, Bold, StripOnInput -> True]}}],
					TextCell[msg, StripOnInput -> True]
					},
					Spacings -> 1.5
				],
				Background -> GrayLevel[0.96],
				ImageSize -> Automatic,
				FrameMargins -> {{20,20},{15,15}},
				BaseStyle -> {FontFamily -> "Helvetica"}
			]
		]],
		"Print",
		Deployed->True	
	]

formatErrorBlob[___] := $Failed


errorBlobMessage[code_, XMLElement["error", _, {___, XMLElement["mathematicamsg", _, {mathematicamsg_}], ___}]] :=
Block[{params, suffix, result},

	params = Flatten[{
		"error" -> code,
	
		buildid=Internal`$CreationID;
		If[StringQ[buildid], "buildid" -> buildid, {}],
	
		actkey=$ActivationKey;
		If[StringQ[actkey], "actkey" -> actkey, {}],
	
		machineid=$MachineID;
		If[StringQ[machineid], "machineid" -> machineid, {}]
	}];
	
	suffix = StringJoin["?", Riffle[{First[#], "=", Last[#]}& /@ params, "&"]];
	
	result = StringReplace[mathematicamsg, ("$$[" ~~ (text__) ~~ "][" ~~ (url__) ~~ "]$$") :>
		With[{fullurl = StringJoin[url, suffix]}, TraditionalForm[Hyperlink[text, fullurl]]]
	];
	
	Switch[Head[result],
		String, result,
		StringExpression, Row[List @@ result],
		_, result
	]
]

errorBlobMessage[code_, XMLElement["error", _, {___, XMLElement["msg", _, {msg_}], ___}]] :=
	errorBlobMessage[code, XMLElement["error", {}, {XMLElement["mathematicamsg", {}, {msg}]}]]

errorBlobMessage[code_, other_] := "Unknown error"







formatQueryResults[values:{___, "error" -> "true", ___}, data_, query_, opts___] :=
	formatFallThrough["error", values, data, query, opts]


(* "success" -> "false" is handled by the main formatQueryResults rule *)


formatQueryResults[values_, data_List, query_, opts___] := 
Block[{numpods, podvars, auxvars, expandedData, recalculateURL, boxversion = 1},
	numpods = Count[data, XMLElement["pod", _, _]];
	
	(* If we're going to be doing a recalculate, proactively add space for extra pods *)
	recalculateURL = "recalculate" /. values /. "recalculate" -> "";
	If[recalculateURL =!= "", numpods += $AlphaQueryRecalculateSpace; boxversion = 2];
	
	podvars = Thread[ToExpression["Typeset`pod" <> ToString[#], InputForm, Hold]& /@ Range[numpods], Hold];
	auxvars = Thread[ToExpression["Typeset`aux" <> ToString[#], InputForm, Hold]& /@ Range[numpods], Hold];
	If[podvars === {}, podvars = Hold[{}]];
	If[auxvars === {}, auxvars = Hold[{}]];
	If[ TrueQ[(AppearanceElements /. {opts} /. Options[WolframAlpha]) === {"UseInputLeft"}] ||
		MatchQ[Asynchronous /. {opts} /. Options[WolframAlpha], True | All | _?NumericQ],
		expandedData = data,
		expandedData = expandData[data]
	];
	buildDynamicModule[boxversion, values, expandedData, recalculateURL, podvars, auxvars, query, opts]
]


buildDynamicModule[boxversion_, values_, data_List, recalculateURL_, Hold[{podvarseq___}], Hold[{auxvarseq___}], query_, opts___] := 
DynamicModule[{ Typeset`q = query, Typeset`opts = {opts}, Typeset`elements, podvarseq, auxvarseq,
				Typeset`asyncpods, Typeset`nonpods, Typeset`initdone = FrontEnd`SuppressEvaluationHash[False], Typeset`queryinfo,
				Typeset`sessioninfo, Typeset`showpods, Typeset`failedpods, Typeset`chosen, Typeset`open,
				Typeset`newq},
	
	Typeset`queryinfo = values;
	Typeset`sessioninfo = Refresh[{"TimeZone" -> $TimeZone, "Date" -> DateList[], "Line" -> $Line, "SessionID" -> $SessionID}, None];

	Typeset`showpods = Cases[data, XMLElement["pod", _, _]];
	
	Evaluate[Take[{podvarseq}, Length[Typeset`showpods]]] = Typeset`showpods;
	{auxvarseq} = {
		True (* opener state *),
		False (* whether a podstate is actively changing *),
		{False} (* whether each subpod state is actively changing *),
		$AlphaQueryShowLinksByDefault (* whether to show internal links as links *) }& /@ {podvarseq};
	Typeset`nonpods = DeleteCases[data, XMLElement["pod", _, _]];
	
	Typeset`showpods = Range[Length[Typeset`showpods]];
	Typeset`failedpods = Complement[Range[Length[{auxvarseq}]], Typeset`showpods];
	
	If[recalculateURL =!= "",
		(* add a temporary pod whose only purpose is to trigger the display of the async pod animator *)
		Evaluate[First[Cases[{podvarseq}, _Symbol]]] = XMLElement["pod", {"recalculate" -> "true"}, {"recalculate"}];
		AppendTo[Typeset`showpods, First[Typeset`failedpods]];
		Typeset`failedpods = Rest[Typeset`failedpods]
	];
	
	{Typeset`open, Typeset`chosen, Typeset`newq} = {"ExtrusionOpen", "ExtrusionChosen", "NewQuery"} /. allMethodOptions[opts];
	If[ !ListQ[Typeset`chosen], Typeset`chosen = {}];
	If[ !StringQ[Typeset`newq], Typeset`newq = query];
	
	
	Typeset`asyncpods = Cases[
		MapIndexed[Function[{x, y}, {First[y], Hold[x], x}, HoldAllComplete], Unevaluated[{podvarseq}]],
		{index_, Hold[var_], XMLElement["pod", {___, "async" -> url_, ___}, _]} :> {index, Hold[var], url}
	];
	
	Typeset`elements = OptionValue[WolframAlpha, {opts}, AppearanceElements];
	Switch[Typeset`elements,
		None, Typeset`elements = {},
		{__String}, Null,
		All, Typeset`elements = $AllAlphaQueryAppearanceElements,
		Automatic | _, Typeset`elements = $AutomaticAlphaQueryAppearanceElements
	];
	
	Dynamic[
		AlphaIntegration`FormatAlphaResults[Dynamic[{
			boxversion,
			{podvarseq}, {auxvarseq}, Typeset`chosen, Typeset`open, Typeset`elements, Typeset`q, Typeset`opts,
			Typeset`nonpods, Typeset`queryinfo, Typeset`sessioninfo, Typeset`showpods, Typeset`failedpods, Typeset`newq
		}]],
		TrackedSymbols :> {Typeset`showpods, Typeset`failedpods}
	],
	
	Evaluate[
		With[{
			PluginAndPacletCheckPlaceholder = If[
				MemberQ[Flatten[{AppearanceElements /. Flatten[{opts}] /. AppearanceElements -> {}}], "PluginAndPacletCheck"],
				PluginAndPacletCheck[], Null
			]},
			If[boxversion === 1,
				Initialization :> If[
					Not[Typeset`initdone === True] && Not[Typeset`initdone === FrontEnd`SuppressEvaluationHash[True]],
					PluginAndPacletCheckPlaceholder;
					doAsyncUpdates[Hold[{podvarseq}], Typeset`asyncpods, Dynamic[Typeset`failedpods]];
					Typeset`asyncpods = {};
					Typeset`initdone = FrontEnd`SuppressEvaluationHash[True]
				],
				Initialization :> If[
					Not[Typeset`initdone === True] && Not[Typeset`initdone === FrontEnd`SuppressEvaluationHash[True]],
					PluginAndPacletCheckPlaceholder;
					AlphaIntegration`DoAsyncInitialization[Hold[{
						boxversion,
						{podvarseq}, {auxvarseq}, Typeset`chosen, Typeset`open, Typeset`elements, Typeset`q, Typeset`opts,
						Typeset`nonpods, Typeset`queryinfo, Typeset`sessioninfo, Typeset`showpods, Typeset`failedpods, Typeset`newq,
						recalculateURL, Typeset`asyncpods
					}]];
					Typeset`asyncpods = {};
					Typeset`initdone = FrontEnd`SuppressEvaluationHash[True]
				]
			]
		] /. PluginAndPacletCheckRule[]
	],
	SynchronousInitialization -> False

]




(* Version 1 *)

AlphaIntegration`FormatAlphaResults[arg:Dynamic[{1|2, {podvarseq___}, {auxvarseq___}, chosen_, open_, elements_, q_, opts_, nonpods_, qinfo_, info_, showpods_, failedpods_, newq_}]] := 
	If[!MemberQ[elements, "Extrusion"], 
		#,
		extrusionOpener[#, nonpods, Dynamic[q], Dynamic[opts], Dynamic[chosen], Dynamic[open], Dynamic[newq]] 
	]& @
	If[!MemberQ[elements, "CDFWarnings" | "CDFAssumptions"],
		#,
		Column[Flatten[{
			If[!MemberQ[elements, "CDFWarnings"], {}, FormatAllWarnings["CDFWarnings", nonpods, Dynamic[newq], Dynamic[q], Sequence @@ opts]],
			If[!MemberQ[elements, "CDFAssumptions"], {}, FormatAllAssumptions["CDFAssumptions", nonpods, Dynamic[newq], Dynamic[q], Dynamic[opts]]],
			#
			}],
			BaseStyle -> {"DialogStyle", AutoItalicWords -> {"Mathematica"}},
			Spacings -> 2
		]
	]& @ 
	If[!TrueQ["AdditionalInformation" /. allMethodOptions[opts]],
		#,
		Column[Flatten[{
			#,
			FormatAdditionalInformation[arg]
			}],
			BaseStyle -> {"DialogStyle", AutoItalicWords -> {"Mathematica"}},
			Spacings -> 2
		]
	]& @
	outerFrame[
		Column[Flatten[{
			If[!MemberQ[elements, "Warnings"], {}, FormatAllWarnings["Warnings", nonpods, Dynamic[newq], Dynamic[q], Sequence @@ opts]],
			If[!MemberQ[elements, "Assumptions"], {}, FormatAllAssumptions["Assumptions", nonpods, Dynamic[newq], Dynamic[q], Dynamic[opts]]],
			Which[
				elements === {"UseInputLeft"},
				FormatAllMInputs[{podvarseq}],

				MemberQ[elements, "Pods"] && {podvarseq} =!= {},
				FormatAllPods[Dynamic[{podvarseq}], Dynamic[{auxvarseq}], Dynamic[chosen], Dynamic[open], Dynamic[showpods], Dynamic[failedpods], Dynamic[newq], Dynamic[q], Dynamic[opts], elements, info],

				MemberQ[elements, "Unsuccessful"],
				FormatAllUnsuccesses[nonpods, Dynamic[newq], Dynamic[q], Dynamic[opts]],

				True,
				{}
			],
			BrandingStripe[elements, nonpods, Dynamic[q], Dynamic[opts]]
			}],
			BaseStyle -> {"DialogStyle", AutoItalicWords -> {"Mathematica"}},
			Spacings -> If[MemberQ[elements, "ContentPadding"], 1.1, 0.9]
		],
		MemberQ[elements, "ContentPadding"]
	]


(* Version unknown *)

AlphaIntegration`FormatAlphaResults[Dynamic[{unknown_, ___}]] := 
	Framed[
		Grid[{{
			Item[TextCell[Row[{
				"Displaying this content requires a more recent version of the Wolfram System. ",
				Hyperlink["\[RightSkeleton]", $AlphaQueryBaseURLProtocol <> "www.wolfram.com/"]}]],
				Background -> GrayLevel[1],
				Frame -> 1,
				FrameStyle -> LightGray
			]}}],
		BaseStyle -> "DialogStyle",
		RoundingRadius -> 5,
		FrameStyle -> LightGray,
		Background -> GrayLevel[0.965],
		FrameMargins -> 5
	]





(*
The formatting of the main column of frames follows the following conventions.

The outer FrameBox has ImageSize -> Full and RoundingRadius -> 8.

All the inner FrameBoxes have ImageSize -> Full and RoundingRadius -> 5.

The overall effect is something like this:

Framed[Column[Table[
	Framed[i, ImageSize -> Full, RoundingRadius -> 5],
	{i, 10}]], ImageSize -> Full, RoundingRadius -> 8]

Then there are GridBoxes within each inner frame. Those use an ItemSize of Scaled[0.998] or
Scaled[0.499], depending on whether they are full-width or half-width.

Framed[Column[Table[
	Framed[Grid[{{i}}, Alignment -> Left, ItemSize -> Scaled[0.998]], ImageSize -> Full, RoundingRadius -> 5],
	{i, 10}]], ImageSize -> Full, RoundingRadius -> 8]
 
 or
 
Framed[Column[Table[
	Framed[Grid[{{i, i + 1}}, Alignment -> {{Left, Right}, Automatic}, ItemSize -> Scaled[0.499]], ImageSize -> Full, RoundingRadius -> 5],
	{i, 10}]], ImageSize -> Full, RoundingRadius -> 8]

*)


outerFrame[expr_, padding_] := 
	Framed[expr,
		RoundingRadius -> 8,
		FrameStyle -> LightGray,
		Background -> RGBColor[0.913725, 0.913725, 0.913725],
		FrameMargins -> If[padding, 9, 7]
	]


innerFrame[expr_, opts___] := 
	Framed[expr,
		opts,
		RoundingRadius -> 5,
		FrameStyle -> LightGray,
		Background -> RGBColor[1, 1, 1],
		ImageSize -> Full,
		Alignment -> Top
	]


BrandingStripe[elements_, nonpods_, Dynamic[q_], Dynamic[opts_]] := 
Block[{left, right, itemsize},

	Switch[
		MemberQ[elements, #]& /@ {"CDFBrand", "Sources", "Brand"},
		{False, False, False}, left = {}; right = {},
		{False, False, True }, left = {}; right = webBrand[True, {}, Dynamic[q], Dynamic[opts]],
		{False, True,  False}, left = {}; right = webBrand[False, FormatAllSourcesActions[nonpods], Dynamic[q], Dynamic[opts]],
		{False, True,  True }, left = {}; right = webBrand[True, FormatAllSourcesActions[nonpods], Dynamic[q], Dynamic[opts]],
		{True,  False, False}, left = computedByLink[]; right = {},
		{True,  False, True }, left = computedByLink[]; right = webBrand[True, {}, Dynamic[q], Dynamic[opts]],
		{True,  True,  False}, left = computedByLink[]; right = browserSourceInfoButton[AllSourceURLs[nonpods], Dynamic[q], Dynamic[opts]],
		{True,  True,  True }, left = computedByLink[]; right = browserSourceInfoButton[AllSourceURLs[nonpods], Dynamic[q], Dynamic[opts]]
	];		

	If[left === {} && right === {}, Return[{}]];

	If[left =!= {}, left = Item[left, Alignment -> {Left, Bottom}]];
	If[right =!= {}, right = Item[Row[right, "   "], Alignment -> {Right, Bottom}]];	

	itemsize = If[left === {} || right === {}, Scaled[0.998], Scaled[0.499]];
	
	innerFrame[
		Grid[{Flatten[{left, right}]}, ItemSize -> itemsize],
		Background -> None,
		FrameStyle -> None,
		FrameMargins -> {{Automatic, Automatic}, {0,0}}
	]
]


computedByLink[] := 
	Hyperlink[
		Grid[{{Dynamic[FEPrivate`FrontEndResource["WAStrings", "ComputedBy"]]}},
			Alignment -> Baseline,
			BaselinePosition -> {1,1},
			BaseStyle -> {FontColor -> Black},
			ItemSize -> Full,
			Spacings -> {0,0}
		],
		$AlphaQueryBaseURLProtocol <> "www.wolfram.com/mathematica",
		ActiveStyle -> {},
		Alignment -> Left,
		ButtonFunction :> (FrontEndExecute[{NotebookLocate[#2, "OpenInNewWindow" -> CurrentValue["HyperlinkModifierKey"]]}]&)
	]



browserSourceInfoButton[sourceurls:{__}, Dynamic[q_], Dynamic[opts_]] := 
Block[{agent, url},
	agent = Cases[Flatten[{opts}], (Method -> {___, "UserAgentString" -> a_String, ___}) :> a];
	agent = If[Length[agent] === 0, "", First[agent]];
	If[StringMatchQ[agent, "*MSIE *"],
		(* IE does not respond to javascript urls, so we use a plain web link instead *)
		url = StringJoin[
			$AlphaQueryBaseURLProtocol, "www.wolframalpha.com/input/sources.jsp?i=", urlencode[q],
			Map[{"&sources=", #} &, StringReplace[FileNameTake /@ sourceurls, "SourceInformationNotes.html" -> ""]],
			"&back=1"
		],
		(* otherwise, javascript is better *)
		url = StringJoin["javascript:openCdfSources(new Array(", Riffle[{"\"", #, "\""}& /@ sourceurls, ","], "))"]
	];
	{Hyperlink[
		Row[{Dynamic[FEPrivate`FrontEndResource["WAStrings", "SourceInfo"]], " \[RightGuillemet]"}],
		url,
		BaseStyle -> {"Hyperlink", FontColor -> Orange},
		ActiveStyle -> {"HyperlinkActive", FontColor -> Red},
		ButtonNote -> ""
	]}
];


browserSourceInfoButton[_, _, _] := {}





webBrand[brandQ_, sourceactions_, Dynamic[q_], Dynamic[opts_]] :=
Block[{button, menuactions, menu},
	button = If[brandQ,
		Hyperlink[
			Dynamic[RawBoxes[FEPrivate`FrontEndResource["WALocalizableBitmaps", "WolframAlpha"]]],
			$AlphaQueryBaseURLProtocol <> "www.wolframalpha.com",
			Alignment -> Left
		],
		{}
	];
	
	menuactions = If[brandQ,
		Flatten[{
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "Feedback"]] :> (NotebookLocate[{URL[#], None}]&[
				StringJoin[
					"mailto:feedback@wolframalpha.com?",
					(* ExternalService`EncodeString[] works better in testing mail clients than the Java-based urlencode *)
					"subject=", ExternalService`EncodeString["Feedback for Wolfram|Alpha in Mathematica"],
					"&body=",
					With[{q=q, assumptions = Cases[Flatten[{InputAssumptions /. Flatten[{opts}] /. InputAssumptions -> {}}], _String]},
						If[assumptions === {},
							ExternalService`EncodeString[ToString[Unevaluated[WolframAlpha[q]],InputForm]],
							ExternalService`EncodeString[ToString[Unevaluated[WolframAlpha[q, InputAssumptions -> assumptions]],InputForm]]
						]
					]
				]
			]),
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "WebVersion"]] :> (
				Quiet[WolframAlpha[]]; (* trigger autoloading *)
				NotebookLocate[{URL[#], None}]&[
					$AlphaQueryBaseURLProtocol <> "www.wolframalpha.com/input/?i=" <>
					Replace[q, {
						Typeset`Query[_, "json" -> a_String] :> Replace[queryJSONToQueryList[a], {
							{{"t"->0,"v"->plaintext_String}} :> (urlencode[plaintext]),
							_ :> (urlencode[queryJSONToQueryJSON64[a]] <> "&i2d=true")
						}],
						Typeset`Query[_, "json64" -> input_String] :> (urlencode[input] <> "&i2d=true"),
						Typeset`Query[_, "plaintext" -> input_String] :> urlencode[input],
						_ :> urlencode[q]
					}] <> 
					With[{assumptions = Cases[Flatten[{InputAssumptions /. Flatten[{opts}] /. InputAssumptions -> {}}], _String]},
						If[assumptions === {}, "", Map[{"&assumption=", urlencode[#]}&, assumptions]]
					]
				]
			),
			If[sourceactions === {}, {}, Delimiter],
			sourceactions
		}],
		sourceactions
	];
		
	menu = If[menuactions === {}, {},
		Tooltip[
			ActionMenu[
				Dynamic[RawBoxes[FEPrivate`FrontEndResource["FEBitmaps", "CirclePlusIconScalable"]]],
				menuactions,
				Appearance -> None
			],
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "Links"]]
		]
	];
	
	Flatten[{button, menu}]
]



(*
The PluginAndPacletCheck[] utility has to inject its code directly into the Initialization option
of the generated DynamicModule, so that it can run even in very old versions of Mathematica.
*)


PluginAndPacletCheckRule[] :=
	With[{minmac = $MinPluginVersionMac, minwin = $MinPluginVersionWindows, newp = $UpgradePlayerCell, newm = $UpgradeMathematicaCell},
		PluginAndPacletCheck[] :> (
			(* Plugin checks: If the plugin is too old, put up a warning in a docked cell. *)
			If[CurrentValue["PluginEnabled"],
				Block[{versionGreaterEqual, versionMin, versionOK},
		
					versionGreaterEqual[v1:{__Integer}, v2:{__Integer}] :=
						With[{base = 1 + Max[Abs[Flatten[{v1, v2}]]], len = Max[Length /@ {v1, v2}]},
							FromDigits[PadRight[Abs[v1], len], base] >= FromDigits[PadRight[Abs[v2], len], base]
						];
		
					versionMin[versions:{{__Integer}..}] := Fold[If[versionGreaterEqual[#1, #2], #2, #1]&, First[versions], Rest[versions]];
					
					versionOK[] := versionOK[MathLink`CallFrontEnd[FrontEnd`Value["$NotebookOperatingSystem"]]];
					versionOK["MacOSX"] := versionOK[minmac];
					versionOK["Windows"] := versionOK[minwin];
					versionOK[otherOS_String] := True;
					versionOK[targetVersion:{__Integer}] := versionOK[MathLink`CallFrontEnd[FrontEnd`Value[FEPrivate`BrowserPluginVersions[]]], targetVersion];
					versionOK[pluginVersions:{__Rule}, targetVersion:{__Integer}] := versionOK[versionMin[Last /@ pluginVersions], targetVersion];
					versionOK[pluginVersion:{__Integer}, targetVersion:{__Integer}] := versionGreaterEqual[pluginVersion, targetVersion];
					versionOK[other___] := True;	
					
					If[Not[versionOK[]],
						With[{nb = EvaluationNotebook[]},
							SelectionMove[nb, Before, Notebook];
							NotebookWrite[nb, If[MemberQ[$ProductInformation, "ProductIDName" -> "MathematicaPlayer"], newp, newm], All]
						]
					]
				]
			];
			
			(* Paclet checks: Only run once per session. *)
			If[Not @ TrueQ @ $AlphaQueryRanPacletSiteUpdate,
				$AlphaQueryRanPacletSiteUpdate = True;
	
				(* Update the PacletManager index *)
				Needs["PacletManager`"];
				PacletManager`PacletSiteUpdate[PacletManager`$PacletSite];
			
				(* Update the WolframAlphaClient paclet *)
				PacletManager`PacletUpdate["WolframAlphaClient"];
			];
		)
	]


$MinPluginVersionMac = {8, 0, 3};

$MinPluginVersionWindows = {8, 0, 36};


$UpgradePlayerURL = "http://www.wolfram.com/cdf-player/upgrade.html";

$UpgradeMathematicaURL = "http://www.wolfram.com/mathematica/upgrade.html";

$UpgradeWarningIcon = 
GraphicsBox[
  TagBox[RasterBox[CompressedData["
1:eJy1ltdKXFEUhockF3mG3IQ8SvII5gkUktuACQQfwt577713wYIdUcSGoqIo
WLBXLCvzLViHPcdxgqAb/tlt7fWvtvecLzG/on6+CwQCvz8Gf6Ki/36NjY2O
+/4hOPkW9+dHzPvg4HMQn4JgUUQCFRUVEgmVlZUh/UvkARwNDQ1SV1cnzc3N
EdHU1PSi9ZaWFm+/trZW6uvrFeHOYwNobGwM0Ym82caa7YfjtX24gM1Nr42B
y4n+3t5emZ6e1tiYbE1NjSfL3HqAXezD4+o19PX1SU9Pj1RXV6sc8sQbjvX1
dSktLdU1ZC0u4fRgG+eqqqpCYoR8WVmZjI6OysbGhnR0dKjtJSUl0traKhcX
F7K7uytFRUV6Fh3t7e0qb7wuzB/gxgZwHj8PDw9V7+TkpHR3d6sftLOzM/UV
mfHxcZUbHh5We1z91lvtuXG0tYKCApmfn5fHx0fVfXNzI27b29tTv2j7+/sa
R8s3sQLGA395ebm3js0jIyO6T16WlpY8Hhpjg9vGxsbULuOwfDJGj4E5Mm1t
bbKwsCDHx8dq4/X1dVgOP9/y8rLqhqu4uFjzS0ysBuzuwkFO2Id3dXX1if5I
XPf393JyciJHR0eKubk5zQW63HcEW1iDKz8/X+cHBwfPxss/tnZ6eir9/f0a
M/SRE3jogfEyhqezs1POz8+f6HGbn3tra0trPTc3V++A5Z76yM7OVhDXvLw8
9YM6uLy89HQ9PDyEzYnft6mpKUlOTpacnBwpLCxUe+HMzMyU9PR0RVZWlt5B
6ob7gi+3t7chtv+vFqgZ3s+UlBTlMmRkZHg8AB7iZzVN7Lh/fp5wMcMm/CYO
g4ODHoffH+ZpaWmSmpqqvicmJkp8fLzWa6S6pu3s7KgfvCnUwMTEhNaB6YXH
wBx+yxd85JD6IY4rKytyd3cXtiY2NzfVLkDMOM8dokcvOTG4c8bI8F7NzMxo
Pjm/uLioeokN/y3U0dramlxdXemYeHDetd1vv3ED6g551pDHN3h4t8kBdzAh
IUGSkpIU1Bnxsnxw3vQbXA72GWO/jQ0myzuNDVajll/ut/GYPDBOl9/025ze
5WRtdnbW47DzphtZV4erx5U1vS5cPexjo8XU9d2tY9c+et4Av16zI5Jtfnm3
98M4+B9w12zdxu6aC94x3hbgl/WfCzb9ThwYGND/CfuWcMfu3OUzDuOx78HX
Au+w+QsHflHTr8lhcP3p6up6Ew4wNDSkHHxLvRWHgTf5rTnA9vb2i3j+AShu
5SE=
    "], {{0, 31}, {26, 0}}, {0, 255},
    ColorFunction->RGBColor],
   BoxForm`ImageTag["Byte", ColorSpace -> "RGB", Interleaving -> True],
   Selectable->False],
  BaseStyle->"ImageGraphics",
  ImageSizeRaw->{26, 31},
  PlotRange->{{0, 26}, {0, 31}}];

$UpgradePlayerCell :=
Cell[BoxData[ToBoxes[
	Framed[
		Grid[{{
			$UpgradeWarningIcon//RawBoxes,
			Pane[Row[{
					"You've got Wolfram ",
					Style["CDF Player", FontSlant -> "Italic"],
					" installed, but you'll need a newer version for best performance of ",
					Style["Wolfram|Alpha with CDF", FontWeight -> Bold], "."
					}],
				BaseStyle -> {
					FontFamily -> "Helvetica",
					LineIndent -> 0,
					LinebreakAdjustments -> {1., 10, 1, 0, 1}
				}
			],
			Item[
				Hyperlink[
					Style["Upgrade Now \[RightGuillemet]", FontSize -> 14, FontWeight -> Bold],
					$UpgradePlayerURL,
					BaseStyle -> {"Hyperlink", FontColor -> Orange},
					ActiveStyle -> {FontColor -> Red}
				]//TraditionalForm,
				Alignment -> Bottom
			]
			}},
			Alignment -> {Left, Center},
			Spacings -> 1.5
		],
		RoundingRadius -> 8,
		FrameStyle -> RGBColor[0.98, 0.52, 0.33],
		FrameMargins -> {{30, 20}, {20, 20}},
		Background -> RGBColor[1., 0.97, 0.89],
		ImageSize -> Full
	]]],
	"Output",
	CellMargins -> {{4, 4}, {10, 4}},
	FontFamily -> "Helvetica"
];

$UpgradeMathematicaCell :=
Cell[BoxData[ToBoxes[
	Framed[
		Grid[{{
			$UpgradeWarningIcon//RawBoxes,
			Pane[Row[{
					"You've got the ",
					Style["Mathematica", FontSlant -> "Italic"],
					" plugin installed, but you'll need a newer version for best performance of ",
					Style["Wolfram|Alpha with CDF", FontWeight -> Bold], "."
					}],
				BaseStyle -> {
					FontFamily -> "Helvetica",
					LineIndent -> 0,
					LinebreakAdjustments -> {1., 10, 1, 0, 1}
				}
			],
			Item[
				Hyperlink[
					Style["Upgrade Now \[RightGuillemet]", FontSize -> 14, FontWeight -> Bold],
					$UpgradeMathematicaURL,
					BaseStyle -> {"Hyperlink", FontColor -> Orange},
					ActiveStyle -> {FontColor -> Red}
				]//TraditionalForm,
				Alignment -> Bottom
			]
			}},
			Alignment -> {Left, Center},
			Spacings -> 1.5
		],
		RoundingRadius -> 8,
		FrameStyle -> RGBColor[0.98, 0.52, 0.33],
		FrameMargins -> {{30, 20}, {20, 20}},
		Background -> RGBColor[1., 0.97, 0.89],
		ImageSize -> Full
	]]],
	"Output",
	CellMargins -> {{4, 4}, {10, 4}},
	FontFamily -> "Helvetica"
];




AlphaIntegration`DoAsyncInitialization[Hold[{1|2, {podvarseq___}, {auxvarseq___}, chosen_, open_, elements_, q_, opts_, nonpods_, qinfo_, info_, showpods_, failedpods_, newq_, recalculateURL_, asyncpods_}]] :=
Block[{},
	doAsyncUpdates[Hold[{podvarseq}], asyncpods, Dynamic[failedpods]];
	asyncpods = {};
	doAsyncRecalculate[recalculateURL, Hold[{podvarseq}], Hold[qinfo], Hold[nonpods], Hold[asyncpods], Dynamic[showpods], Dynamic[failedpods]];
	doAsyncUpdates[Hold[{podvarseq}], asyncpods, Dynamic[failedpods]];
]



doAsyncRecalculate[recalculateURL_, Hold[{podvarseq___}], Hold[queryinfo_], Hold[nonpods_], Hold[asyncpods_], Dynamic[showpods_], Dynamic[failedpods_]] :=
Block[{xml, queryresultvalues, queryresultdata, oldpods, newpods, n, r},
	CheckAbort[
		If[recalculateURL === "", Return[]];
		
		(* otherwise, there is a recalculate URL *)
		xml = qImport[extendedParams @@ splitURL[recalculateURL], "XML"];

		{queryresultvalues, queryresultdata} = {Part[#, 2], Part[#, 3]}& @
			First[Flatten[{Cases[xml, XMLElement["queryresult", _, _], Infinity], XMLElement["queryresult", {}, {}]}]];
		queryinfo = addRecalculateQueryInfo[queryinfo, queryresultvalues];
		nonpods = addRecalculateNonPods[nonpods, DeleteCases[queryresultdata, XMLElement["pod" | "assumptions" | "warnings", _, _]]];
		
		newpods = Cases[queryresultdata, XMLElement["pod", _, _]];
		If[Length[newpods] === 0,
			(* delete the recalc loading pod *)
			r = Last[showpods];
			showpods = DeleteCases[showpods, r];
			failedpods = Union[Flatten[{r, failedpods}]];
			ClearAll @@ Part[Hold /@ Unevaluated[{podvarseq}], r];
			Return[]
		];
		
		(* otherwise, recalculate did return some pods. Most[] avoids the one showing the async animator *)
		oldpods = Most[Cases[{podvarseq}, _XMLElement]];
		newpods = addRecalculateData[oldpods, newpods];
		n = Length[newpods];

		ClearAll[podvarseq];
		Evaluate[Take[{podvarseq}, n]] = newpods;

		asyncpods = Cases[
			MapIndexed[Function[{x, y}, {First[y], Hold[x], x}, HoldAllComplete], Unevaluated[{podvarseq}]],
			{index_, Hold[var_], XMLElement["pod", {___, "async" -> url_, ___}, _]} :> {index, Hold[var], url}
		];
		showpods = Range[n];
		failedpods = Complement[Range[Length[{podvarseq}]], showpods]
		,
		(* if aborted, make sure to remove the recalc pod showing the async animator *)
		If[
			r = Position[{podvarseq}, XMLElement["pod", {"recalculate" -> "true"}, {"recalculate"}], 1];
			MatchQ[r, {{_Integer}}],
			r = r[[1,1]];
			showpods = DeleteCases[showpods, r];
			failedpods = Union[Flatten[{r, failedpods}]];
			ClearAll @@ Part[Hold /@ Unevaluated[{podvarseq}], r];
		];
	]
]


(* Support for legacy notebooks *)
AlphaIntegration`Dump`doAsyncUpdates[args___] := doAsyncUpdates[args]

doAsyncUpdates[Hold[{podvarseq__}], asyncpods_, Dynamic[failedpods_]] :=
	CheckAbort[
		Scan[expandPod, Thread[Hold[{podvarseq}]]];
		getAsyncPods[asyncpods, Dynamic[failedpods]]
		,
		Scan[markAsyncFailure[#, "aborted", Dynamic[failedpods]]&, asyncpods]
	]


expandPod[Hold[var_]] := Block[{expanded = expandData[var]}, If[var =!= expanded, var = expanded]]


getAsyncPods[asyncpods:{__}, Dynamic[failedpods_]] := 
Block[{indexes, vars, urls},
	{indexes, vars, urls} = Transpose[asyncpods];
	multifetchURL[urls, MapThread[Function[{index, var}, getAsyncPodCallback[index, var, Dynamic[failedpods], ##]&], {indexes, vars}]]
];


getAsyncPodCallback[index_, Hold[var_], Dynamic[failedpods_], url_String, filename_String, exception_] :=
Block[{pods},
	If[ exception === Null,	
		(* multifetchURL indicated success *)
		pods = Cases[qImport[File[filename], "XML"], XMLElement["pod", _, _], Infinity];
		pods = DeleteCases[pods, XMLElement["pod", {___, "error" -> "true", ___}, _]];
		If[pods === {}, markAsyncFailure[{index, Hold[var], url}, "data not found", Dynamic[failedpods]], var = expandData[First[pods]]],
		(* otherwise, multifetchURL indicated failure *)
		markAsyncFailure[{index, Hold[var], url}, "data not found", Dynamic[failedpods]]	
	]
]


markAsyncFailure[{index_, Hold[var_], url_}, msg_, Dynamic[failedpods_]] := 
(
	failedpods = Union[Flatten[{failedpods, index}]];
	If[MatchQ[var, XMLElement["pod", {___, "async" -> _, ___}, _]],
		var = Insert[var, "asyncfailed" -> msg, {2, -1}]
	];
)



expandCells[xml_] := xml /. {
	e:XMLElement["cell", _, {_String}] :> expandCell[e]
}


expandData[xml_] := xml /. {
	e:XMLElement["cell", _, {_String}] :> expandCell[e],
	e:XMLElement["img", values_, _] :> expandImage[e] /; FreeQ[values, "ImportedData"],
	e:XMLElement["sound", values:{___, "type" -> "audio/x-wav", ___}, _] :> expandSound[e] /; FreeQ[values, "ImportedData"]
}



(*
The cell expression string returned by the API will be the result of 
one of these:

	Compress[cellexpr]
		or
	ToString[FullForm[cellexpr]]
*)

expandCell[XMLElement["cell", values_, {str_String}]] :=
Block[{cellexpr, c, s},
	cellexpr = If[Head[#] === Uncompress, c=False; str, c=True; #]& @ Quiet[Uncompress[str]];
	If[StringQ[cellexpr], cellexpr = ToExpression[cellexpr]; s=True, s=False];
	If[Head[cellexpr] =!= Cell, Return[Sequence[]]];
	cellexpr = DeleteCases[
		cellexpr /. TagBox[a__, TagBoxNote -> Except[_String], b___] -> TagBox[a, b],
		((FontSize | FontFamily | NumberSeparator | LineSpacing | PageWidth | ShowCellBracket | CellMargins) -> _) | (Magnification -> 1 | 1.)
	];	
	XMLElement["cell", {"compressed" -> c, "string" -> s}, {cellexpr}]
]


expandImage[XMLElement["img", values_, data_]] :=
Block[{src, importedData},
	src = "src" /. values /. "src" -> $Failed;
	importedData = If[src === $Failed, $Failed, qImport[src]];
	If[Head[importedData] === Image, importedData = Image[importedData, Magnification -> 1]];
	XMLElement["img", Append[values, "ImportedData" -> importedData], data]
]


expandSound[XMLElement["sound", values_, data_]] := 
Block[{url, importedData},
	url = "url" /. values /. "url" -> $Failed;
	importedData = If[url === $Failed, $Failed, qImport[url]];
	XMLElement["sound", Append[values, "ImportedData" -> importedData], data]
]


(* ::Subsubsection::Closed:: *)
(*multifetchURL, Mathematica 9 and later*)


(* MultiFetchURL utility from Todd Gayley *)

(* Notes *)

(*
The MultiFetchURL function takes a list of URLs and a list of callback
functions. It downloads the content of each URL in parallel, and when each one
finishes it calls the callback function. The return value is the list of the
results from the callback functions.

The callback functions are called with three arguments: the original URL, the
filename of the downloaded file, and an argument indicating any error condition.
This error argument will be Null if there was no error, or it will be an integer
HTTP status code (like 403), or a descriptive string (like "Couldn't resolve host name"),
suitable for display to a user if desired.

MultiFetchURL does no work other than performing the download, calling the
callback function, and cleaning up. The callback func has to perform any other work.
Its return value is what MultiFetchURL returns for that URL. MultiFetchURL deletes the
file after the callback function is called.

Note that MultiFetchURL doesn't issue any messages itself, as it's considered
an internal utility in this package. Examining the error info returned by each
task (third argument to the callback function) would a pretty thorough way of
detecting and/or reporting errors, if needed.
*)

$multiFetchTaskData  (* A global that gets downvalues assigned to it, then later cleared. *)

multifetchURL[urls : {__String}, userFunctions_List, socketTimeout : _Integer : 10000] :=
Module[{downloadFilename, task, taskData, tasks, url, userFunction, err, result},
    (* First step is to fire off an async task for each URL. We store data about each task in $multiFetchTaskData. *)
    tasks = 
        Function[{url, userFunction},
            downloadFilename = Close[OpenTemporary[]];
            PreemptProtect[
                (* Use PreemptProtect here to ensure that the assignment to $multiFetchTaskData happens
                   before downloadCallback can fire.
                *)
                task = URLSaveAsynchronous[url, downloadFilename, downloadCallback, BinaryFormat->True, "Progress"->False];
                $multiFetchTaskData[task] = {url, downloadFilename, userFunction, Null}
            ];
            task
        ] @@@ Thread[{urls, userFunctions}];
    (* The list of tasks is gradually replaced by a list of results from the callback functions, as each task finishes. *)
    While[Length[Cases[tasks, _AsynchronousTaskObject]] > 0,
        tasks =
            Function[{taskOrResult},
                If[Head[taskOrResult] === AsynchronousTaskObject,
                    (* tasks disappear from AsynchronousTasks[] when they are finished, so we use that
                       as an indicator that they are complete.
                    *)
                    If[!MemberQ[AsynchronousTasks[], taskOrResult],
                        (* Task is done. Call userFunction and return the result, thus replacing the task in the list of tasks. *)
                        {url, downloadFilename, userFunction, err} = $multiFetchTaskData[taskOrResult];
                        $multiFetchTaskData[taskOrResult] =.;
                        result = userFunction[url, downloadFilename, err];
                        Quiet[DeleteFile[downloadFilename]];
                        result,
                    (* else *)
                        (* download is still in progress; return the task unmodified. *)
                        taskOrResult
                    ],
                (* else *)
                    (* This element of tasks is a result from a previously-called userFunction. *)
                    taskOrResult
                ]
            ] /@ tasks;
        Pause[.05]
    ];
    Clear[taskData];
    tasks
]

downloadCallback[task_, "statuscode", data_] :=
    If[data =!= {200},
        $multiFetchTaskData[task] = ReplacePart[$multiFetchTaskData[task], 4->First[data]]
    ]
downloadCallback[task_, "error", data_] :=
    $multiFetchTaskData[task] = ReplacePart[$multiFetchTaskData[task], 4->First[data]]



(* ::Subsection::Closed:: *)
(*MWACompute[]*)


Options[Internal`MWACompute] = {
	"Asynchronous" -> False,
	"AsynchronousFunction" -> Null,
	"ContextPath" -> {"Internal`MWASymbols`", "System`"},
	"Context" -> "Internal`MWASymbols`Temporary`",
	TimeConstraint -> None,
	"MessageHead" -> Automatic,
	"ConvertMWASymbols" -> True,
	"CacheEntityNames" -> True,
	"Compress" -> True, (* Could be False during Parse since it's mostly encrypted.
	                       However, because Parse should take into account EvalEnv, etc.,
	                       that will require rejiggering below. *)
	"TimeZone" -> None,
	"GeoLocation" -> None,
	"Sources" -> True,
	"UnitSystem" -> None
};


Internal`MWACompute[type_String, input_, OptionsPattern[]] := 
With[{msghead=Replace[OptionValue["MessageHead"], Automatic->EntityValue],
	checkmessages:={MessageName[Utilities`URLTools`FetchURL, "conopen"],
					 MessageName[Utilities`URLTools`FetchURL, "contime"], 
 					 MessageName[Utilities`URLTools`FetchURL, "erropts"],
 					 MessageName[Utilities`URLTools`FetchURL, "httperr"], 
 					 MessageName[Utilities`URLTools`FetchURL, "nofile"],
 					 MessageName[Utilities`URLTools`FetchURL, "nolib"],
 					 MessageName[URLFetch, "invhttp"], 
					 MessageName[General, "offline"],MessageName[General,"nffil"]},
	argsIn = {input, "EvalEnv"->$EvaluationEnvironment,If[TrueQ[OptionValue["Sources"]],"Sources"->True,Unevaluated[Sequence[]]]}},
	Module[{res, argscompressed, tmpcontext
, timeout, args=argsIn},
		tmpcontext = OptionValue["Context"];
		timeout = OptionValue[TimeConstraint];
		If[!TrueQ[timeout > 0], timeout = $AlphaQueryTimeConstraint];
		If[StringQ[tmpcontext] && !StringFreeQ[tmpcontext,"Temporary"], Quiet[Remove@@{tmpcontext<>"*"}]];
		If[TrueQ[OptionValue["ConvertMWASymbols"]], args = args/.$ToMWARules];
		argscompressed = 
		    If[ False === OptionValue["Compress"] && StringQ[input],
		        args = input, (* or have to Uncompress args[[1]], with the right $ContextPath, before recompressing.  Ugh. *)
		        Block[{$ContextPath = OptionValue["ContextPath"], $Context = OptionValue["Context"]},
		            If[ MatchQ[{type, args}, {"PLIParse"|"PLIGrammarDeploy", _HoldComplete}],
		                With[{a=Replace[args, {HoldComplete[actual_], argOpts___} :> Unevaluated[{actual, argOpts}]]}, Compress[a]],
		                Compress[args]
		            ]
		        ]
		    ];
		Block[{$APITimeZone=iTimeZoneToGMTString[OptionValue["TimeZone"]],$APILatLong=iGeoLocationtoLatLong[OptionValue["GeoLocation"]],
			$APIUnitSystem = iVerifyUnitSystem[OptionValue["UnitSystem"]], $APITimeout = timeout},
		res = TimeConstrained[
			Quiet[
				Check[If[
					OptionValue["Asynchronous"] === True,
						AsynchronousFetchMWACompute[type, argscompressed, OptionValue["AsynchronousFunction"]],
						FetchMWACompute[type, argscompressed]
				],
					Message[msghead::conopen, msghead]; $Failed,checkmessages
				],
				checkmessages
			], timeout, Message[msghead::timeout, msghead]; $Failed
		]];
		If[FailureQ[res],
			Return[res, Module];
		];
		res = First[
			processMWAComputeResultList[
				{res},
				OptionValue[{
					"ContextPath",
					"Context",
					"ConvertMWASymbols",
					"Sources",
					"CacheEntityNames"
				}]
			]
		];
		res
	]
]

processMWAComputeResultList[
	result_List,
	{contextPath_, context_, convertMWASymbols_, sources_, cacheEntityNames_}
] := Module[
	{res = result},
	Block[
		{$ContextPath = contextPath, $Context = context},
		res = Map[
			Quiet[
				Check[
					Uncompress[#, HoldComplete],
					$Failed
				]
			] &,
			res
		];
	];
	If[TrueQ[convertMWASymbols],
		res = res /. $FromMWARules;
	];
	If[TrueQ[sources],
		res = noteSourceAndExtractData /@ res;
	];
	If[TrueQ[cacheEntityNames],
		Internal`CacheEntityNames /@ res;
	];
	res
]

Internal`ConvertFromMWASymbols[args_] := ReplaceAll[args, $FromMWARules]

noteSourceAndExtractData[res_] := Replace[
	res, 
	HoldPattern[HoldComplete[{r_, "Sources" -> s_}]] :> (Internal`NoteAlphaSources[s]; HoldComplete[r])
]

$TimeStampDateFormat = {"DayNameShort", ", ", "Day", " ", "MonthNameShort", " ", "Year", " ", "Time", " GMT"};

getTimeStamp[] := Developer`EncodeBase64[DateString[DateList[],$TimeStampDateFormat,TimeZone->0]]

productionServerQ[Automatic] := productionServerQ[getPreference["BaseURL", "Automatic"]/.$Failed->"Production"]
productionServerQ[baseurl_String] := MatchQ[ToLowerCase[baseurl],"production" | "public" | "api" | "automatic" | "english" | "en" | "japanese" | "ja" | "spanish" | "es"]
productionServerQ[___] := False

apiQueryMode[type_,mmode_] := If[MemberQ[{"entity","paclet","utility"}, mmode],
	mmode,
	Switch[type,
		"MWACalculateData","entity",
		"MWAFormulaNameLookup"|"CurrencyConversionMean"|"AstronomyConvenienceFunction", "utility" (*?*),
		"MWANames"|"MWAUnitSystem","utility",
		"MWAThermodynamicData"|"MWAEarthquakeData", "paclet",
		"ElevationData"|"WolframMapper"|"ElevationMapper"|"MWAWolframMapper"|"MWAGeoEntityLookup"|"MWAElevationWebService"|"MWAGeocode", "geofunction",
		"MWAInterpreterEntityTypes"|"MWAInterpreterTypeMetaData", "utility"(*?*),
		"MWASemanticInterpretation"|"MWAInterpreter"|"MWAInterpreterSmartFieldExamples"|"PLIParse"|"PLIGrammarDeploy", "semantic",
		_,mmode
	]]

$DontPUT = SameQ[$LicenseType, "Player"];

getMWAComputeURLAndArgs[type_, args_, opts___] := Catch[
  If[TrueQ[$DontPUT], makeMWAComputeURL[type, args, opts],
   Block[{baseurl, params, timezone, latlong, usys, timeout,
     tstamp, $RequestType = type},
    baseurl = "Server" /. allMethodOptions[opts];
    baseurl = If[MatchQ[type, "MWAGeoEntityLookup" | "MWAWolframMapper"] && productionServerQ[baseurl], 
    	$AlphaQueryBaseURL["Maps"] <> $AlphaComputeJSP, $AlphaQueryBaseURL[baseurl] <> $AlphaComputeJSP]; 
    timezone = $APITimeZone;
    timezone = Switch[timezone, _String, "timezone" -> urlencode[timezone], _, {}];
    latlong = $APILatLong;
    latlong = Switch[latlong, _String, "latlong" -> urlencode[latlong], _, {}];
    tstamp = "ts" -> getTimeStamp[];
    usys = UnitSystemToParam[$APIUnitSystem];
    timeout = "timeout" -> ToString[$APITimeout];
    params = Flatten[{
       "type" -> urlencode[type], 
       "releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]], 
       "patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]], 
       "systemid" -> urlencode[$SystemID], 
       "mclient" -> $AlphaQueryMClient, 
       "mmode" -> apiQueryMode[type, $AlphaQueryMMode], 
       timezone, 
       latlong, 
       tstamp,
       usys,
       timeout}];
    {extendedParams[baseurl, params], args}]], "WAE"]
    

UnitSystemToParam["Imperial"] := "units"->"nonmetric"
UnitSystemToParam["Metric"] := "units"->"metric"
UnitSystemToParam[__] := {}

FetchMWACompute[type_,args_,opts___] := Block[{url,body},Catch[
	url = getMWAComputeURLAndArgs[type,args,opts];
	Switch[url,
		_List,{url,body} = url;sendWAEvent[type];URLFetch[url, "Method" -> "POST", "Headers" -> {"Content-Type" -> "application/mathematica"}, "BodyData" -> body],
		_String, qImport[url, "String"],
		_,$Failed
	], "WAE"]
]

AsynchronousFetchMWACompute[type_, args_, fun_] := Block[{url,body},Catch[
	url = getMWAComputeURLAndArgs[type, args];
	If[ListQ[url],
		{url,body} = url;
		URLFetchAsynchronous[url, fun, "Method" -> "POST", 	"Headers" -> {"Content-Type" -> "application/mathematica"}, "BodyData" -> body]
	];
	$Failed(*return $Failed so synchronous eval doesn't wait around*), "WAE"]
]

makeMWAComputeURL[type_, args_, opts___] := 
	Block[{baseurl, params},
	
		baseurl = "Server" /. allMethodOptions[opts];
		baseurl = If[MatchQ[type,"MWAGeoEntityLookup"|"MWAWolframMapper"] && productionServerQ[baseurl],
			$AlphaQueryBaseURL["Maps"] <> $AlphaComputeJSP,
			$AlphaQueryBaseURL[baseurl] <> $AlphaComputeJSP];
	
		params = Flatten[{
			"type" -> urlencode[type],
			"args" -> urlencode[args],
			"releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]], 
			"patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]], 
			"systemid" -> urlencode[$SystemID], 
			"mclient" -> $AlphaQueryMClient, 
			"mmode" -> $AlphaQueryMMode
		}];

		extendedParams[baseurl, params]
	]

$TimeZoneStringPrefix = "GMT";
  
iTimeZoneToGMTString[offset_] := With[{n = N[offset]},
	If[
		NumberQ[n],
		Switch[
			Sign[n],
			-1, StringJoin[$TimeZoneStringPrefix, StringTrim[ToString[n], "."]],
			0, $TimeZoneStringPrefix,
			1, StringJoin[$TimeZoneStringPrefix, "+", StringTrim[ToString[n], "."]],
			_, None
		],
		None
	]
]
  
iGeoLocationtoLatLong[location_] := 
 With[{coords = location /. GeoPosition[l : {_, _}] :> l},
  If[MatchQ[
    coords, {lat_?NumberQ, 
      long_?NumberQ} /; -90 <= lat <= 90 && -180 <= long <= 180],
   Block[{lat = ToString[First[coords]], 
     long = ToString[Last[coords]]},
    lat <> "," <> long],
   None]
  ]
  
iVerifyUnitSystem[system_String] := If[MatchQ[system,"Imperial"|"Metric"],system, (*TODO: add Message[head::unit, system];*)None]
iVerifyUnitSystem[___] := None

(* Use RuleDelayed to avoid immediate evaluation of Entity/Quantity triggering respective package loads *)
$FromMWARules = {
    Internal`MWASymbols`MWAEntity :> Entity,
    Internal`MWASymbols`MWAEntityClass :> EntityClass,
    Internal`MWASymbols`MWAEntityType :> EntityType,
    Internal`MWASymbols`MWAProperty :> EntityProperty,
    Internal`MWASymbols`MWAPropertyClass :> EntityPropertyClass,
    Internal`MWASymbols`MWAData :> EntityValue,
    Internal`MWASymbols`MWAQuantity :> Quantity,
    Internal`MWASymbols`MWADateObject :> DateObject,
    Internal`MWASymbols`MWATimeObject :> TimeObject,
    Internal`MWASymbols`MWAGeoVariant :> GeoVariant,
    Internal`MWASymbols`MWADateRange :> Interval
};

$ToMWARules = Replace[
	DeleteCases[$FromMWARules, _ :> Interval], (* don't include the Interval rewrite rule *)
	(a_ :> b_) :> (HoldPattern[b] :> a), {1}];


(* ::Subsection::Closed:: *)
(*ParallelMWACompute[]*)


divideIntoBatches[list_List,maxsize_Integer] := With[{n=Length[list]},
	Block[{steps = Range[1, n, maxsize], pairs},
	pairs = {#, # + maxsize - 1} & /@ steps;
	Take[list,#]& /@ ReplacePart[pairs, -1 -> {pairs[[-1, 1]], n}
	]
]]
divideIntoBatches[___] := $Failed

Options[Internal`ParallelMWACompute] = Options[Internal`MWACompute];

$MaxAsyncCalls = 30;
Internal`ParallelMWACompute[type_String, argsIn_List, opts : OptionsPattern[]] /; 
And[$PMWARec =!= True, Length[argsIn] > $MaxAsyncCalls] := Block[{$PMWARec=True},
	Join@@Map[Internal`ParallelMWACompute[type,#,opts]&, divideIntoBatches[argsIn,$MaxAsyncCalls]]]
	
Internal`ParallelMWACompute[type_String, argsIn_List, opts : OptionsPattern[]] := Catch[
	With[
		{
			msghead = Replace[OptionValue["MessageHead"], Automatic -> EntityValue],
			checkmessages := {
				MessageName[Utilities`URLTools`FetchURL, "conopen"],
				MessageName[Utilities`URLTools`FetchURL, "contime"],
				MessageName[Utilities`URLTools`FetchURL, "erropts"],
				MessageName[Utilities`URLTools`FetchURL, "httperr"],
				MessageName[Utilities`URLTools`FetchURL, "nofile"],
				MessageName[Utilities`URLTools`FetchURL, "nolib"],
				MessageName[URLFetch, "invhttp"],
				MessageName[General, "offline"],
				MessageName[General, "nffil"]
			}
		},
		Module[
			{res, argscompressed, tmpcontext, timeout, args = {#, "EvalEnv" -> $EvaluationEnvironment} &/@ argsIn, urls, df, data, tasks},
			data[__] = {};
			tmpcontext = OptionValue["Context"];
			timeout = OptionValue[TimeConstraint];
			If[! TrueQ[timeout > 0], timeout = $AlphaQueryTimeConstraint];
			If[StringQ[tmpcontext] && ! StringFreeQ[tmpcontext, "Temporary"],
				Quiet[Remove @@ {tmpcontext <> "*"}];
			];
			If[TrueQ[OptionValue["ConvertMWASymbols"]], args = args /. $ToMWARules];
			argscompressed = Block[
				{$ContextPath = OptionValue["ContextPath"], $Context = OptionValue["Context"]},
				Compress /@ args
			];
			Block[
				{
					$APITimeZone = iTimeZoneToGMTString[OptionValue["TimeZone"]],
					$APILatLong=iGeoLocationtoLatLong[OptionValue["GeoLocation"]],
					$APIUnitSystem = iVerifyUnitSystem[OptionValue["UnitSystem"]],
					$APITimeout = timeout
				},
				urls = getMWAComputeURLAndArgs[type, #, opts] & /@ argscompressed;
			];
			(df[#] := Function[{asyncObj, eventType, document},
				Switch[eventType,
					"data", data[#] = document,
					"error", data[#] = ToCharacterCode["Missing[\"RetrievalFailure\"]"],
					_, Null
				]
			]) & /@ urls;
			sendWAEvent[type];
			Quiet[
				Check[
					tasks = Map[AsynchFetch[#, df[#]] &, urls];
					TimeConstrained[
						Quiet[Scan[WaitAsynchronousTask, tasks], {WaitAsynchronousTask::asyncobj}];,
						timeout,
						Message[msghead::timeout, msghead]
					],
					$Failed,
					checkmessages
				],
				checkmessages
			];
			res = FromCharacterCode[data[#]] & /@ urls;
			res = Replace[res, {x_} :> x, {1}];
			res = processMWAComputeResultList[
				res,
				OptionValue[{
					"ContextPath",
					"Context",
					"ConvertMWASymbols",
					"Sources",
					"CacheEntityNames"
				}]
			];
			Clear[df]; (*see bug 59377*)
			res
		]
	],
	$tag | "WAE"
]
    
AsynchFetch[url_String,fun_] := URLFetchAsynchronous[url,fun]
AsynchFetch[{url_String,body_String},fun_] := URLFetchAsynchronous[url,fun,"Method" -> "POST", 	"Headers" -> {"Content-Type" -> "application/mathematica"}, "BodyData"->body]
AsynchFetch[___] := $Failed


(* ::Subsection::Closed:: *)
(*ImageEditingQuery[]*)


(*
Special value for ImageEditingQuery are used to get the category listing.
*)


AlphaIntegration`ImageEditingQuery["ImageEditing NestedCategoryListing"] :=
	If[TrueQ[$MInterfaceUseCachedCategories],
		Uncompress @ Last @ $CategoryCacheDefault,
		(* otherwise, try grabbing updates from W|A *)
		UpdateCategoryCacheIfNecessary[];
		GetCategoryMenus[]
	]



(*
We may want to remove all but the last form of ImageEditingQuery once the real form
is hooked in and working. But for now, the others are useful.
*)


AlphaIntegration`ImageEditingQuery[boxes_] := 
	AlphaIntegration`ImageEditingQuery[boxes, Thumbnail @ ExampleData[{"TestImage", "Lena"}], StandardForm]


AlphaIntegration`ImageEditingQuery[boxes_, fmt_] := 
	AlphaIntegration`ImageEditingQuery[boxes, Thumbnail @ ExampleData[{"TestImage", "Lena"}], fmt]


AlphaIntegration`ImageEditingQuery[boxes_, imageThumbnail_, fmt_] := 
	AlphaIntegration`ImageEditingQuery[boxes, imageThumbnail, imageThumbnail, fmt]


AlphaIntegration`ImageEditingQuery[boxes_, imageThumbnail_, image_, fmt_] := 
	AlphaIntegration`ImageEditingQuery[Dynamic[{thing1, thing2}], boxes, imageThumbnail, image, fmt]


AlphaIntegration`ImageEditingQuery[Dynamic[ievars_], boxes_, imageThumbnail_, image_, fmt_] := 
	Block[{opts = {}, query, result, $AlphaQueryMMode = "query"},
		If[boxes === None, Return @ 
			Pane[Animator[Appearance -> "Necklace"], ImageSize -> Full, Alignment -> Center]
		];
		query = StringTrim[queryBoxesToQueryString[boxes, fmt]];
		If[query === "", Return[Null]];
		If[doQuerySideEffects[query], Return[Null]];

		(* FIXME: redirectMessages? *)
		
		result = WolframAlpha[query, "RawImageEditingQuery", opts];

		(* FIXME: errorBlob? *)
		Switch[result,
			{Hold[_], Hold[{_, _, _}]},
			formatImageEditor[Dynamic[ievars], result, imageThumbnail, image, query, opts],
			(*
				If there's no interface version, but there is an minput with exactly the
				right kind of placeholder, construct a trivial manipulate
			*)
			{Hold[expr_], Hold[Null]} /; (Cases[Hold[expr], _Placeholder, Infinity] === {Placeholder["image"]}),
			formatImageEditor[
				Dynamic[ievars],
				Replace[result, {Hold[expr_], Hold[Null]} :> {Hold[expr], Hold[{expr, {}, {}}]}],
				imageThumbnail, image, query, opts],
			(* $Failed indicated that the network failed *)
			$Failed,
			Grid[{{
				Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "WAErrorIcon"]]],
				Style["This feature requires internet connectivity. Please check your network connection.", Gray]
			}}],
			(* otherwise, there's no interface to show *)
			_, Style["\"" <> query <> "\" is not currently supported", Gray]
		]

	]



formatImageEditor[Dynamic[ievars_], {Hold[minput_], Hold[interactiveVersion_]}, imageThumbnail_, image_, query_, opts___] := 
	Module[{manipulate, variables, initializers, inputLabel, newInput, previewImage},
		
		(* 
			TextRecognize and possibly other functions should apply to the full size
			image, not the thumbnail. For now, we make that distinction here in the
			client, and only for TextRecognize. But moving forward, that distinction
			really should be made in the W|A return value from minterface.jsp, say 
			with a new Placeholder type, Placeholder["originalImage"].
		*)
		previewImage = If[
			FreeQ[Hold[interactiveVersion], HoldPattern @ TextRecognize[Placeholder["image"]]],
			imageThumbnail,
			image
		];
		
		manipulate = Hold[interactiveVersion] /. Placeholder["image"] -> previewImage;
		manipulate = interactiveVersionToManipulate[manipulate];
		{variables, initializers} = processManipulate[manipulate];

		inputLabel = Defer[minput] /.
			Replace[variables, {Hold[var_] :> (HoldPattern[var] :> Dynamic[var]), _ :> Sequence[]}, {1}];
		
		newInput = Hold[minput] /. Placeholder["image"] -> image;
		
		assembleImageEditor[Dynamic[ievars], initializers, newInput, inputLabel, manipulate, query]
	]


interactiveVersionToManipulate[Hold[{body_, {controls___}, options___}]] :=
	Manipulate[
		Column[{
			Item[Pane[body, FrameMargins -> {{10,10},{10,0}}], ItemSize -> Fit], 
			Pane["ImageEditorButtonColumn", FrameMargins -> {{0,0},{0,10}}]
			},
			Alignment -> Center,
			Dividers -> {{True, {False}}, Center},
			FrameStyle -> RGBColor[179/255., 179/255., 179/255.]
		],
		"ImageEditorDynamicLabel",
		"",
		controls,
		AppearanceElements -> {},
		ControlPlacement -> Left,
		Deployed -> True,
		Paneled -> False,
		FrameMargins -> 0,
		Evaluate[Sequence @@ Flatten[{options}]]
	]


assembleImageEditor[Dynamic[ievars_], Hold[{inits___}], Hold[newInput_], dynamicLabel_, Manipulate[args___], query_] :=
	Manipulate[args] /. {
		"ImageEditorDynamicLabel" -> 
			Style[dynamicLabel, "StandardForm", FontSize -> 12, ShowStringCharacters -> True],
		"ImageEditorButtonColumn" ->
			DynamicModule[{Typeset`evaluating = False, Typeset`canceled = False},
				Column[{
					Button["Cancel", 
						Typeset`canceled = True;
						ievars = {"More", None},
						Appearance :> FEPrivate`FrontEndResource["FEExpressions", "GrayButtonNinePatchAppearance"],
						FrameMargins -> 0,
						ImageSize -> 87,
						BaseStyle -> {FontColor -> Black, Bold, "DialogStyle"},
						DefaultBaseStyle -> {},
						Method -> "Preemptive"],
					Button[
						Dynamic[If[TrueQ @@ {Typeset`evaluating},
							Row[{"Applying", Animator[Appearance -> "Ellipsis"]}],
							"Apply"
						]],
						Module[{eval},
							Typeset`evaluating = True;
							eval = newInput;
							If[StringQ[eval], eval = ToString[eval, InputForm]];
							If[Typeset`canceled =!= True,
								FrontEnd`Private`ImageEditingReplaceImage[ButtonNotebook[], eval];
								FrontEnd`Private`ImageEditingAddToRecentActionsMenu[query];
							];
							Typeset`evaluating = False;
						],
						Appearance :> FEPrivate`FrontEndResource["FEExpressions", "OrangeButtonNinePatchAppearance"],
						FrameMargins -> 0,
						ImageSize -> 87,
						BaseStyle -> {FontColor -> White, Bold, "DialogStyle"},
						DefaultBaseStyle -> {},
						Method -> "Queued"]
					}],
					Deinitialization :> (Typeset`canceled = True),
					SynchronousInitialization -> True
			]
		}



processManipulate[HoldPattern[Manipulate][_, params___?Manipulate`Dump`validParameterOrOtherArgument, ___?Manipulate`Dump`manipulateOptionQ]] := 
Module[{list, variables, initializers},
	list = variableAndInitializer /@ Unevaluated[{params}];
	variables = Flatten[First /@ list];
	initializers = Flatten[Last /@ list];
	initializers = If[initializers === {}, Hold[{}], Thread[initializers, Hold]];
	{variables, initializers}
]

SetAttributes[variableAndInitializer, HoldAllComplete];
variableAndInitializer[expr_] := {{}, {}} /; Not[Manipulate`Dump`validParameter[expr]]
variableAndInitializer[{{var_Symbol, init_, ___}, ___}] := {Hold[var], Hold[var=init]}
variableAndInitializer[{var_Symbol, {val_, ___}}] := {Hold[var], Hold[var=val]}
variableAndInitializer[{var_Symbol, {val1_, val2_}, {_, _}}] := {Hold[var], Hold[var={val1,val2}]}
variableAndInitializer[{var_Symbol, {val1_, val2_}, {_, _}, {_, _}}] := {Hold[var], Hold[var={val1,val2}]}
variableAndInitializer[{var_Symbol, color_?Manipulate`Dump`colorQ}] := {Hold[var], Hold[var=color]}
variableAndInitializer[{var_Symbol, -Infinity, Infinity, ___}] := {Hold[var], Hold[var=0]}
variableAndInitializer[{var_Symbol, -Infinity, max_, ___}] := {Hold[var], Hold[var=max]}
variableAndInitializer[{var_Symbol, min_, ___}] := {Hold[var], Hold[var=min]}
variableAndInitializer[{var_Symbol, ___}] := {Hold[var], Hold[var=Null]}
variableAndInitializer[ _String -> expr_] := variableAndInitializer[expr]
variableAndInitializer[other_] := {{}, {}}


(* ::Subsection::Closed:: *)
(*Cache management for Image Editor*)


(* ::Subsubsection::Closed:: *)
(*XML to Menu / MenuItem utilities*)


ImageEditorCategoryURL[opts___] :=
	WolframAlpha["ImageProcessingOperations.xml", "ExampleURL", opts]



DownloadNewCategoryXML[] :=
Module[{url, xml},
	url = ImageEditorCategoryURL[];
	xml = qImport[url, "XML"];
	xml = Flatten[Cases[xml, XMLElement["example", _, list_] :> list, \[Infinity]]];
	xml
];


CategoryXMLToCategoryMenus[xml_] := 
Module[{result},
	result = xml //.
		{a___, XMLElement["caption", _, {caption_}], XMLElement["input", _, {input_}], b___} :>
			{a, XMLElement["input", {"label" -> StringTrim[caption]}, {StringTrim[input]}], b} //.
		{
			XMLElement["section-title", _, {category_}] :> ("Category" -> category),
			XMLElement["section-subtitle", _, {subcategory_}] :> {"Subcategory" -> subcategory},
			XMLElement["input", {"label" -> input_}, {input_}] :> (MenuItem[input]),
			XMLElement["input", {"label" -> label_}, {input_}] :> (MenuItem[label, input]),
			XMLElement[other_, _, _] :> Sequence[]
		};
	categoriesToMenus[result]
]


categoriesToMenus[expr_] := 
Block[{c = 0, result},
	result = SplitBy[expr, (If[MatchQ[#, "Category" -> _], ++c]; c)&];
	result = Map[
		If[MatchQ[#, {"Category" -> _, __}], Menu[#[[1,2]], Rest[#]], #]&,
		result
	];
	result /. Menu[label_, list_List] :> Menu[label, Flatten[subcategoriesToMenus[Flatten[list]]]]
]


subcategoriesToMenus[list_] := 
Block[{c = 0, result},
	result = SplitBy[list, (If[MatchQ[#, "Subcategory" -> _], ++c]; c)&];
	Map[
		If[MatchQ[#, {"Subcategory" -> _, __}], Menu[#[[1,2]], Rest[#]], #]&,
		result
	]
]


(* ::Subsubsection::Closed:: *)
(*"CategoryCache" utilities*)


GetCategoryMenus[] := Uncompress @ Last @ GetCleanCategoryCache[]


GetCleanCategoryCache[] :=
Module[{cache},
	cache = getCategoryCache[];
	(* If no cache exists, create one with the version cached in WolframAlphaClient.m *)
	If[!MatchQ[cache, {_String?DigitQ, _String}],
		cache = $CategoryCacheDefault;
		setCategoryCache[cache]
	];
	cache
]


getCategoryCache[] := CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "InterfaceSettings", "ImageEditingToolbar", "CategoryCache"}, None];

setCategoryCache[val_] := (CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "InterfaceSettings", "ImageEditingToolbar", "CategoryCache"}] = val);


$CategoryCacheUpdateInterval = 60 * 60 * 24 * 7;


UpdateCategoryCacheIfNecessary[] :=
Module[{now, then, newmenus, oldcache, timestamp, compressedstr, newcache},
	oldcache = {timestamp, compressedstr} = GetCleanCategoryCache[];

	(* perform this check no more than once per session *)
	If[$AlreadyCheckedCategoryCacheTimestampThisSession, Return[oldcache]];
	$AlreadyCheckedCategoryCacheTimestampThisSession = True;

	If[DigitQ[timestamp], then = ToExpression[timestamp], then = 0];
	now = Round[AbsoluteTime[]];
	If[now - then < $CategoryCacheUpdateInterval, (* don't update *) Return[oldcache]];

	(* Otherwise, attempt an update *)
	newmenus = Quiet[CategoryXMLToCategoryMenus[DownloadNewCategoryXML[]]];

	If[MatchQ[newmenus, {__Menu}],
		(* If the update contains good info, store it *)
		newcache = {ToString[now, InputForm], Compress[newmenus]};
		setCategoryCache[newcache];
		newcache,
		(* Otherwise, stick with the old info *)
		oldcache
	]
]



(* MInterface cache utilities *)


$MInterfaceCacheUpdateInterval = 60 * 60 * 24 * 7


getMInterfaceCache[] := Uncompress @ CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "InterfaceSettings", "ImageEditingToolbar", "MInterfaceCache"}, Compress @ {}];

setMInterfaceCache[val_] := (CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "InterfaceSettings", "ImageEditingToolbar", "MInterfaceCache"}] = Compress[val]);


(* remove entries that are too old *)
cleanMInterfaceCache[] := 
Module[{now, list},
	now = Round[AbsoluteTime[]];
	list = getMInterfaceCache[];
	list = Cases[list, _[_, {then_String?DigitQ, _String}] /; (now - ToExpression[then] < $MInterfaceCacheUpdateInterval)];
	setMInterfaceCache[list]
]

(* insert a new entry for the given result *)
updateFrontEndCache[str_String, resultstr_String] :=
Module[{list, new},
	list = getMInterfaceCache[];
	list = DeleteCases[list, _[str, _]];
	new = str -> {ToString[Round[AbsoluteTime[]]], resultstr};
	PrependTo[list, new];
	setMInterfaceCache[list]
]


frontendCacheRawMInterface[str_] :=
Module[{result},
	(* By running cleanMInterfaceCache first, we don't have to check the date again afterwards *)
	cleanMInterfaceCache[];
	result = Cases[getMInterfaceCache[], _[str, {date_, result_}] :> result];
	If[Length[result] > 0, First[result], $Failed]
]







(* ::Subsubsection::Closed:: *)
(*Fall-through caches*)


$CategoryCacheDefault = {
"0",
"1:eJylVm1T1DAQRkBAARUVX8Yv8Qfcj1AQhxlwGG5GP+fSbRtJszVJT+\
DXu9vrwZU2LTN+uclent1kd5990s8zvEy319bW/\
Cb9nGkf0vWldQ62mr6ixREadOLCoQLvtc3uIT0OH+\
4cjnJpLZhVx82W4xO2dhrH0wDFdJ8MbefgglAcw/dgDsmA6+\
CkCkKaMpdCLQ7qwb7nf6CUTgZYRFyC+yK/I6MEV1RdcDvHA84A5pTbl+\
R35UMBNvgHya13wnOZjc7yAK5OtFkLXcgMevBbZCTSXRF8724ZRXNEWd9GGL6ar51qtFj8\
35PxSzL+VNIGfQvLirdT5bCndZBvaQqK0twa6eHeouZaCqg9eiBvyEBtRCk1HW2zOJIvSE\
1wCqWJo5ikUMywJlkcxrmU6Lnmg1fzaKTTtzJotEtklwBNVSxRRAEzIN0YGItntJgWiCHn\
OdgYqSFvzUzlerZ2yShAWpFqQ5lEUrgAh1ZOzqXRV0tk90LHYFE/\
ejALSPTQudyFmTY0ao6aFYVxqF8aLHUhiuFJDBgozJw6sWhEbxbPuazEjxJsV5i6cWulW8\
AjCSSg0M7RzGF10lqNPwEZKgfiGAIxg66WPn3ESCQ1WkCSQUzVGsgt9U4ot2BzH3b/\
HlvIa7rmCEZbxnQlY/ceE0hRp8xKVA9LzJefQsYMrxsxqP7Mva/\
aSncjxnw6DSctAOt1uBEhd+\
BzNAkVYLpT08ryRPbp3ifWPWrDnAXMBqycaND1ydPXtM8v0eQneBDtQHfX/\
siLygQ9uYSMyda6/NhwcO+UqVhYWH8UFiXa+knoYt+SkSKlFx4F/\
cvT5HNIxqGUvaVWtqHtNFlNz1DRWJ3UwzT2lr+o/7YgXeMwrv58ROZkQgMe4vPNqO+\
yIoYPCQp35UyWRiqGjeO5CjTcBVqtxLBGcssywAKCG8UeLBXX5zoN/\
TrEmB9oTbtY2yPFYvWi6YwfXQPk9bCYPkaXiRJUFmLdcLV9kJa+MxKRwLwtu/9Xwmhnut+\
O5+hKGn3MNPN05duxzbyuELCR8PPDU896UrQiLbciH2akuZ4dDzuOzU7EDxcvT48frr5JH\
T9l0Pf7NTsRIgYsJ7kkwXbSehKSIlLtGYaARRv6D+UGL7E="
};


$MInterfaceCacheDefault = {
"0",
"1:eJztnVtv2zYUx4NtwIZdsPuGvbF52jCnSIwVKAa0QOOkaYCmFytIhzkeSku0zVYiNZJ\
20xoehmEY9jDsYU973EfcRxipi+NEsi07KFNpByhcSufoiJT+P1IiKeZKhze719bW1uQb+\
ucular7WrrVHPjEeU8nKBsSoZDLfS6k8/j1tbXR+sE+\
CwdqHW3cROsNY7hHeliR1gMfu6TPfY+I1vHxOg1wj+j/2+\
31GlrfZ4oI7Co6JEdESMpZHGC0322hK3zo15BOSeoRdOMG2tJb5vBbUpKg45PWaBRtH+\
KnpKWCsF5Dt3x9CGsnjqkh2T/aQl8jVkMbW+P2eNxeEG1WsHp+\
tNStwYOQyyTCKPbnkXnU4EwJ7usTRSW7jX1JakhfDK4vp4+\
f69RYuyX7D8WARLncec5wQN3W6TU5DaTP/3DAFSVMtfgz6ql+\
DdXbUVCPDvV1E3FQfeliM9ow6S0Tdyo7NbR59VqclRC7VCVZ2Yw822fzEOVA2/W/\
6RjmJkXn0ftNOinMlrmfOu2TroqOqCc7BO31VezUxB7l2wOlONvGIj7hJHB0He9xRsbn9t\
ZzdqeXIDbo0PuMKop9+gIrLS707U30ZRwxuTGR/\
xjdQKNZOjXRC6hZ304qpGpFGtihAWFGzXL2Ae3xV2ODwK4QXDS4RyLlb6Z7DoiU2jXGYX1\
8DsJPdYKcKIMOwn7Yx8jtY8aI73QyMN4y9kZsBhqBxpLTWEzOdnH83GyREAtdRcStYsqjd\
EZrua2jk7ivRuTKOf1MJ0IigkE2o3/nN+\
OaoA5lpNXkSmf3rhZOq2ABoFKBSqUklcqFlW63vjHP4L65uESgKKTzXQbeu7EdWnyAs+\
RwLlSyXfje1QkPi6cT9o4y7O1EZkAP0Cs5eouEbJ+8KBTC3pOBVDk9T7GsIyvgB/\
iVHL9CarbL4Ps68eMAM0VfkLQD+K/8N8eHidvMnNTQN/r+\
UtUngrKeOTASIPAJfJaDz4vJ3H7jKUlIMSLdLnGV8++sxtNrJa/D0SjP2Y3ZZdS/ewI/\
ly72T/c097bj9Gjzav36dSOmreh3DJAD5OWA3CoVduuEj3WCUx+\
FmOoqTNdNSdXwW37VsBtZ55f2PvUfJNHMPqAcKC8P5SsI3P7Tt9vHwuXYT2H9eXVYG0koI\
BVIrR6p0+q2i+kHZnpG0OFSTjWqv6zO6W4aC0AFUKsH6hl52x9J1bIzA6kJptCZBbj+\
L3EtU2eWeXGV3MciLXEC7++rt7HOVDhoZoHb0nC7ssLtImsSHX8gnGaG0W29G0ZrAcSSgz\
hfxnZpe0cnAoIZ6lJfo+N4GegOtPV2ZJxTa9SBPqCvHPQV1LP959QHRHCGNw50+\
Z6mOPoZHGOvyGlBKQBJQLIcSC6hafu9PgHx6Gn72M9pH40dWkjAsSo4Fla0/\
cGSDvWxIchPeTzJviSmLosLEKkAuAQuy8Hl0sq231g+\
0gIgYnZjGduhsQQoqwJlYUXbhdF8v6240g3lEAsal29mk3loHI9SP6AT6KwKnUsr2y6lb5\
qtPhYhYTlfZjuxBTp4AMOSY7hQyfbfJD3icjbk/pAkX2f/\
kT8lYGfiN6feGI30Xds0Nyu63/DRCpBZEjIvoPFLWE2BKOIqRLwekc4PGV53PVMUNW9+\
A4AJYJYDzCJitr+WYELgCyI4ckU8GVc63exU2MQGOAKOlcCxqKDtd7kmSAb4RAd1jrPjk/\
gEKAQKK0FhAS1fHoCUzQCQMgAQAKwGgIu1bBfA183KBK7IWT33kJyopn6x7bF5n6m8nNVz\
34qmKjAzi5443+dMUYhNUCFAhVDyCmGxlO3WBx/phFn0e+OISIImDD7JvqFqJ+\
MDLAKLFWGxuKTtdxy5/sB8PG0+BXWN8JgWgsxZPyj6uNTkvTE5oDHxn5M/\
gBQgLQekKwrcLrKf6ESXCyLVOWL/nE1slLPb6UFALVBbQWpXELl9cp+ZacKyT7xpcn+\
dTe6j1B+gBWgrCO1y+rbPq8sZI646y+s/s3k94CLUGeE96mJ/qkyLH/\
yBXqC3bPReRO32V93sCX1Zdf5mf8G9l3jAPHuAtSqwLqFp+\
0ju4YGUU99w5yCZeACSgGRlkCyuabtIfmH+XjgOdWzD5Hk4f8r+gcLUFygFSqtG6Sritv+\
C2sci4Iy6aHqpMJUh9U7iBkuGAaRVgnRJXdsfXe0RHhAlzgE6zD7npn5AKBBaJUKXFbZdR\
D9MF9mUfdpVs9dhMJl3jAssXQR8VorPpZVtF9C3DaB00nB28ubwQ3MJOFYGx0JyvgQG8ck\
cBvEJMAgMVobBQnKG9W6BRCDxZT+cvrLr3bo8CHRR5eSlkeXMCkpcAElAsipILiNq+\
6OXUmHmYeEhjwzPrq2ZHb10Et+d1BUwBUyrgukq4obREcAUMIXREZhgAIACoMev/\
gQDs5R8MD0HH3nmD1HoMzqPM4TuJCbgErgsP5eF1Gz/\
gfYsjURwc8K8BXFjC7AILJafxSJivmwUeUgYZb0cFO/HFkARUCw/ikXEfNkouj6X+\
Sg2YgugCCiWH8UiYrY/2VXxcKOPFVICM9nlIsgZtzzk4R2sDlMPwBFwLD+Oy4jafi9rh+\
vrFpwjM9vLuh25AZwAZ7XgXFLXL5nP/wCX6bHc"
};


defaultCacheRawMInterface[str_] := str /. Uncompress[Last @ $MInterfaceCacheDefault] /. (str -> "$Failed")



(*
The following global settings effectively disable the live cache updating
features, except for queries which are not in the cache to begin with. Best
thinking for now is to update these caches by updating WolframAlphaClient.m
directly. Thus, the caches above are where the buck stops.
*)

$MInterfaceUseCachedCategories = True;

$MInterfaceUseCachedQueriesFirst = True;

$MInterfaceFallbackToWeb = True;



(* ::Subsection::Closed:: *)
(*FallThroughs*)


suggestionsButtonOpener[content_, type:("CDFNoResults" | "CDFReinterpret")] := 
	DynamicModule[{Typeset`tip = False, Typeset`display},
		Typeset`display = content /. suggestionsDialogButtonPlaceholder[] :> 
			Tooltip[
				Button[
					Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "QuestionMarkSmallIcon"]]],
					Typeset`tip = Not[Typeset`tip],
					Appearance -> None,
					BaseStyle -> {ShowStringCharacters -> False}
				],
				cachedFrontEndResource["WAStrings", "SuggestionsTooltip"]
			];
		
		PaneSelector[{
			False -> Dynamic[Typeset`display],
			True ->
				Column[{
					Dynamic[Typeset`display],
					Style[
						If[type === "CDFReinterpret",
							Column[{
								FrontEndResource["WAStrings", "SuggestionsTitleReinterpret"],
								FrontEndResource["WAStrings", "SuggestionsBody"]
								}
							],
							FrontEndResource["WAStrings", "SuggestionsBody"]
						],
						FontColor -> Gray
					]},
					Dividers -> Center,
					FrameStyle -> LightGray,
					ItemSize -> Scaled[0.998],
					Spacings -> 3
				]
			},
			Dynamic[Typeset`tip],
			ImageSize -> Automatic
		]
	]


suggestionsDialogButton[type_, args___] := 
	Tooltip[
		Button[
			Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "QuestionMarkSmallIcon"]]],
			Quiet[WolframAlpha[]]; (* trigger autoloading *)
			openSuggestionsDialog[type, args],
			Appearance -> None,
			BaseStyle -> {ShowStringCharacters -> False}
		],
		cachedFrontEndResource["WAStrings", "SuggestionsTooltip"]
	]


openSuggestionsDialog[type_, query_, args___] := 
MessageDialog[
	Grid[{
		{"  ",""},
		{"", TextCell[
			Switch[type,
				"Reinterpret", cachedFrontEndResource["WAStrings", "SuggestionsTitleReinterpret"],
				"NoTranslations", cachedFrontEndResource["WAStrings", "SuggestionsTitleNoTranslations"],
				"NoResults" | _, cachedFrontEndResource["WAStrings", "SuggestionsTitleNoResults"]
			], "Text", Bold]
		},
		{"", ""},
		Switch[type,
			"NoTranslations",
			Sequence @@ {
				{"", TextCell[cachedFrontEndResource["WAStrings", "SuggestionsBodyNoTranslations"], "Text"]},
				{"", Button[
					Hyperlink[cachedFrontEndResource["WAStrings", "SuggestionsBodyTryIt"]], 
					Block[{nb},
						nb = CreateDocument[TextCell[query, "WolframAlphaLong", FormatType -> "TextForm"]];
						FrontEndTokenExecute[nb, "EvaluateNotebook"]
					],
					ImageSize -> Automatic,
					Appearance -> None,
					BaseStyle -> {},
					DefaultBaseStyle -> {}
				]}
			},
			_,
			{"", TextCell[cachedFrontEndResource["WAStrings", "SuggestionsBody"], "Text"]}
		]
		},
		Alignment -> Left
	],
	{
		cachedFrontEndResource["WAStrings", "SuggestionsFeedbackButton"] :> sendFeedback[query, args]
	},
	WindowSize -> {500, FitAll},
	WindowTitle -> cachedFrontEndResource["WAStrings", "SuggestionsWindowTitle"]
]


sendFeedback[args___] :=
	NotebookLocate[{URL[#], None}]&[
		StringJoin[
			"mailto:feedback@wolframalpha.com?",
			(* ExternalService`EncodeString[] works better in testing mail clients than the Java-based urlencode *)
			"subject=", ExternalService`EncodeString["Feedback for Wolfram|Alpha in Mathematica"],
			"&body=", ExternalService`EncodeString[ToString[Unevaluated[WolframAlpha[args]],InputForm]]
		]
	]


formatFallThrough["error", values_, data_, query_, opts___] := (
	Message[WolframAlpha::interr,
		Row[Riffle[Cases[data, error:XMLElement["error", _, _] :> formatErrorString[error]], ";"]]
	];
	$Failed
)


formatErrorString[error:XMLElement["error", _, {___, XMLElement["code", _, {code_}], ___}]] :=
	Row[{errorBlobMessage[code, error], " (", code, ")"}]


formatErrorString[XMLElement["error", _, {___, XMLElement["msg", _, {msg_}], ___}]] := msg


formatErrorString[XMLElement["error", _, other_]] := "unknown error"



formatFallThroughElement[XMLElement["warnings", _, data_], q_, opts___] := formatFallThroughElement[#, q, opts]& /@ data


formatFallThroughElement[XMLElement["relatedexamples", _, data_], q_, opts___] := formatFallThroughElement[#, q, opts]& /@ data


formatFallThroughElement[XMLElement["didyoumeans", _, data_], q_, opts___] := formatFallThroughElement[#, q, opts]& /@ data


(* "spellcheck" elements are handled as a warning, not as a fall-through *)
(* formatFallThroughElement[XMLElement["spellcheck", {___, "text"-> text_, ___}, _], q_, opts___] := text *)


formatFallThroughElement[XMLElement["relatedexample", values_, data_], q_, opts___] :=
	Row[Flatten[{
		"\[Bullet] Related example: ",
		replaceQueryResultsButton["input" /. values, "input" /. values, opts],
		If[("desc" /. values) === "", {}, {" (", "desc" /. values, ")"}]
	}]]


formatFallThroughElement[XMLElement["didyoumean", values_, {suggestion_}], q_, opts___] :=
	Row[{"Did you mean ", replaceQueryResultsButton[suggestion, suggestion, opts], "?"}]


formatFallThroughElement[XMLElement["futuretopic", values_, data_], q_, opts___] :=
	Row[{"\[Bullet] ", "topic" /. values, ":  ", "msg" /. values}]


formatFallThroughElement[XMLElement["examplepage", values_, data_], q_, opts___] :=
Block[{url, xml},
	url = "url" /. values;
	url = StringReplace[url, "-content.html" -> ".xml"];
	xml = qImport[url, "XML"];
	formatExample[Flatten[Cases[xml, XMLElement["example", _, a_] :> a, Infinity]], q, opts]
]

formatExample[list_List, q_, opts___] := If[# === {}, {}, Column[Last /@ #, Spacings -> {Automatic, First /@ #}]]&[ formatExampleElement[#, q, opts]& /@ list ]

formatExampleElement[XMLElement["category", _, {category_}], q_, opts___] := {2, Style[category, "Subsection", FontColor -> Red]}

formatExampleElement[XMLElement["section-title", {}, {___, XMLElement["link", {"ref" -> ref_}, {title_String}], ___}], q_, opts___] :=
	{2, replaceQueryResultsButton[Row[{Style[title, Bold, FontColor -> Black], " \[RightGuillemet]"}], title, opts]}

formatExampleElement[XMLElement["section-title", {}, {title_String}], q_, opts___] := {2, Style[title, Bold, FontColor -> Black]}

formatExampleElement[XMLElement["caption", {}, {caption_}], q_, opts___] := {1.2, caption}

formatExampleElement[XMLElement["input", {}, {input_}], q_, opts___] := {0.4, replaceQueryResultsButton[input <> " \[RightGuillemet]", input, opts]}

formatExampleElement[other_, q_, opts___] := Sequence[]


formatFallThroughElement[XMLElement["languagemsg", values:{___, "english" -> english_, ___}, _], q_, opts___] :=
	If[MatchQ[values, {___, "other" -> _String, ___}],
		Grid[{{"\[Bullet] ", Style["other" /. values, FontSize -> Larger]}, {"", english}}, Alignment -> Left],
		Row[{"\[Bullet] ", english}]
	]


formatFallThroughElement[other_, q_, opts___] := {}



replaceQueryResultsButton[label_, query_, opts___] := 
Button[
	Style[Mouseover[Style[label, FontColor -> Orange], Style[label, FontColor -> Red]], "DialogStyle"],
	Quiet[WolframAlpha[]]; (* trigger autoloading *)
	If[MemberQ[AppearanceElements /. Flatten[{opts}] /. AppearanceElements -> $AutomaticAlphaQueryAppearanceElements, "CDFWarnings"],
		NotebookLocate[{URL[WolframAlpha[query, "CDFURL", opts]], None}, "OpenInNewWindow" -> CurrentValue["HyperlinkModifierKey"]],
		replaceQueryResults[ButtonNotebook[], query, opts]
	],
	Appearance -> None,
	Method -> "Queued"
]


replaceQueryResults[nb_, query_, opts___] :=
	MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[
		FE`BoxReference[MathLink`CallFrontEnd[FrontEnd`Value[FEPrivate`Self[]]], {FE`Parent["WolframAlphaQueryResults"]}],
		formatAlphaXML[WolframAlpha[query, "ProcessedXML", opts], query, opts] // ToBoxes,
		AutoScroll -> False
	]];


(* ::Subsection::Closed:: *)
(*success -> false*)


FormatAllUnsuccesses[nonpods_List, Dynamic[newq_], Dynamic[q_], Dynamic[opts_]] := 
	Block[{list},
		list = Flatten[formatFallThroughElement[#, q, Sequence @@ Flatten[{opts}]]& /@ nonpods];
		{innerFrame[
			Column[
				If[list === {},
					If[MemberQ[AppearanceElements /. Flatten[{opts}] /. AppearanceElements -> $AutomaticAlphaQueryAppearanceElements, "CDFWarnings"],
						{suggestionsButtonOpener[
							Row[{
								Style[Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoResults"]], Italic],
								"  ",
								suggestionsDialogButtonPlaceholder[]
							}],
							"CDFNoResults"
						]},
						{Row[{
							Style[Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoResults"]], Italic],
							"  ",
							suggestionsDialogButton["NoResults", q]
						}]}
					],
					list
				],
				Dividers -> Center,
				FrameStyle -> LightGray,
				BaseStyle -> {"DialogStyle", FontColor -> Gray},
				Spacings -> 2,
				ItemSize -> Scaled[0.998]
			],
			FrameMargins -> 15,
			Background -> queryBlobBackground[Dynamic[q], Dynamic[newq], GrayLevel[0.965]]
		]}
	]


(* ::Subsection::Closed:: *)
(*warnings*)


FormatAllWarnings[ type_, data_, args___] := formatWarnings[#, type, args]& /@ data

formatWarnings[XMLElement["warnings", values_, data_], type_, args___] := formatWarning[#, type, args]& /@ data

formatWarnings[other_, ___] := {}

formatWarning[XMLElement["spellcheck", values:{___, "text" -> text_, ___}, data_], type_, Dynamic[newq_], Dynamic[q_], opts___] := 
	warningFrame[type, text, Dynamic[newq], Dynamic[q]]

formatWarning[XMLElement["delimiters", values:{___, "text" -> text_, ___}, data_], type_, Dynamic[newq_], Dynamic[q_], opts___] :=
	warningFrame[type, text, Dynamic[newq], Dynamic[q]]

formatWarning[XMLElement["translation", values:{___, "text" -> text_, ___}, data_], type_, Dynamic[newq_], Dynamic[q_], opts___] := 
	warningFrame[type, text, Dynamic[newq], Dynamic[q]]

formatWarning[XMLElement["reinterpret", values:{___, "new" -> new_, ___}, data_], type_, Dynamic[newq_], Dynamic[q_], opts___] := 
	Block[{alternatives, rows},
		rows = {If[type === "CDFWarnings",
			suggestionsButtonOpener[
				Grid[{{
						Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "ReinterpretationIndicator"]]],
						Row[{Style["text" /. values, FontColor -> Gray], " ", Style[new, Bold]}],
						suggestionsDialogButtonPlaceholder[]
					}},
					Alignment -> Left,
					Spacings -> 0.5
				],
				"CDFReinterpret"
			],
			Grid[{{
					Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "ReinterpretationIndicator"]]],
					Row[{Style["text" /. values, FontColor -> Gray], " ", Style[new, Bold]}],
					suggestionsDialogButton["Reinterpret", q]
				}},
				Alignment -> Left,
				Spacings -> 0.5
			]
		]};

		alternatives = Cases[data, XMLElement["alternative", _, {alternative_String}] :> alternative];
		alternatives = replaceQueryResultsButton[#, #, opts]& /@ alternatives;
		If[alternatives =!= {},
			AppendTo[rows, Row[Flatten[{Style["More interpretations:", FontColor -> Gray], " ", Riffle[alternatives, $verticalBar1]}]]]
		];
		
		warningFrame[
			type,
			Grid[List /@ rows,
				Alignment -> Left,
				Dividers -> Center,
				FrameStyle -> LightGray,
				ItemSize -> Scaled[0.998],
				Spacings -> {0,2}
			],
			Dynamic[newq],
			Dynamic[q]
		]
	]


formatWarning[unknownWarning_, args___] := {}


warningFrame["Warnings", expr_, Dynamic[newq_], Dynamic[q_]] :=
	innerFrame[expr,
		FrameStyle -> Orange,
		FrameMargins -> 15,
		Background -> queryBlobBackground[Dynamic[q], Dynamic[newq], GrayLevel[0.965]]
	]

warningFrame["CDFWarnings", expr_, Dynamic[newq_], Dynamic[q_]] :=
	innerFrame[expr,
		FrameStyle -> RGBColor[0.98, 0.52, 0.33],
		FrontEnd`BoxFrame -> 2,
		FrameMargins -> 15,
		Background -> queryBlobBackground[Dynamic[q], Dynamic[newq], GrayLevel[0.965]],
		RoundingRadius -> 8
	]



(* ::Subsection::Closed:: *)
(*pods*)


(* The DynamicModule created by FormatIndependentPod is based closely on the one built by buildDynamicModule[] *)
FormatIndependentPod[pod:XMLElement["pod", _, _], query_, opts___] :=
	DynamicModule[{
			Typeset`q = query, Typeset`opts = {opts}, Typeset`elements,
			Typeset`sessioninfo, Typeset`chosen, Typeset`open,
			Typeset`newq, Typeset`pod, Typeset`aux},
				
		Typeset`pod = pod;
		Typeset`aux = {True, False, {False}, True};

		Typeset`sessioninfo = Refresh[{"TimeZone" -> $TimeZone, "Date" -> DateList[], "Line" -> $Line, "SessionID" -> $SessionID}, None];

		{Typeset`open, Typeset`chosen, Typeset`newq} = {"ExtrusionOpen", "ExtrusionChosen", "NewQuery"} /. allMethodOptions[opts];
		If[ !ListQ[Typeset`chosen], Typeset`chosen = {}];
		If[ !StringQ[Typeset`newq], Typeset`newq = query];

		Typeset`elements = OptionValue[WolframAlpha, {opts}, AppearanceElements];
		Switch[Typeset`elements,
			None, Typeset`elements = {},
			{__String}, Null,
			All, Typeset`elements = $AllAlphaQueryAppearanceElements,
			Automatic | _, Typeset`elements = $AutomaticAlphaQueryAppearanceElements
		];

		Dynamic[
			FormatPod[Typeset`pod, Dynamic[Typeset`pod], Dynamic[Typeset`aux], Dynamic[Typeset`chosen], Dynamic[Typeset`open], Dynamic[Typeset`newq], Dynamic[Typeset`q], Dynamic[Typeset`opts], Typeset`elements, Typeset`sessioninfo],
			TrackedSymbols :> {Typeset`pod}
		],

		BaseStyle -> {"DialogStyle", AutoItalicWords -> {"Mathematica"}},
		Initialization :> Quiet[WolframAlpha[]] (* trigger autoloading *)
	]


FormatAllPods[Dynamic[{podvarseq___}], Dynamic[{auxvarseq___}], Dynamic[chosen_], Dynamic[open_], Dynamic[showpods_], Dynamic[failedpods_], Dynamic[newq_], Dynamic[q_], Dynamic[opts_], elements_, info_] := 
	Extract[#, List /@ Complement[showpods, failedpods]]& @ 
	MapThread[
		Function[{var, aux},
			Dynamic[
				FormatPod[var, Dynamic[var], Dynamic[aux], Dynamic[chosen], Dynamic[open], Dynamic[newq], Dynamic[q], Dynamic[opts], elements, info],
				TrackedSymbols :> {var}
			],
			HoldAllComplete
		],
		Unevaluated[{{podvarseq}, {auxvarseq}}]
	]


(* async failed *)
FormatPod[e:XMLElement["pod", values:{___, "asyncfailed" -> msg_}, data_], Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[newq_], Dynamic[q_], Dynamic[opts_], elements_, info_] :=
	innerFrame[
		Grid[{{
			Item[podTitle[values, ""], Alignment -> Left],
			Item[Style[msg, Italic, FontColor -> LightGray], Alignment -> Right]
			}},
			Alignment -> Top,
			ItemSize -> Scaled[0.499]
		]
	]


(* async content is available *)
FormatPod[e:XMLElement["pod", values:{___, "async" -> url_, ___}, data_], Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[newq_], Dynamic[q_], Dynamic[opts_], elements_, info_] :=
	asyncPodAnimator[]


(* recalculate content is available *)
FormatPod[e:XMLElement["pod", values:{___, "recalculate" -> "true", ___}, data:{"recalculate"}], Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[newq_], Dynamic[q_], Dynamic[opts_], elements_, info_] :=
	asyncPodAnimator[]


(* main case: no async content *)
FormatPod[XMLElement["pod", values_, data_], Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[newq_], Dynamic[q_], Dynamic[opts_], elements_, info_] :=
Block[{numberOfSubpods, subpods, podinfos, poddefinitions, podnotes, $subpodCounter = 0, subpodsAreSteps, steptypes},
	numberOfSubpods = Count[data, XMLElement["subpod", _, _]];
	subpods = formatSubpods["id" /. values, "title" /. values, data, Dynamic[var], Dynamic[aux], Dynamic[chosen], Dynamic[open], Dynamic[q], Dynamic[opts], Hold[$subpodCounter], numberOfSubpods, info, AppearanceElements /. opts /. Options[WolframAlpha]];
	podinfos = Item[#, Alignment -> Right]& /@ podInfos[data];
	poddefinitions = Item[#, Alignment -> Right]& /@ podDefinitions[data];
	podnotes = Item[#, Alignment -> Right]& /@ podNotes[data];
	(* If there are both "Definitions" and "Notes" elements, combine them into a single "Definitions and notes" element *)	
	If[poddefinitions =!= {} && podnotes =!= {},
		poddefinitions = Item[podDefinitionsAndNotes[data], Alignment -> Right];
		podnotes = {}
	];

	If[subpods === {}, Return[""]];
	
	subpodsAreSteps = MemberQ[values, "stepbystepcontenttype" -> "StepByStep"];
	
	dogEarPanel[
		innerFrame[
			podOpener[{
				Column[Flatten[{
					Grid[{{
						Item[
							Pane[
								podOpenerButton[podTitle[values, ":"], Dynamic[aux[[1]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "HidePod"]], elements],
								ImageMargins -> {{0,0},{0,1}}
							],
							Alignment -> Left
						],
						Item[
							PaneSelector[{
								True -> necklaceAnimator[$necklaceAnimatorSize],
								False -> podStates["id" /. values, data, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts]]
								},
								Dynamic[TrueQ[aux[[2]]]],
								ImageSize -> All,
								Alignment -> {Right, Top}
							],
							Alignment -> Right
						]
						}},
						Alignment -> Top,
						(* If there are no pod state or sound buttons, give more space to the title *)
						ItemSize -> If[MemberQ[data, XMLElement["states"|"sounds", _, _]], Scaled[0.499], {{Scaled[0.9], Scaled[0.098]}, Automatic}]
					],
					If[subpodsAreSteps && Length[subpods] > 1,
						steptypes = Cases[data, XMLElement["subpod", _, subdata_] :> Replace[subdata, {
							{___, XMLElement["stepbystepcontenttype", _, {type_String}], ___} :> type,
							_ :> "SBSStep"
						}]];
						formatSubpodsAsSteps[subpods, steptypes],
						subpods
					],
					podinfos,
					poddefinitions,
					podnotes
					}],
					ItemSize -> Scaled[0.998],
					(*
					If there's exactly one subpod, don't add delimiters, even if there are infos.
					If there's more than one subpod, add a delimiter between each one, and before the infos if any.
					*)
					Dividers -> (
						subpods = Length[Flatten[subpods]];
						podinfos = Length[Flatten[{podinfos, poddefinitions, podnotes}]];
						If[subpods === 1,
							None,
							{None, Table[i -> True, {i, 3, 3 + subpods -2 + Sign[podinfos]}]}
						]
					),
					Spacings -> If[MemberQ[elements, "ContentPadding"],
						{Automatic, Flatten[{Automatic, .9, If[subpods === 1, {}, Table[1.1, {subpods}]]}]},
						Automatic
					],
					FrameStyle -> GrayLevel[0.95]
				],
				podTitle[values, " \[RightGuillemet]"]
				},
				Dynamic[aux[[1]]],
				elements
			],
			Background -> queryBlobBackground[Dynamic[q], Dynamic[newq], GrayLevel[0.965]],
			FrameMargins -> If[MemberQ[elements, "ContentPadding"], {{13,10},{6, 6}}, Automatic]
		],
		"id" /. values,
		data,
		Dynamic[var],
		Dynamic[aux],
		Dynamic[q],
		Dynamic[opts],
		elements
	]
]


podOpener[{pod_, title_}, Dynamic[open_], elements_] := 
	If[MemberQ[elements, "PodCornerCurl"],
		pod,
		PaneSelector[{
				True -> pod,
				False -> Column[{podOpenerButton[title, Dynamic[open], Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowPod"]], elements]}, ItemSize -> Scaled[0.998]]
			},
			Dynamic[open],
			ImageSize -> Automatic
		]
	]


podOpenerButton[label_, Dynamic[open_], tip_, elements_] :=
	If[MemberQ[elements, "PodCornerCurl"],
		label,
		Tooltip[
			Button[
				label,
				Quiet[WolframAlpha[]]; (* trigger autoloading *)
				open = Not @ open,
				Appearance -> None,
				BaseStyle -> {},
				DefaultBaseStyle -> {},
				ContentPadding -> False
			],
			tip
		]
	]


asyncPodAnimator[] := 
DynamicModule[{Typeset`linecolor, Typeset`darkfill, Typeset`lightfill, Typeset`roundedcorners, Typeset`v = 0},
	Typeset`linecolor = RGBColor[0.8161745632104982, 0.8162966353856718, 0.8161135271229114];
	Typeset`darkfill = RGBColor[0.873182269016556, 0.8733043411917296, 0.8731212329289693];
	Typeset`lightfill = RGBColor[0.9752040894178683, 0.9753566796368353, 0.9751277943083848]; 
	Typeset`roundedcorners = {{0., 9.}, {2., 8.464101615137753}, {3.4641016151377544, 7.}, {4., 5.},
		{3.4641016151377544, 3.}, {2., 1.5358983848622456}, {0., 1.}, {-2., 1.5358983848622456},
		{-3.4641016151377544, 3.}, {-4., 5.}, {-3.4641016151377544, 7.}, {-2., 8.464101615137753},
		{0., 9.}
	};

	Grid[{{
		
		Animator[Dynamic[Typeset`v], {0, 1}, AppearanceElements -> {}, ImageSize -> {1, 1}, DefaultDuration -> 15],
		
		Graphics[{Typeset`darkfill, EdgeForm[Typeset`linecolor], Polygon[Typeset`roundedcorners]},
			PlotRange -> {{-4.5, 1}, {-1, 9}},
			ImageSize -> {Automatic, 9},
			ImagePadding -> 0,
			PlotRangePadding -> 0
		],
  
		Graphics[{
			EdgeForm[],
			Typeset`darkfill, Rectangle[{0, 0}, {Dynamic[Typeset`v], 9}],
			Typeset`lightfill, Rectangle[{Dynamic[Typeset`v], 0}, {1, 9}],
			Typeset`linecolor, Line[{{0, 1}, {1, 1}}], Line[{{0, 9}, {1, 9}}]
			},
			ImageSize -> {Full, 9},
			AspectRatio -> Full,
			ImagePadding -> 0,
			PlotRangePadding -> 0
		],
  
		Graphics[{Typeset`lightfill, EdgeForm[Typeset`linecolor], Polygon[Typeset`roundedcorners]},
			PlotRange -> {{0, 4.5}, {-1, 9}},
			ImageSize -> {Automatic, 9},
			ImagePadding -> 0,
			PlotRangePadding -> 0
		]

		}},
		Spacings -> 0
	]
]


$necklaceAnimatorSize = 13;

necklaceAnimator[size_] := Animator[Appearance -> "Necklace", ImageSize -> size]





dogEarClosed[] := Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "DogEarClosed"]]]


dogEarOpen[podid_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_]] :=
	With[{linksQ = TrueQ @ aux[[4]],
			label = If[TrueQ @ aux[[4]],
						Dynamic[FEPrivate`FrontEndResource["WAStrings", "HideLinks"]],
						Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowLinks"]]
					]
		},
		Overlay[{
			Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "DogEarOpen"]]],
			Pane[
				Row[Flatten[{
					Button[
						Style[Mouseover[ Style[label, FontColor -> Orange], Style[label, FontColor -> Red] ], "DialogStyle"],
						(Quiet[WolframAlpha[]]; (* trigger autoloading *) changeLinkState[Dynamic[var], Dynamic[aux], Not @ linksQ]),
						Appearance -> None,
						BaseStyle -> {},
						DefaultBaseStyle -> {},
						Method -> "Queued"
					],
					cdfPodMenu[podid, data, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts]]
					}],
					$verticalBar2
				],
				ImageMargins -> {{15, 3}, {3, 0}}
			]
			},
			{1,2},
			2,
			Alignment -> {Left, Bottom}
		]
	]


dogEarPanel[innerframe_, podid_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], elements_] :=
	Block[{},
		If[Not @ MemberQ[elements, "PodCornerCurl"], Return[innerframe]];
				
		DynamicModule[{Typeset`layers = {1}, Typeset`currentlayer = 1},
			EventHandler[
				Overlay[{
					innerframe,
					dogEarClosed[],
					dogEarOpen[podid, data, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts]]
					},
					Dynamic[Typeset`layers],
					Dynamic[Typeset`currentlayer],
					ImageSize -> All,
					Alignment -> Bottom
				],
				{
					"MouseEntered" :> (FEPrivate`Set[Typeset`layers, {1,2}]; FEPrivate`Set[Typeset`currentlayer, 1]),
					"MouseMoved" :>
						FEPrivate`Which[
							FEPrivate`And[
								FEPrivate`Less[FEPrivate`Part[FrontEnd`CurrentValue[{"MousePosition","EventHandlerAbsolute"}],1], 24],
								FEPrivate`Less[FEPrivate`Part[FrontEnd`CurrentValue[{"MousePosition","EventHandlerAbsolute"}],2], 25]
							],
							FEPrivate`Set[Typeset`layers, {1,3}]; FEPrivate`Set[Typeset`currentlayer, 3],
							FEPrivate`Greater[FEPrivate`Part[FrontEnd`CurrentValue[{"MousePosition","EventHandlerAbsolute"}],2], 50],
							FEPrivate`Set[Typeset`layers, {1,2}]; FEPrivate`Set[Typeset`currentlayer, 1]
						],
					"MouseExited" :> (FEPrivate`Set[Typeset`layers, {1}]; FEPrivate`Set[Typeset`currentlayer, 1])
				}
			]
		]
	]






FormatAllMInputs[{podvarseq___}] := 
	Block[{list},
		list = Flatten[Map[formatPodMInputsOnly, {podvarseq}]];
		If[ list === {}, {Style["No inputs were found", Italic, Gray]}, list]
	]


formatPodMInputsOnly[XMLElement["pod", values_, data_]] :=
Block[{subpods},
	subpods = Flatten[Cases[data, XMLElement["subpod", _, d_] :> d]];
	subpods = Flatten[formatSubpodMInputLeft /@ subpods];

	If[subpods === {}, Return[{}]];
	
	innerFrame[
		Column[Flatten[{
			podTitle[values, ":"],
			subpods
			}],
			ItemSize -> Scaled[0.998],
			Spacings -> .8
		]
	]
]


podTitle[{___, "title" -> str_, ___}, suffix_String] := Style[str <> suffix, FontColor -> RGBColor[0.411764, 0.352941, 0.494117], AutoItalicWords -> {"Mathematica"}]

podTitle[{___, "title" -> str_, ___}, {c_Integer, 0 | 1, "Input" | "Output" | "Content"}] := podTitle[{"title" -> str}, "  "]

podTitle[{___, "title" -> str_, ___}, {c_Integer, 0 | 1, datatype_String}] := podTitle[{"title" -> str}, " \[LongDash] " <> datatype]

podTitle[{___, "title" -> str_, ___}, {c_Integer, n_Integer, "Input" | "Output" | "Content"}] :=
	Style[Row[{str,
		Style[Row[{" (", ToString[c], " of ", ToString[n], ")  "}], FontColor -> GrayLevel[0.75], PrivateFontOptions -> {"OperatorSubstitution" -> False}]
		}],
		FontColor -> RGBColor[0.411764, 0.352941, 0.494117],
		AutoItalicWords -> {"Mathematica"}
	]

podTitle[{___, "title" -> str_, ___}, {c_Integer, n_Integer, datatype_String}] :=
	Style[Row[{
		str <> " \[LongDash] " <> datatype <> "  ",
		Style[Row[{" (", ToString[c], " of ", ToString[n], ")  "}], FontColor -> GrayLevel[0.75], PrivateFontOptions -> {"OperatorSubstitution" -> False}]
		}],
		FontColor -> RGBColor[0.411764, 0.352941, 0.494117],
		AutoItalicWords -> {"Mathematica"}
	]


podInfos[data_] := Flatten[podInfo /@ data]

podInfo[XMLElement["infos", values_, data:{__}]] := formatPodInfo /@ Cases[data, _XMLElement]

podInfo[other_] := {}


formatPodInfo[XMLElement["info", values_, data:{___, Except[_XMLElement], ___}]] :=
	formatPodInfo[XMLElement["info", values, Cases[data, _XMLElement]]]

formatPodInfo[XMLElement["info", values_, {XMLElement["units", _, data_]}]] :=
	formatPodInfoUnits[data]

formatPodInfo[info:XMLElement["info", {___, "text" -> text_, ___}, data:{___, XMLElement["link", _, _], ___}]] :=
	With[{base = formatPodInfoBase[info]},
		Mouseover[
			Row[{base, Style[" \[RightGuillemet]", Orange]}],
			Row[Flatten[{base, formatLink /@ Cases[data, XMLElement["link", _, _]]}], " | "],
			ImageSize -> {Automatic, All}
		]
	]

formatPodInfo[info:XMLElement["info", {___, "text" -> text_, ___}, data_]] :=
	formatPodInfoBase[info]

formatPodInfo[XMLElement["info", {}, {onelink : XMLElement["link", _, _]}]] :=
	formatLink[onelink, " \[RightGuillemet]"]

formatPodInfo[XMLElement["info", values_, data_]] :=
	Style[Row[data /. elem : XMLElement["link", _, _] :> formatLink[elem], " | "], FontColor -> Gray]

formatPodInfo[other_] := {}


formatPodInfoUnits[{___, XMLElement["cell", _, {cell_}], ___}] :=
	podInfoOpener[
		"Units",
		Style[prepareSubpodCellContent[cell, Automatic, ResetAndUndeploy], Background -> White]
	]

formatPodInfoUnits[{___, XMLElement["img", {___, "ImportedData" -> image_, ___}, _], ___}] :=
	podInfoOpener[
		"Units",
		image
	]

formatPodInfoUnits[data : {___, XMLElement["unit", _, _], ___}] := 
	podInfoOpener[
		"Units",
		Grid[
			Cases[data, XMLElement["unit", {___, "short" -> short_, ___, "long" -> long_, ___} | {___, "long" -> long_, ___, "short" -> short_, ___}, _] :> {short, long}],
			Frame -> All,
			FrameStyle -> LightGray,
			Alignment -> Left,
			Background -> White,
			Spacings -> {1,1},
			ItemStyle -> {{FontColor -> Black, FontColor -> Gray}, None}
		]
	]

formatPodInfoUnits[other_] := {}


podInfoOpener[label_, content_] :=
	DynamicModule[{Typeset`open = False, Typeset`button},
		Typeset`button = Button[
			Row[{
				PaneSelector[{
					True -> Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "OrangeMinus"]]],
					False -> Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "OrangePlus"]]]},
					Dynamic[Typeset`open]
				],
				" ",
				Mouseover[Style[label, FontColor -> Orange], Style[label, FontColor -> Red], ImageSize -> All]
			}],
			Typeset`open = Not[Typeset`open],
			Appearance -> None,
			BaseStyle -> {}
		];
		PaneSelector[{
				False -> Dynamic[Typeset`button],
				True -> Grid[{
					{Dynamic[Typeset`button]},
					{Panel[content,
						Alignment -> {Left, Top},
						Appearance :> FEPrivate`FrontEndResource["WAExpressions", "PodInfoAppearance"],
						FrameMargins -> {{30,30},{10,15}},
						ImageSize -> Scaled[1]
					]}
					}, Alignment -> Right]
			},
			Dynamic[Typeset`open],
			ImageSize -> Automatic
		]
	]





formatPodInfoBase[XMLElement["info", {___, "text" -> text_, ___}, {___, XMLElement["cell", _, {cell_Cell}], ___}]] := formatPodInfoCell[cell]

formatPodInfoBase[XMLElement["info", {___, "text" -> text_, ___}, {___, img:XMLElement["img", _, _], ___}]] := formatPodInfoImage[img]

formatPodInfoBase[XMLElement["info", {___, "text" -> text_, ___}, _]] := text


formatPodInfoCell[cell_Cell] := RawBoxes[cell //. StyleBox[x_, a___, _Integer | (FontSize -> _) | (FontFamily -> _), b___] :> StyleBox[x, a, b]]

formatPodInfoCell[other_] := ""


formatPodInfoImage[XMLElement["img", {___, "ImportedData" -> image_, ___}, _]] := image

formatPodInfoImage[XMLElement["img", values:{___, "alt" -> alt_, ___}, _]] := 
	Framed["" (* alt *), ImageSize -> {
		ToExpression["width" /. values /. "width" -> "Automatic"],
		ToExpression["height" /. values /. "height" -> "Automatic"]},
		FrameStyle -> GrayLevel[0.95],
		Alignment -> {Left, Top}
	]

formatPodInfoImage[other_] := ""






podDefinitions[data_] := Flatten[podDefinition /@ data]

podDefinition[XMLElement["definitions", values_, data:{___, XMLElement["definition", _, _], ___}]] :=
	podInfoOpener[
		"Definitions",
		Style[ Column[Flatten[formatPodDefinition /@ data], Spacings -> 1.2], FontColor -> GrayLevel[0.4] ]
	]

podDefinition[other_] := {}

formatPodDefinition[XMLElement["definition", values_, data_]] :=
	TextCell[
		Row[{
			Style[("word" /. values) <> ":", Bold],
			"\[IndentingNewLine]" <> ("desc" /. values)
		}],
		ParagraphIndent -> -10
	]

formatPodDefinition[other_] := {}



podNotes[data_] := Flatten[podNote /@ data]

podNote[XMLElement["notes", values_, data:{__}]] := 
	podInfoOpener[
		"Notes",
		Style[ Column[Flatten[formatPodNote /@ data], Spacings -> 1.2], FontColor -> GrayLevel[0.4] ]
	]

podNote[other_] := {}

formatPodNote[XMLElement["note", values_, {note_String}]] := TextCell[note]

formatPodNote[other_] := {}



podDefinitionsAndNotes[list_] := 
	Block[{defs, notes},
		defs = Flatten[Cases[list, XMLElement["definitions", values_, data_] :> data]];
		defs = Cases[defs, XMLElement["definition", _, _]];
		notes = Flatten[Cases[list, XMLElement["notes", values_, data:{__}] :> data]];
		notes = Cases[notes, XMLElement["note", _, {_String}]];
		podInfoOpener[
			"Definitions and notes",
			Style[ Column[Flatten[{formatPodDefinition /@ defs, formatPodNote /@ notes}], Spacings -> 1.2], FontColor -> GrayLevel[0.4]]
		]
	]



podStates[podid_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_]] :=
	If[# === {}, "", Row[#, "  "]]& @ Flatten[{
		podState[podid, #, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], {2}]& /@ data,
		podTearOffButton[podid, Dynamic[q], Dynamic[opts]],
		podTearOffMenu[podid, data, Dynamic[var], Dynamic[q], Dynamic[opts]]
	}]

subpodStates[podid_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], c_] :=
	If[# === {}, "", Row[#, "  "]]& @ Flatten[
		podState[podid, #, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], {3, c}]& /@ data
	]

podState[podid_, XMLElement["states", values_, data_], Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], auxpart_] :=
	Row[Flatten[podStateControl[podid, #, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], auxpart]& /@ data], $verticalBar3]

podState[podid_, XMLElement["sounds", values_, data_], Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], auxpart_] :=
	Row[Flatten[podSoundControls /@ data], $verticalBar3]

podState[podid_, other_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], auxpart_] := {}


podStateControl[podid_, e:XMLElement["state", {___, "name" -> "Start over", ___}, _], Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], {auxpart__}] :=
	{} (* these states are handled via formatSubpodsAsSteps *)

podStateControl[podid_, e:XMLElement["state", {___, "name" -> name_, ___}, _], Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], {auxpart__}] :=
	Button[
		lightFrameButtonBase[formatStateName[e]],
		CheckAbort[
			Quiet[WolframAlpha[]]; (* trigger autoloading *)
			changePodState[podid, stateInput[e], stateSBSQ[e], Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], {auxpart}],
			aux[[auxpart]] = False
		],
		ImageSize -> Automatic,
		Method -> "Queued",
		Appearance -> None
	]

podStateControl[podid_, XMLElement["statelist", {___, "value" -> value_, ___}, states:{___, XMLElement["state", {___, "name" -> _, ___}, _], ___}],
							Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], {auxpart__}] :=
Block[{choices, names, labels},
	choices = Cases[states, XMLElement["state", {___, "name" -> name_, ___}, _]];
	names = ("name" /. #[[2]])& /@ choices;
	labels = formatStateName /@ choices;
	ActionMenu[
		lightFrameActionMenuBase[Replace[value, Thread[names -> labels]]],
		MapThread[(#1 :> CheckAbort[
				Quiet[WolframAlpha[]]; (* trigger autoloading *)
				changePodState[podid, stateInput[#2], stateSBSQ[#2], Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], {auxpart}],
				aux[[auxpart]] = False
			])&,
			{labels, choices}
		],
		Appearance -> None,
		Method -> "Queued"
	]
]

podStateControl[other___] := {}


lightFrameButtonBase[label_] := 
	Style[
		Framed[
			Mouseover[
				Style[label, $assumptionLinkStyle],
				Style[label, $assumptionLinkActiveStyle],
				BaselinePosition -> Baseline,
				ImageSize -> All
			],
			Background -> GrayLevel[0.98],
			BaselinePosition -> Baseline,
			FrameMargins -> {{5,5},{1,1}},
			FrameStyle -> GrayLevel[0.9],
			RoundingRadius -> 3
		],
		"DialogStyle"
	]

lightFrameActionMenuBase[label_] := 
	Style[
		Framed[
			Mouseover[
				Row[{Style[label, $assumptionLinkStyle], $verticalBar1, Style["\[DownPointer]", 14]}],
				Row[{Style[label, $assumptionLinkActiveStyle], $verticalBar1, Style["\[DownPointer]", 14]}],
				BaselinePosition -> Baseline,
				ImageSize -> All
			],
			Background -> GrayLevel[0.98],
			BaselinePosition -> Baseline,
			FrameMargins -> {{5,5},{1,1}},
			FrameStyle -> GrayLevel[0.9],
			RoundingRadius -> 3
		],
		"DialogStyle"
	]


nextStepButtonBase[label_] := 
	Framed[
		label,
		Background -> Dynamic[If[CurrentValue["MouseOver"], Hue[0.08,0.82,1], Hue[0.07,0.877,0.99] ]],
		BaselinePosition -> Baseline,
		BaseStyle -> {"ControlStyle", FontColor -> GrayLevel[1]},
		FrameMargins -> {{15,15},{8,8}},
		FrameStyle -> Dynamic[If[CurrentValue["MouseOver"], Hue[0.08,0.82,1], Hue[0.07,0.877,0.99] ]],
		RoundingRadius -> 3
	]


allStepsButtonBase[label_] :=  
	Framed[
		label,
		Background -> Dynamic[If[CurrentValue["MouseOver"], Hue[0.08,0.82,1], GrayLevel[0.98] ]],
		BaselinePosition -> Baseline,
		BaseStyle -> {"ControlStyle", FontColor -> Dynamic[If[CurrentValue["MouseOver"], GrayLevel[1], RGBColor[0.987, 0.409, 0.032] ]]},
		FrameMargins -> {{15,15},{8,8}},
		FrameStyle -> Dynamic[If[CurrentValue["MouseOver"], Hue[0.08,0.82,1], GrayLevel[0.85] ]],
		RoundingRadius -> 3
	]



podTearOffButton[podid_, Dynamic[q_], Dynamic[opts_]] :=
If[ !MemberQ[AppearanceElements /. opts /. Options[WolframAlpha], "TearOffButtons"],
	{},
	Tooltip[
		Button[
			Dynamic[RawBoxes[FEPrivate`FrontEndResource["FEBitmaps", "ManipulatePasteIcon"]]],
			Quiet[WolframAlpha[]]; (* trigger autoloading *)
			doPodTearOffButton[q, IncludePods -> podid, AppearanceElements -> {"Pods"},
				Sequence @@ DeleteCases[opts, _[IncludePods | AppearanceElements | Asynchronous | Method, _]]],
			Appearance -> None,
			BaseStyle -> {}
		],
		Dynamic[FEPrivate`FrontEndResource["WAStrings", "ReplaceCellPod"]]
	]
]


doPodTearOffButton[q_, opts___] := (
	SelectionMove[EvaluationNotebook[], All, ButtonCell];
	NotebookWrite[EvaluationNotebook[],
		Cell[BoxData[ToBoxes[Defer[WolframAlpha][q, opts]]], "Input"],
		All
	];
	FrontEndTokenExecute[EvaluationNotebook[], "HandleShiftReturn"]
)


podTearOffMenu[podid_, data_, Dynamic[var_], Dynamic[q_], Dynamic[opts_]] := 
	With[{n = Count[data, XMLElement["subpod", _, _]]},
		Block[{dataformats, textformats},
			If[ !MemberQ[AppearanceElements /. opts /. Options[WolframAlpha], "PodMenus"],
				{},
				dataformats = First[Flatten[{Cases[data, XMLElement["dataformats", _, {dataformats_String}] :> dataformats, Infinity], ""}]];
				{dataformats, textformats} = {parseDataFormatsString[dataformats], parseTextFormatsString[dataformats]};
			
				Tooltip[
					ActionMenu[
						Dynamic[RawBoxes[FEPrivate`FrontEndResource["FEBitmaps", "CirclePlusIconScalable"]]],
						Flatten[{
							Dynamic[FEPrivate`FrontEndResource["WAStrings", "FormattedPod"]] :>
								(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodTearOffMenu["Pod", podid, var, q, opts]),
							Dynamic[FEPrivate`FrontEndResource["WAStrings", "SubpodContent"]] :>
								(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodTearOffMenu["Content", podid, n, var, q, opts]), 
							If[textformats === {}, {}, Delimiter],
							Map[(Dynamic[FEPrivate`FrontEndResource["WAStrings", #]] :>
									(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodTearOffMenu[#, podid, n, var, q, opts]))&,
								textformats
							],
							If[dataformats === {}, {}, Delimiter],
							Map[(Dynamic[FEPrivate`FrontEndResource["WAStrings", #]] :>
									(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodTearOffMenu[#, podid, n, var, q, opts]))&,
								dataformats
							]
						}],
						Appearance -> None,
						BaselinePosition -> (Top -> Top)
					],
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "PasteRelatedInputs"]]
				]
			]
		]
	]


doPodTearOffMenu["Pod", podid_, var_, q_, opts_] :=
	tearOffPaste[Defer[WolframAlpha][q, IncludePods -> podid, AppearanceElements -> {"Pods"},
		Sequence @@ DeleteCases[opts, _[IncludePods | AppearanceElements | Asynchronous | Method, _]]]]

doPodTearOffMenu[type_, podid_, 1, var_, q_, opts_] :=
	tearOffPaste[Defer[WolframAlpha][q, {{podid, 1}, type}, Sequence @@ Cases[opts, _[InputAssumptions | PodStates, _]]]]

doPodTearOffMenu[type_, podid_, n_Integer, var_, q_, opts_] :=
	tearOffPaste[Map[Defer[WolframAlpha][q, {{podid, #}, type}, Sequence @@ Cases[opts, _[InputAssumptions | PodStates, _]]]&, Range[n]]]


tearOffPaste[expr_] := (
	SelectionMove[EvaluationNotebook[], After, ButtonCell];
	NotebookWrite[EvaluationNotebook[], Cell[BoxData[ToBoxes[expr]], "Input"], All];
	FrontEndTokenExecute[EvaluationNotebook[], "HandleShiftReturn"]
)



formatStateName[XMLElement["state", {___, "name" -> name_, ___}, {___, XMLElement["nameformats", _, fmts_], ___}]] :=
	Replace[fmts, {
		{___, XMLElement["name", KeyValuePattern[{"format" -> "MathML", "name" -> mathml_String}], _], ___} :>
			Replace[ImportString[mathml, "MathML"] // Quiet, {$Failed :> name, boxes_ :> RawBoxes[boxes]}],
		{___, XMLElement["name", KeyValuePattern[{"format" -> "TeX", "name" -> tex_String}], _], ___} :>
			Replace[ImportString[tex, "TeX"] // Quiet, {$Failed :> name, boxes_ :> RawBoxes[boxes]}],
		_ :> name
	}]

formatStateName[XMLElement["state", {___, "name" -> name_, ___}, _]] := name

stateInput[XMLElement["state", {___, "input" -> input_, ___}, _]] := input

stateInput[XMLElement["state", {___, "name" -> name_, ___}, _]] := name

stateSBSQ[XMLElement["state", data_, _]] := MemberQ[data, "stepbystep" -> "true"]


changePodState[podid_, newstate_, sbsQ_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], {auxpart__}] := 
Block[{pods, podstates, newopts},

	If[sbsQ && $TrialProductQ, Message[WolframAlpha::trpr]; Return[$Failed]];
	aux[[auxpart]] = True;
	
	podstates = PodStates /. opts /. PodStates -> {};
	podstates = Flatten[{podstates, newstate}];

	newopts = Flatten[{DeleteCases[opts, _[PodStates, _]], PodStates -> podstates}];
	
	pods = Cases[
		WolframAlpha[q, "ProcessedXML", IncludePods -> {podid}, Asynchronous -> False, TimeConstraint -> {$AlphaQueryTimeConstraint, None, None, None}, Sequence @@ newopts],
		XMLElement["pod", {___, "id" -> podid, ___}, _],
		Infinity
	];
	If[pods =!= {}, var = expandData[First[pods]]; opts = newopts, Message[WolframAlpha::nopst, newstate, podid]; (* Beep[] *)];

	aux[[auxpart]] = False;
]


podSoundControls[XMLElement["sound", {___, "ImportedData" -> sound_, ___}, _]] :=
Block[{},
	If[ Head[sound] =!= Sound, Return[{}]];
	Button[
		lightFrameButtonBase["Play sound"],
		EmitSound[sound],
		ImageSize -> Automatic,
		Appearance -> None
	]
]


(* MIDI sends the full Sound object through, provided you've asked for msound *)
podSoundControls[XMLElement["sound", _, {a_String}]] := 
With[{sound = ToExpression[a]},
	If[ Head[sound] =!= Sound, Return[{}]];
	Button[
		lightFrameButtonBase["Play sound"],
		EmitSound[sound],
		ImageSize -> Automatic,
		Appearance -> None
	]
]


podSoundControls[other_] := {}



cdfPodMenu[podid_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_]] := 
		Block[{dataformats, textformats},
			If[ !MemberQ[AppearanceElements /. opts /. Options[WolframAlpha], "CDFPodMenus"],
				{},
				dataformats = First[Flatten[{Cases[data, XMLElement["dataformats", _, {dataformats_String}] :> dataformats, Infinity], ""}]];
				{dataformats, textformats} = {parseDataFormatsString[dataformats], parseTextFormatsString[dataformats]};
				dataformats = {};

				Tooltip[
					ActionMenu[
						Style[Mouseover[
							Style[Row[{Dynamic[FEPrivate`FrontEndResource["WAStrings", "CopyAs"]], " \[RightGuillemet]"}], FontColor -> Orange],
							Style[Row[{Dynamic[FEPrivate`FrontEndResource["WAStrings", "CopyAs"]], " \[RightGuillemet]"}], FontColor -> Red]
							],
							"DialogStyle"
						],
						Flatten[{
							Dynamic[FEPrivate`FrontEndResource["WAStrings", "FormattedPod"]] :>
								(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodCopyAs["Pod", podid, var, q, opts]),
							Dynamic[FEPrivate`FrontEndResource["WAStrings", "CDF"]] :>
								(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodCopyAs["CDF", podid, var, q, opts]),
							(*
							Dynamic[FEPrivate`FrontEndResource["WAStrings", "SubpodContent"]] :>
								(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodCopyAs["Content", podid, var, q, opts]), 
							*)
							If[textformats === {}, {}, Delimiter],
							Map[(Dynamic[FEPrivate`FrontEndResource["WAStrings", #]] :>
									(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodCopyAs[#, podid, var, q, opts]))&,
								textformats
							],
							If[dataformats === {}, {}, Delimiter],
							Map[(Dynamic[FEPrivate`FrontEndResource["WAStrings", #]] :>
									(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodCopyAs[#, podid, var, q, opts]))&,
								dataformats
							]
						}],
						Appearance -> None (* ,
						Method -> "Queued"*)
					],
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "CopyPod"]]
				]
			]
		]


changeLinkState[Dynamic[var_], Dynamic[aux_], value_] := 
	Block[{},
		(* Change the value of var to coerce dynamic updating *)
		var = {var};
		aux[[4]] = value;
		var = First[var];
	]


(*
Rasterize currently trouble rendering Dynamic content in the Plugin. So we
resolve Dynamic content before rasterizing.
*)

doPodCopyAs["Pod", podid_, var_, q_, opts_] :=
	CopyToClipboard[Rasterize[WolframAlpha[q, IncludePods -> podid, AppearanceElements -> {"Pods"},
		Sequence @@ DeleteCases[opts, _[IncludePods | AppearanceElements | Asynchronous | Method, _]]] //.
		HoldPattern[Dynamic][x_, ___] :> x, "Image"]]

doPodCopyAs["CDF", podid_, var_, q_, opts_] :=
	CopyToClipboard[
		WolframAlpha[q,
			AppearanceElements -> {"Brand", "CDFBrand", "ContentPadding", "Pods"},
			IncludePods -> podid,
			Sequence @@ Cases[opts, _[InputAssumptions | PodStates, _]]
		]
	]

doPodCopyAs[type_, podid_, var_, q_, opts_] :=
	Block[{result},
		result = WolframAlpha[q, {{podid, All}, type}, Sequence @@ Cases[opts, _[InputAssumptions | PodStates, _]]];
		CopyToClipboard[If[MatchQ[result, {__Rule}], Last /@ result, result]]
	]







formatLink[XMLElement["link", {___, "url" -> url_, ___, "text" -> text_, ___} | {___, "text" -> text_, ___, "url" -> url_, ___}, {}]] :=
	Hyperlink[text, url, 
		BaseStyle -> {"Hyperlink", FontColor -> Orange},
		ActiveStyle -> {"HyperlinkActive", FontColor -> Red},
		ButtonFunction :> (FrontEndExecute[{NotebookLocate[#2, "OpenInNewWindow" -> CurrentValue["HyperlinkModifierKey"]]}]&)
	]


formatLink[XMLElement["link", {___, "url" -> url_, ___, "text" -> text_, ___} | {___, "text" -> text_, ___, "url" -> url_, ___}, {}], suffix_String] :=
	formatLink[XMLElement["link", {"url" -> url, "text" -> (text <> suffix)}, {}]]


$subpodIndent = "   "


formatSubpods[ podid_, title_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[q_], Dynamic[opts_], Hold[$subpodCounter_], numberOfSubpods_, info_, appearanceElements_] :=
	Flatten[formatSubpod[podid, title, #, Dynamic[var], Dynamic[aux], Dynamic[chosen], Dynamic[open], Dynamic[q], Dynamic[opts], Hold[$subpodCounter], numberOfSubpods, info, appearanceElements]& /@ data]



processStepSubpods[formattedSubpods_List, steptypes_List] := 
Module[{list = formattedSubpods, length = Length[formattedSubpods], types = steptypes, struct, indexed, indexedItems, c, k, numberOfSteps, thin, thick},
	(* ensure the two lists have the same length *)
	If[Length[types] =!= length, types = PadRight[types, length, "SBSStep"]];
	(* simplify the incoming box generators if possible *)
	list = Replace[list, Column[{Grid[{{$subpodIndent, expr_}}, ___]}, ___] :> expr, {1}];
	(* group hints and steps together for purposes of inserting labels *)
	struct = MapThread[#1[#2] &, {types, list}] //. {a___, "SBSHint"[x_], "SBSStep"[y_], b___} :> {a, "SBSHintAndStep"[x, y], b};
	(* label each step, and split hints and steps back apart *)
	c = 0; k = 0;
	indexed = Flatten[
		Replace[
			Flatten[{"SBSLabel"[++c], #} & /@ struct], {
				Pattern[a, Blank["SBSLabel"]] :> (++k -> a),
				"SBSHintAndStep"[hint_, step_] :> {k -> "SBSHint"[hint], ++k -> "SBSStep"[step]},
				other_ :> (k -> other)
			},
			{1}
		]
	];
	(* now we know how many times the user will have to click "next" before getting to the end *)
	numberOfClicks = k;
	
	(* add appropriate margins and dividers directly to the content *)
	thin = {Directive[GrayLevel[0.87], Thickness[1]], Inherited, Inherited, Inherited};
	thick = {Directive[GrayLevel[0.95], Thickness[3]], Inherited, Inherited, Inherited};
	indexedItems = Replace[indexed, {
			(i_ -> "SBSLabel"[index_]) :> i -> {
				Spacer[15],
				Item[
					Pane[
						Row[{stringResource["StepPrefix"], " ", index}],
						BaseStyle -> {"ControlStyle", FontColor -> RGBColor[0.240, 0.583, 0.731]},
					ImageMargins -> {{0, 0}, {5, If[index === 1, 0, 20]}}],
					Frame -> thin,
					ItemSize -> Fit]
			},
    		(i_ -> "SBSHint"[expr_]) :> i -> {
				Spacer[15],
				Pane[expr, ImageMargins -> {{0, 0}, {5, 13}}]
			},
    		(i_ -> ("SBSStep" | "SBSIntro" | _)[expr_]) :> i -> {
    			(* the divider between steps extends into the left margin *)
				Item[Spacer[15], Frame -> thick], 
				Item[Pane[adjustStepMargins @ adjustStepText @ expr, ImageMargins -> {{0, 0}, {13, 13}}], Frame -> thick]
			}
		},
		{1}
	];
	{numberOfClicks, indexedItems}
]


(* Adjustments intended to avoid CLOUD-18639 *)
$AdjustStepText := False
$AdjustStepMargins := $CloudEvaluation

adjustStepText[expr_] := (expr /. (FontSize -> 14) :> (FontSize -> 15)) /; $AdjustStepText
adjustStepText[expr_] := expr

adjustStepMargins[ResetAndUndeploy[expr_]] :=
	ResetAndUndeploy[Style[expr, "AdjustStepMargins" -> {{0,0},{3,3}}]] /; $AdjustStepMargins
adjustStepMargins[expr_] := expr



formatSubpodsAsSteps[formattedSubpods_List, steptypes_List] := 
	DynamicModule[{Typeset`length, Typeset`indexedItems, Typeset`n = 1},
	
		{Typeset`length, Typeset`indexedItems} = processStepSubpods[formattedSubpods, steptypes];
		If[$TrialProductQ,
			{Typeset`length, Typeset`indexedItems} = {1, Cases[Typeset`indexedItems, _[1, _]]}
		];
	
		Column[{
			Dynamic[
				Grid[
					Cases[Typeset`indexedItems, _[Alternatives @@ Range[Typeset`n], Typeset`item_] :> Typeset`item],
					Spacings -> {0,0},
					Alignment -> Left
				],
				TrackedSymbols :> {Typeset`n}
			],
			If[TrueQ @ $TrialProductQ,
				Pane[
					Row[{
						Style[stringResource["StepByStepNotAvailable"], FontSlant -> "Italic"],
						MouseAppearance[#, "LinkHand"]& @ 
						Button[
							Framed[Style[stringResource["StepByStepUpgrade"], FontColor -> White],
								Background -> RGBColor["#f0702b"],
								BaselinePosition -> Baseline,
								FrameStyle -> None,
								RoundingRadius -> 3,
								FrameMargins -> {{8, 8}, {2, 2}}
							],
							SystemOpen[sbsUpgradeURL[]],
							Appearance -> None,
							BaselinePosition -> Baseline,
							ButtonNote -> sbsUpgradeURL[],
							ImageSize -> Automatic,
							ImageMargins -> {{6,0}, {0,0}}
						]
					}],
					BaseStyle -> {"ControlStyle"},
					ImageMargins -> {{15,15},{12,16}}
				],
				Item[
					PaneSelector[{
						False -> Grid[{{
							Button[nextStepButtonBase[stringResource["ShowNextStep"]],
								Typeset`n++,
								Appearance -> None,
								ImageMargins -> {{10,10},{10,14}}
							],
							Button[allStepsButtonBase[stringResource["ShowAllSteps"]],
								Typeset`n = Typeset`length,
								Appearance -> None,
								ImageMargins -> {{10,10},{10,14}}
							]
						}}],
						True ->
							Button[nextStepButtonBase[stringResource["StartOver"]],
								Typeset`n = 1,
								Appearance -> None,
								ImageMargins -> {{10,10},{10,14}}
							]
						},
						Dynamic[Typeset`n === Typeset`length],
						ImageSize -> Automatic
					]
				]
			]
		}, Spacings -> {0,0}]
	]	
	



formatSubpod[podid_, title_, e:XMLElement["subpod", values_, data_], Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[q_], Dynamic[opts_], Hold[$subpodCounter_], numberOfSubpods_, info_, appearanceElements_] := 
Block[{list, mforms, dataformats, textformats, subpodTitle},
	++$subpodCounter;
	If[Length[aux] === 2, AppendTo[aux, {False}]];
	If[Length[aux] === 3, AppendTo[aux, False]];
	If[Length[aux[[3]]] < $subpodCounter, aux[[3]] = PadRight[aux[[3]], $subpodCounter, False]];
	dataformats = First[Flatten[{Cases[data, XMLElement["dataformats", _, {dataformats_String}] :> dataformats], ""}]];
	{dataformats, textformats} = {parseDataFormatsString[dataformats], parseTextFormatsString[dataformats]};
	
	subpodTitle = If[
		MemberQ[data, XMLElement["states", _, _]],
		With[{c = $subpodCounter},
			Grid[{{
				Item[If[# === {}, "", #]& @ formatSubpodTitle[values], Alignment -> Left],
				Item[
					PaneSelector[{
						True -> necklaceAnimator[$necklaceAnimatorSize],
						False -> subpodStates[podid, data, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], c]
						},
						Dynamic[TrueQ[aux[[3, c]]]],
						ImageSize -> All,
						Alignment -> {Right, Top}
					],
					Alignment -> Right
				]
				}},
				Alignment -> Top,
				ItemSize -> Scaled[0.499]
			]
		],
		formatSubpodTitle[values]
	];

	list = Flatten[{
		subpodTitle,
		If[MemberQ[appearanceElements, "UseInputLeft"],
			{formatSubpodMInputLeft /@ data, formatSubpodMOutput /@ data},
			{}
		],
		If[MemberQ[appearanceElements, "UseInput"],
			formatSubpodMInputRight /@ data,
			{}
		],
		If[MemberQ[appearanceElements, "Extrusion"],
			mforms = formatMathematicaForm[#, "minput", "Input", Dynamic[chosen], Dynamic[open], Dynamic[q], Dynamic[opts], podid, title, $subpodCounter, numberOfSubpods]& /@ data;
			If[Flatten[mforms] === {},
				mforms = formatMathematicaForm[#, "moutput", "Output", Dynamic[chosen], Dynamic[open], Dynamic[q], Dynamic[opts], podid, title, $subpodCounter, numberOfSubpods]& /@ data;
			];
			{
				mforms,
				formatSubpodCell[#, Dynamic[aux], Dynamic[chosen], Dynamic[open], Dynamic[q], Dynamic[opts], podid, title, $subpodCounter, numberOfSubpods, textformats, dataformats]& /@ data
			},
			formatSubpodCell[#, Dynamic[aux], Dynamic[q], Dynamic[opts]]& /@ data
		],
		formatSubpodImage /@ data,
		subpodTearOffMenu[podid, $subpodCounter, numberOfSubpods, e, Dynamic[var], Dynamic[q], Dynamic[opts], appearanceElements]
	}];
	If[list === {}, {}, Column[list]]
]

formatSubpod[podid_, other_, ___] := {}

formatSubpodTitle[{___, "title" -> title:Except[""], ___}] :=
	Style["   " <> title <> ":", FontColor -> RGBColor[0.411764, 0.352941, 0.494117], AutoItalicWords -> {"Mathematica"}]

formatSubpodTitle[other_] := {}


subpodTearOffMenu[podid_, c_, n_, e:XMLElement["subpod", values_, data_], Dynamic[var_], Dynamic[q_], Dynamic[opts_], appearanceElements_] := 
If[ !MemberQ[appearanceElements, "SubpodMenus"],
	{},
	Item[
		Tooltip[
			ActionMenu[
				Dynamic[RawBoxes[FEPrivate`FrontEndResource["FEBitmaps", "CirclePlusIconScalable"]]],
				{
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "SubpodContent"]] :> 
						(Quiet[WolframAlpha[]]; (* trigger autoloading *) doSubpodTearOffMenu["Content", podid, c, e, q, opts]),
					Delimiter,
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "Plaintext"]] :>
						(Quiet[WolframAlpha[]]; (* trigger autoloading *) doSubpodTearOffMenu["Plaintext", podid, c, e, q, opts]),
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "Input"]] :> 
						(Quiet[WolframAlpha[]]; (* trigger autoloading *) doSubpodTearOffMenu["Input", podid, c, e, q, opts])
				},
				Appearance -> None
			],
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "PasteRelatedInputsSubpod"]]
		] // Grid[{{$subpodIndent, #}}]&,
		Alignment -> Left
	]
]



doSubpodTearOffMenu["Content", podid_, c_, e_, q_, opts_] :=
	subpodTearOffPaste[Defer[WolframAlpha][q, {{podid, c}, "Content"}, Sequence @@ Cases[opts, _[Assumption | PodStates, _]]]]

doSubpodTearOffMenu["Plaintext", podid_, c_, e_, q_, opts_] :=
	subpodTearOffPaste[Defer[WolframAlpha][q, {{podid, c}, "Plaintext"}, Sequence @@ Cases[opts, _[Assumption | PodStates, _]]]]

doSubpodTearOffMenu["Input", podid_, c_, e_, q_, opts_] :=
	subpodTearOffPaste[Defer[WolframAlpha][q, {{podid, c}, "Input"}, Sequence @@ Cases[opts, _[Assumption | PodStates, _]]]]



subpodTearOffPaste[expr_] := (
	SelectionMove[EvaluationNotebook[], After, ButtonCell];
	NotebookWrite[EvaluationNotebook[], Cell[BoxData[ToBoxes[expr]], "Input"], All];
	FrontEndTokenExecute[EvaluationNotebook[], "HandleShiftReturn"]
)


formatSubpodMInputAndOutput[XMLElement["minput", _, {text_}]] :=
	Grid[{{
		Item[Style["In[]:=", "CellLabel"], Alignment -> Right],
		formatSubpodUtility["minput content", text],
		Item[formatSubpodUtility["minput link", text], Alignment -> Right]
		},
		{
		Item[Style["Out[]=", "CellLabel"], Alignment -> Right],
		Style[
			Dynamic[TimeConstrained[ToExpression[text, InputForm], 3, $TimedOut], System`DestroyAfterEvaluation -> True, SynchronousUpdating -> False],
			"StandardForm", "Output", 12, ShowStringCharacters -> False, ScriptLevel -> 0],
		SpanFromLeft
		}},
		ItemSize -> {{Full, Fit, Full}},
		Alignment -> Left
	]

formatSubpodMInputAndOutput[other_] := {}

formatSubpodMInputLeft[XMLElement["minput", _, {text_}]] :=
	Grid[{{
		$subpodIndent,
		Item[formatSubpodUtility["minput content", text], Alignment -> Left],
		Item[formatSubpodUtility["minput link", text], Alignment -> Right]}},
		ItemSize -> {{Full, Fit, Full}}
	]

formatSubpodMInputLeft[other_] := {}

formatSubpodMInputRight[XMLElement["minput", _, {text_}]] := 
	Grid[{{formatSubpodUtility["minput right link", text]}}, ItemSize -> Fit, Alignment -> Right]

formatSubpodMInputRight[other_] := {}


formatSubpodMOutput[XMLElement["moutput", _, {text_}]] :=
	Grid[
		{{$subpodIndent, formatSubpodUtility["moutput", text]}},
		Alignment -> {Left, Baseline}]

formatSubpodMOutput[other_] := {}


formatSubpodCell[XMLElement["cell", _, {cell_}], Dynamic[aux_], Dynamic[q_], Dynamic[opts_]] :=
	Grid[{{
		$subpodIndent,
		prepareSubpodCellContent[cell, If[TrueQ @ aux[[4]], "AutomaticWithLinks", Automatic], ResetAndUndeploy, Dynamic[q], Dynamic[opts]]
		}},
		Alignment -> {Left, Baseline}]

formatSubpodCell[XMLElement["cell", _, {cell_}], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[q_], Dynamic[opts_], podid_, title_, c_, n_, textformats_, dataformats_] :=
	Grid[{{
		chosenIndent[Dynamic[chosen], {podid, c, {"Content", "ComputableData", "FormattedData", "FormulaData", "NumberData", "QuantityData", "SoundData", "TimeSeriesData"}}, None],
		Tooltip[
			Button[
				Framed[
					prepareSubpodCellContent[cell, If[TrueQ @ aux[[4]], "AutomaticWithLinks", Automatic], ResetAndDeploy, Dynamic[q], Dynamic[opts]],
					FrameStyle -> chosenFrameColor[Dynamic[chosen], {podid, c}, RGBColor[1., 0.723, 0.093], GrayLevel[0.85], None],
					Background -> Dynamic[FEPrivate`If[FrontEnd`CurrentValue["MouseOver"], RGBColor[1., 0.955, 0.836], None]],
					RoundingRadius -> 5
				],
				Quiet[WolframAlpha[]]; (* trigger autoloading *)
				chosen = {podid, title, c, n, "Content"};
				opts = updateOptionSetting[opts, Method, "ExtrusionChosen" -> chosen];
				SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
				If[$AlphaQueryExtrusionClickClose[] || MemberQ[CurrentValue["ModifierKeys"], "Command" | "Alt"],
					open = False;
					opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open]

				];
				If[$AlphaQueryExtrusionClickEvaluate[],
					SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
					FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
				],
				Appearance -> None,
				ImageSize -> Automatic,
				Alignment -> Left
			],
			chosenTooltip[Dynamic[chosen], {podid, c, {"Content", "ComputableData", "FormattedData", "FormulaData", "NumberData", "QuantityData", "SoundData", "TimeSeriesData"}}, None,
				PaneSelector[{
					"Content" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedSubpod"]],
					"ComputableData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedComputableData"]],
					"FormattedData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedFormattedData"]],
					"FormulaData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedFormulaData"]],
					"NumberData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedNumberData"]],
					"QuantityData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedQuantityData"]],
					"SoundData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedSoundData"]],
					"TimeSeriesData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedTimeSeriesData"]]
					},
					Dynamic[FEPrivate`Part[chosen, 5]],
					ImageSize -> Automatic
				],
				Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectThisSubpod"]]
			],
			BaseStyle -> {
				ContextMenu -> Flatten[{
					KernelMenuItem[cachedFrontEndResource["WAStrings", "CopySubpodContent"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						CopyToClipboard[ExpressionCell[prepareSubpodCellContent[cell, Automatic, ResetAndUndeploy], "Input"]]
					],
					KernelMenu[cachedFrontEndResource["WAStrings", "CopyAs"], Flatten[{
						Map[
							KernelMenuItem[cachedFrontEndResource["WAStrings", #], CopyToClipboard[ExpressionCell[WolframAlpha[q, {{podid, c}, #}], "Input"]]]&,
							textformats
						],
						If[textformats === {}, {}, Delimiter],
						Map[
							KernelMenuItem[cachedFrontEndResource["WAStrings", #], CopyToClipboard[ExpressionCell[WolframAlpha[q, {{podid, c}, #}], "Input"]]]&,
							dataformats
						],
						If[dataformats === {}, {}, Delimiter],
						KernelMenuItem[cachedFrontEndResource["WAStrings", "FormattedPod"], CopyToClipboard[ExpressionCell[WolframAlpha[q, AppearanceElements -> {"Pods"}, IncludePods -> podid], "Input"]]]
					}]],
					Delimiter,
					KernelMenu[cachedFrontEndResource["WAStrings", "PasteInputFor"], Flatten[{
						Map[
							KernelMenuItem[cachedFrontEndResource["WAStrings", #], CellPrint[ExpressionCell[Defer[WolframAlpha][q, {{podid, c}, #}], "Input", GeneratedCell -> False, CellAutoOverwrite -> False]]]&,
							textformats
						],
						If[textformats === {}, {}, Delimiter],
						Map[
							KernelMenuItem[cachedFrontEndResource["WAStrings", #], CellPrint[ExpressionCell[Defer[WolframAlpha][q, {{podid, c}, #}], "Input", GeneratedCell -> False, CellAutoOverwrite -> False]]]&,
							dataformats
						],
						If[dataformats === {}, {}, Delimiter],
						KernelMenuItem[cachedFrontEndResource["WAStrings", "SubpodContent"], CellPrint[ExpressionCell[Defer[WolframAlpha][q, {{podid, c}, "Content"}], "Input", GeneratedCell -> False, CellAutoOverwrite -> False]]],
						KernelMenuItem[cachedFrontEndResource["WAStrings", "FormattedPod"], CellPrint[ExpressionCell[Defer[WolframAlpha][q, AppearanceElements -> {"Pods"}, IncludePods -> podid], "Input", GeneratedCell -> False, CellAutoOverwrite -> False]]]
					}]],
					Delimiter,
					KernelMenuItem[cachedFrontEndResource["WAStrings", "Collapse"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						open = False;
						opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open]
					],
					KernelMenuItem[cachedFrontEndResource["WAStrings", "Evaluate"],
						SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
						FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
					],
					If[dataformats === {}, {}, KernelMenu[cachedFrontEndResource["WAStrings", "EvaluateAs"], 
						Map[
							KernelMenuItem[cachedFrontEndResource["WAStrings", #],
								Quiet[WolframAlpha[]]; (* trigger autoloading *)
								chosen = {podid, title, c, n, #, With[{q=q}, ToString[Unevaluated[WolframAlpha[q, {{podid, c}, #}]], InputForm]]};
								opts = updateOptionSetting[opts, Method, "ExtrusionChosen" -> chosen];
								SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
								FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
							]&,
							dataformats
						]
					]],
					KernelMenuItem[cachedFrontEndResource["WAStrings", "CollapseAndEvaluate"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						open = False;
						opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open];
						SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
						FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
					]
				}]
			}		
		]
		}},
		Alignment -> {Left, Baseline},
		Spacings -> 0.4
	]
		

formatSubpodCell[other_, ___] := {}


(*
prop: either "minput" or "moutput"
type: either "Input" or "Output"
mform: either the minput string or the moutput string
*)

formatMathematicaForm[XMLElement[prop_, _, {mform_String}], prop_, type_, Dynamic[chosen_], Dynamic[open_], Dynamic[q_], Dynamic[opts_], podid_, title_, c_, n_] := 
	Grid[{{
		chosenIndent[Dynamic[chosen], {podid, c, {"Input", "Output"}}, mform],
		Tooltip[
			Button[
				Framed[
					Style[
						shortenMathematicaBoxes[ ToExpression[mform, InputForm, AlphaQueryMakeBoxes], {"StandardForm", 12}, Scrollbars -> {False, False} ],
						If[type === "Input", Bold, Plain]
					],
					FrameStyle -> {Dynamic[FEPrivate`If[FrontEnd`CurrentValue["MouseOver"], RGBColor[1., 0.723, 0.093], GrayLevel[0.85]]]},
					Background -> Dynamic[FEPrivate`If[FrontEnd`CurrentValue["MouseOver"], RGBColor[1., 0.955, 0.836], RGBColor[0.973, 0.978, 0.98]]],
					RoundingRadius -> 5,
					FrameMargins -> {{8,8},{5,5}}
				],
				Quiet[WolframAlpha[]]; (* trigger autoloading *)
				chosen = {podid, title, c, n, type, mform};
				opts = updateOptionSetting[opts, Method, "ExtrusionChosen" -> chosen];
				SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
				If[$AlphaQueryExtrusionClickClose[] || MemberQ[CurrentValue["ModifierKeys"], "Command" | "Alt"],
					open = False;
					opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open]
				];
				If[$AlphaQueryExtrusionClickEvaluate[],
					SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
					FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
				],
				Appearance -> None,
				ImageSize -> Automatic,
				Alignment -> Left
			],
			chosenTooltip[Dynamic[chosen], {podid, c, {"Input", "Output"}}, mform,
				Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedInput"]],
				Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectThisInput"]]
			],
			BaseStyle -> {
				ContextMenu -> {
					KernelMenuItem[cachedFrontEndResource["WAStrings", "CopyInput"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						CopyToClipboard[Cell[BoxData[ToExpression[mform, InputForm, AlphaQueryMakeBoxes]], "Input"]]
					],
					Delimiter,
					KernelMenuItem[cachedFrontEndResource["WAStrings", "Collapse"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						open = False;
						opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open]
					],
					KernelMenuItem[cachedFrontEndResource["WAStrings", "Evaluate"],
						SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
						FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
					],
					KernelMenuItem[cachedFrontEndResource["WAStrings", "CollapseAndEvaluate"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						open = False;
						opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open];
						SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
						FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
					],
					Delimiter,
					KernelMenuItem[cachedFrontEndResource["WAStrings", "RemoveResults"],
						SelectionMove[ButtonNotebook[], All, ButtonCell];
						NotebookWrite[ButtonNotebook[], Cell[BoxData[mform], "Input"], All];
						SelectionMove[ButtonNotebook[], After, CellContents],
						Method -> "Queued" (* without this, the SelectionMoves don't happen properly *)
					]
				}
			}		
		]
		}},
		Alignment -> {Left, Baseline},
		Spacings -> 0.4
	]

formatMathematicaForm[other_, ___] := {}



SetAttributes[KernelMenuItem, HoldRest];

KernelMenu[label_, list_List] := System`Menu[label, list]

KernelMenuItem[label_, action_, opts___] := System`MenuItem[label, System`KernelExecute[action], System`MenuEvaluator -> Automatic, opts]



chosenIndent[Dynamic[chosen_], {podid_, c_, types_}, mform_] :=
	PaneSelector[{
		True -> Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "ChosenSubpodIndicator"]]],
		False -> ""
		},
		Dynamic[FEPrivate`Or[
			FEPrivate`And[
				FEPrivate`SameQ[FEPrivate`Part[chosen, 1], podid],
				FEPrivate`SameQ[FEPrivate`Part[chosen, 3], c],
				FEPrivate`MemberQ[types, FEPrivate`Part[chosen, 5]]
			],
			FEPrivate`SameQ[FEPrivate`Part[chosen, 6], mform]
		]],
		ImageSize -> All
	]


chosenTooltip[Dynamic[chosen_], {podid_, c_, types_}, mform_, chosenLabel_, otherLabel_] :=
	PaneSelector[
		{ True -> chosenLabel, False -> otherLabel },
		Dynamic[FEPrivate`Or[
			FEPrivate`And[
				FEPrivate`SameQ[FEPrivate`Part[chosen, 1], podid],
				FEPrivate`SameQ[FEPrivate`Part[chosen, 3], c],
				FEPrivate`MemberQ[types, FEPrivate`Part[chosen, 5]]
			],
			FEPrivate`SameQ[FEPrivate`Part[chosen, 6], mform]
		]],
		ImageSize -> Automatic
	]


chosenFrameColor[Dynamic[chosen_], {podid_, c_}, mouseoverColor_, chosenColor_, otherColor_] :=
	Dynamic[
		FEPrivate`If[
			FrontEnd`CurrentValue["MouseOver"],
			mouseoverColor,
			FEPrivate`If[
				FEPrivate`And[
					FEPrivate`SameQ[FEPrivate`Part[chosen, 1], podid],
					FEPrivate`SameQ[FEPrivate`Part[chosen, 3], c],
					FEPrivate`MemberQ[
						{"Content", "ComputableData", "FormattedData", "FormulaData", "NumberData", "QuantityData", "SoundData", "TimeSeriesData"},
						FEPrivate`Part[chosen, 5]
					]
				],
				chosenColor,
				otherColor
			]
		]
	]


(* two-arg, support for regular options *)

updateOptionSetting[{a___, _[name_, _], b___}, opt:(_[name_, _])] := {a, opt, b}

updateOptionSetting[{a___}, opt_] := {a, opt}

(* three-arg, support for Method options *)

updateOptionSetting[{a___, Method -> method_, b___}, Method, opt_] := {a, Method -> updateOptionSetting[Flatten[{method}], opt], b}

updateOptionSetting[{a___}, Method, opt_] := {a, Method -> {opt}}


getOptionSetting[{a___, _[name_, val_], b___}, name_] := val

getOptionSetting[{a___, Method -> method_, b___}, Method, name_] := getOptionSetting[Flatten[{method}], name]

getOptionSetting[{a___}, name_] := None





formatSubpodImage[XMLElement["img", {___, "ImportedData" -> image_, ___}, _]] :=
	Grid[{{$subpodIndent, ResetAndUndeploy @ image}}, ItemSize -> Full]

formatSubpodImage[XMLElement["img", values:{___, "alt" -> alt_, ___}, _]] := 
	Grid[{{"     ", 
		Framed["" (* alt *), ImageSize -> {
			ToExpression["width" /. values /. "width" -> "Automatic"],
			ToExpression["height" /. values /. "height" -> "Automatic"]},
			FrameStyle -> GrayLevel[0.95],
			Alignment -> {Left, Top}
		]}}, ItemSize -> Full]

formatSubpodImage[other_] := {}


formatSubpodCellAndData[data_] := 
Block[{cell, mdata},
	cell = Flatten[Cases[data, XMLElement["cell", _, {cell_}] :> cell]];
	mdata = Flatten[Cases[data, XMLElement["formatteddata" | "computabledata", _, {_}]]];

	If[Length[cell] > 0,
		cell = First[cell];
		If[Length[mdata] > 0,
			formatSubpodCellAndDataGrid[cell, mdata],
			formatSubpodCellAndDataGrid[cell, None]
		],
		(* else *)
		If[Length[mdata] > 0,
			formatSubpodCellAndDataGrid[None, mdata],
			{}
		]
	]
]

formatSubpodCellAndDataGrid[None, mdata_List] := 
	Grid[{{
		$subpodIndent,
		"",
		Item[formatSubpodUtility["data link", mdata], Alignment -> {Right, Top}]}},
		ItemSize -> {{Full, Fit, Full}}
	]

formatSubpodCellAndDataGrid[cell_, None] := 
	Grid[{{$subpodIndent, prepareSubpodCellContent[cell, Automatic, ResetAndUndeploy]}},
		Alignment -> {Left, Baseline}
	]

formatSubpodCellAndDataGrid[cell_, mdata_List] := 
	Grid[{{
		$subpodIndent,
		Item[prepareSubpodCellContent[cell, Automatic, ResetAndUndeploy], Alignment -> Left],
		Item[formatSubpodUtility["data link", mdata], Alignment -> {Right, Top}]
		}},
		ItemSize -> {{Full, Fit, Full}}
	]



parsePartialURL[url_String] := 
	Block[{input, assumptions, tmp},
		tmp = StringSplit[#, "="]& /@ StringSplit[url, {"/input/?", "&"}];
		input = Cases[tmp, {"i", s_String} :> s];
		If[input === {}, Return[$Failed]];
		{urldecode[First @ input], InputAssumptions -> Cases[tmp, {"a", s_String} :> s]}
	]


internalLink[boxes_, {input_, newopts___}, Dynamic[q_], Dynamic[opts_]] :=
	Button[
		Framed[RawBoxes[boxes],
			BaselinePosition -> Baseline,
			FrontEnd`BoxFrame -> {{0, 0}, {1, 0}},
			ContentPadding -> False,
			FrameMargins -> {{0, 0}, {1, 0}},
			FrameStyle -> Dynamic[FEPrivate`If[FrontEnd`CurrentValue["MouseOver"],
				(* RGB: 123, 171, 208 *) RGBColor[0.4824, 0.6706, 0.8157],
				(* RGB: 236, 236, 236 *) GrayLevel[0.9255]
			]]
		],
		If[MemberQ[AppearanceElements /. Flatten[{opts}] /. AppearanceElements -> {}, "CDFAssumptions"],
			NotebookLocate[{URL[WolframAlpha[input, "CDFURL", newopts, Sequence @@ DeleteCases[opts, _[InputAssumptions | PodStates, _]]]], None}, "OpenInNewWindow" -> CurrentValue["HyperlinkModifierKey"]],
			replaceQueryResults[ButtonNotebook[], input, newopts, Sequence @@ DeleteCases[opts, _[InputAssumptions | PodStates, _]]]
		],
		Method -> "Queued",
		BaseStyle -> {},
		DefaultBaseStyle -> {},
		Appearance -> None,
		ButtonNote -> input,
		ContentPadding -> False
	]


externalLink[boxes_, "URL"] := RawBoxes[boxes]

externalLink[boxes_, url_] := 
	Button[
		Grid[{{RawBoxes[boxes]}},
			BaselinePosition -> {1,1},
			FrameStyle -> Dynamic[FEPrivate`If[FrontEnd`CurrentValue["MouseOver"],
				RGBColor[0.4868, 0.50295, 0.736775],
				Directive[Dashing[{0.5, 0}], GrayLevel[0.5]]
			]],
			Dividers -> {None, 2 -> True},
			Spacings -> {0,0}
		],
		FrontEndExecute[{NotebookLocate[{URL[url], None}, "OpenInNewWindow" -> True]}],
		Evaluator -> None,
		BaseStyle -> {},
		DefaultBaseStyle -> {},
		Appearance -> None,
		ButtonNote -> url,
		ContentPadding -> False
	]


prepareSubpodCell[cell_Cell, simplifiers_, wrapper_] :=
	wrapper @ RawBoxes @ boxSimplify[#, simplifiers]& @ 
		Join[cell, Cell[FontFamily -> "Times", FontSize -> 14, ScriptLevel -> 0, Background -> None]]


prepareSubpodCellContent[cell_Cell, simplifiers_, wrapper_, args___] :=
	Block[{opts},
		
		opts = List @@ Rest[cell];
		opts = Flatten[{opts, FontFamily -> "Times", FontSize -> 14, ScriptLevel -> 0, Background -> None}];

		wrapper @ RawBoxes @ boxSimplify[#, simplifiers, args]& @ 
		If[MatchQ[cell, Cell[BoxData[_], ___]],
			StyleBox[First @ First @ cell, Sequence @@ opts],
			Cell[First @ cell, Sequence @@ opts]
		]
	]



ResetAndDeploy /: MakeBoxes[ResetAndDeploy[expr_], fmt_] := 
	MakeBoxes[Deploy[resetImageSizeLevel[expr]], fmt]

ResetAndUndeploy /: MakeBoxes[ResetAndUndeploy[expr_], fmt_] :=
	MakeBoxes[BoxForm`Undeploy[resetImageSizeLevel[expr]], fmt]


resetImageSizeLevel /: MakeBoxes[resetImageSizeLevel[Style[expr_, "AdjustStepMargins" -> val_]], fmt_] :=
	PaneBox[MakeBoxes[expr, fmt], BaselinePosition -> Baseline, FrameMargins -> val]

resetImageSizeLevel /: MakeBoxes[resetImageSizeLevel[expr_], fmt_] :=
	PaneBox[MakeBoxes[expr, fmt], BaselinePosition -> Baseline, FrameMargins -> 0]



formatSubpodUtility["minput content", minput_String] := 
	Block[{button, interactive},
		button = minputButton[
			Style[RawBoxes[minput], "StandardForm", "Input", 12, ShowStringCharacters -> True],
			minput
		];

		interactive = ToExpression[minput, InputForm, HoldComplete];
		interactive = minputToInteractive[interactive];
		If[interactive === None, Return[button]];

		interactive = Column[{
			minputButton[
				Style[RawBoxes[interactive], "StandardForm", "Input", 12, ShowStringCharacters -> True],
				interactive
			],
			ToExpression[interactive]
		}];
		OpenerView[{button, interactive}]
	]


minputButton[label_, inputboxes_] :=
	Button[
		Tooltip[label, "Paste input below"],
		If[CurrentValue["OptionKey"] || CurrentValue["AltKey"],
			NotebookPut[Notebook[{Cell[BoxData[inputboxes], "Input"]}]],
			SelectionMove[ButtonNotebook[], All, ButtonCell];
			SelectionMove[ButtonNotebook[], After, Cell];
			NotebookWrite[ButtonNotebook[], Cell[BoxData[inputboxes], "Input"], All];
			(* SelectionMove[ButtonNotebook[], All, CellContents]; *)
			(* FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"] *)
		],
		DefaultBaseStyle -> {},
		BaseStyle -> {},
		Appearance -> None
	]
	

(*
The toPlainString utility makes sure that literal occurrences of AlphaIntegration`Dump`
or Global` don't appear in the resulting string.
*)

toPlainString[expr_] :=
	StringReplace[ ToString[expr, InputForm], {$Context -> "", "AlphaIntegration`Dump`" -> ""}]


addQ[head_, interface_] := $AlphaQueryInterfaceAddedToMInput[head] === interface


minputToInteractive[HoldComplete[N[expr_, k_]]] /; addQ["N", "Manipulate"] := toPlainString @ 
	Manipulate[N[expr, n], {{n, k}, Floor[k / 2], 10 k, 1, Appearance -> "Labeled"}]

minputToInteractive[HoldComplete[ContinuedFraction[expr_, k_]]] /; addQ["ContinuedFraction", "Manipulate"] := toPlainString @ 
	Manipulate[ContinuedFraction[expr, n], {{n, k}, Floor[k / 2], 10 k, 1, Appearance -> "Labeled"}]

minputToInteractive[HoldComplete[Series[f_, {x_, x0_, k_}, opts___]]] /; addQ["Series", "Manipulate"] := toPlainString @ 
	Manipulate[Series[f, {x, x0, n}], {{n, k}, Floor[k / 2], 5 k, 1, Appearance -> "Labeled"}]

minputToInteractive[HoldComplete[Plot[f_, {x_, xmin_, xmax_}, opts___]]] /; addQ["Plot", "Manipulate"] := toPlainString @ 
	Manipulate[Plot[f, {x, xmin - n, xmax + n}, opts], {n, 0, (xmax - xmin)/2}]

minputToInteractive[HoldComplete[plot:Plot[f_, {x_, xmin_, xmax_}, opts___]]] /; addQ["Plot", "Explore"] := toPlainString @ 
	Experimental`Explore[plot, PlotRange]

minputToInteractive[HoldComplete[Plot3D[f_, {x_, xmin_, xmax_}, {y_, ymin_, ymax_}, opts___]]]  /; addQ["Plot3D", "Manipulate"] := toPlainString @ 
	Manipulate[Plot3D[f, {x, xmin - n, xmax + n}, {y, ymin - n, ymax + n}, opts], {n, 0, (xmax - xmin)/2}]

minputToInteractive[HoldComplete[ContourPlot[f_, {x_, xmin_, xmax_}, {y_, ymin_, ymax_}, opts___]]]  /; addQ["ContourPlot", "Manipulate"] := toPlainString @ 
	Manipulate[ContourPlot[f, {x, xmin - n, xmax + n}, {y, ymin - n, ymax + n}, opts], {n, 0, (xmax - xmin)/2}]

minputToInteractive[other_] := None



formatSubpodUtility["minput link", minput_String] :=
	minputButton[
		Style[Mouseover[
			Style["Use input \[RightGuillemet]", FontColor -> Purple],
			Style["Use input \[RightGuillemet]", FontColor -> Red]], "DialogStyle"],
		minput
	]


$UseInputLength = 40;

formatSubpodUtility["minput right link", minput_String] :=
Block[{label},
	label = If[StringLength[minput] < $UseInputLength+2, minput, StringTake[minput, $UseInputLength-2] <> " \[Ellipsis]"];
	minputButton[
		Style[Mouseover[
			Row[{
				Style[RawBoxes[label], "StandardForm", "Input", FontSize -> (Inherited+1), ShowStringCharacters -> True, Plain, ShowAutoStyles -> False],
				$verticalBar2,
				"Use input \[RightGuillemet]"
				},
				BaseStyle -> {FontColor -> GrayLevel[0.65]}
			],
			Row[{
				Style[RawBoxes[minput], "StandardForm", "Input", FontSize -> (Inherited+1), ShowStringCharacters -> True, Plain, FontColor -> Black],
				$verticalBar2,
				Style["Use input \[RightGuillemet]", FontColor -> Red]
				},
				BaseStyle -> {FontColor -> GrayLevel[0.65]}
			]],
			"DialogStyle"
		],
		minput
	]
]


$ErrorHighlightingOff = AutoStyleOptions -> {
	"HighlightSyntaxErrors" -> False, 
	"HighlightUnknownOptions" -> False, 
	"HighlightExcessArguments" -> False, 
	"HighlightMissingArguments" -> False, 
	"HighlightUnwantedAssignments" -> False, 
	"HighlightSymbolShadowing" -> False
	};



formatSubpodUtility["moutput", moutput_String] :=
	Style[TraditionalForm[ToExpression[moutput] /. HoldComplete -> Defer], "TraditionalForm", "TR", "Output", Larger, ScriptLevel -> 0]


formatSubpodUtility["data link", mdata_List] :=
	With[{data = dataView[mdata]},
		Tooltip[
			Button[
				Style[Mouseover[
					Style["Use data \[RightGuillemet]", FontColor -> Purple],
					Style["Use data \[RightGuillemet]", FontColor -> Red]], "DialogStyle"],
				If[CurrentValue["OptionKey"] || CurrentValue["AltKey"],
					NotebookPut[Notebook[{Cell[BoxData[ToBoxes[data]], "Input"]}]],
					SelectionMove[ButtonNotebook[], All, ButtonCell];
					SelectionMove[ButtonNotebook[], After, Cell];
					NotebookWrite[ButtonNotebook[], Cell[BoxData[ToBoxes[data]], "Input"], All];
				],
				DefaultBaseStyle -> {},
				BaseStyle -> {},
				Appearance -> None
			],
			"Paste data below"
		]
	]

dataName["computabledata"] := "ComputableData"
dataName["formatteddata"] := "FormattedData"
dataName[_] := "Unknown Data"

dataView[mdata:{XMLElement[_, _, {_}]..}] := dataName[First[#]] -> RawBoxes[First[Last[#]]]& /@ mdata







boxSimplify[boxes_, types_, args___] := boxes //. Flatten[boxSimplifyRules[#, args]& /@ Flatten[{types}]]


$BoxSimplifyAll = {
	"AllTemplateBoxDisplayFunctions",
	"AllTagBoxes",
	"AllButtonBoxes",
	"AllStyleBoxes",
	"AllFormBoxes"
}

$BoxSimplifyAutomatic = {
	"RowTemplateBoxes",
	"StripInternalLinks",
	"FixExternalLinks",
	"StringTagBoxes",
	"TagBoxWrappers",
	"IdentityTagBoxes"
}

$BoxSimplifyAutomaticWithLinks = {
	"RowTemplateBoxes",
	"CombineInternalLinks",
	"FixInternalLinks",
	"FixExternalLinks",
	"StringTagBoxes",
	"TagBoxWrappers",
	"IdentityTagBoxes"
}




boxSimplifyRules[None, args___] := {}

boxSimplifyRules[All, args___] := boxSimplifyRules[#, args]& /@ $BoxSimplifyAll

boxSimplifyRules[Automatic, args___] := boxSimplifyRules[#, args]& /@ $BoxSimplifyAutomatic

boxSimplifyRules["AutomaticWithLinks", args___] := boxSimplifyRules[#, args]& /@ $BoxSimplifyAutomaticWithLinks



boxSimplifyRules["AllTemplateBoxDisplayFunctions", ___] :=
	t:TemplateBox[_List, ___, DisplayFunction -> _Function, ___] :> Block[{}, BoxForm`TemplateBoxToDisplayBoxes[t] /; True]

boxSimplifyRules["RowTemplateBoxes", ___] :=
	t:TemplateBox[_List, "Row", ___, DisplayFunction -> _Function, ___] :> Block[{}, BoxForm`TemplateBoxToDisplayBoxes[t] /; True]

boxSimplifyRules["AllTagBoxes", ___] := {
	TagBox[arg_, tag_String, ___] :> StyleBox[arg, tag],
	TagBox[arg_, ___] :> arg
}

boxSimplifyRules["StringTagBoxes", ___] := TagBox[arg_, tag:Except["ResetImageSizeLevel" | "SkipImageSizeLevel" | "DynamicName", _String], ___] :> StyleBox[arg, tag]

boxSimplifyRules["TagBoxWrappers", ___] := TagBox[arg_, sym_Symbol[___], ___] :> arg /; SymbolName[Unevaluated[sym]] === "TagBoxWrapper"

boxSimplifyRules["IdentityTagBoxes", ___] := TagBox[arg_, Identity | (# &), ___] :> arg

boxSimplifyRules["StripInternalLinks", ___] := 
	(TagBox[ButtonBox[content_, ___] | content_, Annotation[#, s_String, "Hyperlink"]&] /; StringMatchQ[s, "/input/*"]) :> content

boxSimplifyRules["CombineInternalLinks", ___] := {
	(RowBox[list:{TagBox[ButtonBox[_, ___], Annotation[#, s_String, "Hyperlink"]&]..}] /; StringMatchQ[s, "/input/*"]) :> 
		Block[{}, ReplacePart[First[list], {1,1} -> RowBox[#[[1,1]]& /@ list]] /; True],
	(RowBox[list:{TagBox[_, Annotation[#, s_String, "Hyperlink"]&]..}] /; StringMatchQ[s, "/input/*"]) :> 
		Block[{}, ReplacePart[First[list], {1} -> RowBox[#[[1]]& /@ list]] /; True]
}

boxSimplifyRules["FixInternalLinks", args___] := 
	(TagBox[ButtonBox[content_, ___] | content_, Annotation[#, s_String, "Hyperlink"]&] /; StringMatchQ[s, "/input/*"]) :>
			Block[{}, ToBoxes[internalLink[content, parsePartialURL[s], args]] /; True]

boxSimplifyRules["StripExternalLinks", args___] := 
	TagBox[content_, Annotation[#, info_List, "ExternalLink"]&, ___] :> content

boxSimplifyRules["FixExternalLinks", args___] := 
	TagBox[content_, Annotation[#, info_List, "ExternalLink"]&, ___] :>
			Block[{}, ToBoxes[externalLink[content, "URL" /. info]] /; True]

boxSimplifyRules["AllButtonBoxes", ___] := ButtonBox[arg_, ___] :> arg

boxSimplifyRules["AllStyleBoxes", ___] := StyleBox[arg_, ___] :> arg

boxSimplifyRules["AllFormBoxes", ___] := FormBox[arg_, ___] :> arg

boxSimplifyRules[other___] := {}


(* ::Subsection::Closed:: *)
(*sources*)


AllSourceURLs[data_] := DeleteDuplicates[Flatten[sourceURLs /@ data]]

sourceURLs[XMLElement["sources", values_, data_]] :=
	Cases[data, XMLElement["source", {___, "url" -> url_String, ___}, _] :> url]

sourceURLs[other_] := {}



FormatAllSources[data_] := Flatten[formatSources /@ data]

formatSources[XMLElement["sources", values_, data_]] := 
	If[# === {}, {},
		ActionMenu[
			Mouseover[
				Style["Source Information \[RightGuillemet]", "DialogStyle", Gray],
				Style["Source Information \[RightGuillemet]", "DialogStyle", Red]
			],
			#,
			Appearance -> None
		]
	]& @ Map[formatSource, data]

formatSources[other_] := {}

formatSource[XMLElement["source", {___, "url" -> url_String, ___, "text" -> text_, ___} | {___, "text" -> text_, ___, "url" -> url_String, ___}, _]] :=
	With[{newurl = transformSourceURL[url]}, text :> NotebookLocate[{URL[newurl], None}]]

formatSource[other_] := {}


FormatAllSourcesActions[data_] := DeleteDuplicates[Flatten[formatSourcesActions /@ data]]

formatSourcesActions[XMLElement["sources", values_, data_]] := Map[formatSourceAction, data]

formatSourcesActions[other_] := {}

formatSourceAction[XMLElement["source", {___, "url" -> url_String, ___, "text" -> text_, ___} | {___, "text" -> text_, ___, "url" -> url_String, ___}, _]] :=
	With[{newurl = transformSourceURL[url]}, Row[{Dynamic[FEPrivate`FrontEndResource["WAStrings", "SourceInfo"]], ": " <> text}] :> NotebookLocate[{URL[newurl], None}] ]

formatSourceAction[other_] := {}


(*
The API currently points to completely unstyled HTML pages for source information. The
transformSourceURL utility changes the url reported by the API to one which points to a highly
styled page. Ideally, these urls would be reported by the API.
*)

transformSourceURL[url_String] :=
	If[StringMatchQ[url, "http://www.wolframalpha.com/sources/*SourceInformationNotes.html"],
		"http://www.wolframalpha.com/input/sources.jsp?sources=" <> StringTake[url, {37, -28}],
		url
	]


(* ::Subsection::Closed:: *)
(*additional information*)


FormatAdditionalInformation[Dynamic[{boxversion_, {podvarseq___}, {auxvarseq___}, chosen_, open_, elements_, q_, opts_, nonpods_, qinfo_, info_, showpods_, failedpods_, newq_}]] := 
	OpenerView[
		{
			"Additional Information",
			Dynamic[
				outerFrame[
					Column[Flatten[{
						Style["Query: " <> q, Bold],
						Grid[List @@@ Flatten[{opts, Cases[qinfo, _["queryurl", _], Infinity]}], Alignment -> Left],
						If[MatchQ[qinfo, {_List, _List}],
							{
								Style["Query result", Bold],
								Grid[List @@@ DeleteCases[First[qinfo], _[_, ""] | _["queryurl", _]], Alignment -> Left],
								Style["Recalculate result", Bold],
								Grid[List @@@ DeleteCases[Last[qinfo], _[_, ""] | _["queryurl", _]], Alignment -> Left]
							},
							{
								Style["Query result", Bold],
								Grid[List @@@ DeleteCases[qinfo, _[_, ""] | _["queryurl", _]], Alignment -> Left]
							}
						],
						Style["Client information", Bold],
						Grid[{
								{"product", ("ProductIDName" /. $ProductInformation)},
								{"kernel", SystemInformation["Kernel", "ReleaseID"]},
								{"", SystemInformation["Kernel", "Version"]},
								{"front end", SystemInformation["FrontEnd", "ReleaseID"]},
								{"", SystemInformation["FrontEnd", "Version"]},
								{"plugin version", SystemInformation["FrontEnd", "BrowserPlugin"]},
								{"plugin enabled", CurrentValue["PluginEnabled"]},
								{"client", $AlphaQueryClientLocation},
								{"box version", boxversion}
							},
							Alignment -> Left
						],
						Grid[List @@@ FE`Evaluate[FEPrivate`$StartTimes], Alignment -> Left],
						If[ListQ[#], Grid[List @@@ Sort[#], Alignment -> Left], {}]& @ FE`Evaluate[FEPrivate`TaskTimes[]]
						}],
						Spacings -> {1.5,1.5}
					],
					True
				]
			]
		},
		False
	]

			


(* ::Subsection::Closed:: *)
(*ExtrusionEvaluate[]*)


extrusionOpener[content_, nonpods_, Dynamic[q_], Dynamic[opts_], Dynamic[chosen_], Dynamic[open_], Dynamic[newq_]] := 
	Block[{grid, assumptionsQ = MatchQ[nonpods, {___, XMLElement["assumptions", _, _], ___}]},
		grid =
			DynamicModule[{Typeset`show = False, Typeset`reinterpret},
				Typeset`reinterpret = Cases[nonpods, XMLElement["reinterpret", values:{___, "new" -> new_, ___}, _] :> values, Infinity];
				If[Typeset`reinterpret =!= {}, Typeset`reinterpret = {"new", "text"} /. First[Typeset`reinterpret]];
				EventHandler[
					innerFrame[
						Grid[{{
								Dynamic[
									If[MatchQ[Typeset`reinterpret, {_String, _String}],
										Grid[{{
												Style[queryBlobInputField[Dynamic[newq]], Plain, Gray],
												SpanFromLeft,
												SpanFromLeft
											},
											{
												Tooltip[
													Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "ReinterpretationIndicator"]]],
													Last[Typeset`reinterpret]
												],
												queryBlobButton[First[Typeset`reinterpret]],
												suggestionsDialogButton["Reinterpret", q]
											}},
											Alignment -> Left,
											BaselinePosition -> {1,1},
											Spacings -> 0.5
										],
										queryBlobInputField[Dynamic[newq]]
									]
								],
								If[assumptionsQ,
									Button[
										Tooltip[
											Style["\[RightGuillemet]", Orange],
											Dynamic[FEPrivate`FrontEndResource["WAStrings", "InterpretationAssumptions"]]
										],
										Quiet[WolframAlpha[]]; (* trigger autoloading *)
										open = !open;
										opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open],
										Appearance -> None,
										BaselinePosition -> Baseline,
										ContentPadding -> False
									],
									Unevaluated[Sequence[]]
								],
								Item[
									Button[
										PaneSelector[{
											True -> Tooltip[Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "OrangeSquareMinus"]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "HideAllResults"]]],
											False -> PaneSelector[{
												True -> Tooltip[Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "OrangeSquarePlus"]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowAllResults"]]],
												False -> Tooltip[Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "GraySquarePlus"]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowAllResults"]]]
												},
												Dynamic[Typeset`show],
												ImageSize -> All
											]
											},
											Dynamic[open],
											ImageSize -> All
										],
										Quiet[WolframAlpha[]]; (* trigger autoloading *)
										If[CurrentValue["OptionKey"] || CurrentValue["AltKey"],
											SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
											NotebookWrite[ButtonNotebook[], Cell[newq, "WolframAlphaShort", FormatType -> "TextForm"], All],
											(* else *)
											open = !open;
											opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open]
										],
										Appearance -> None
									],
									Alignment -> {Right, Top}
								]
							},
							{
								Dynamic[Quiet[Switch[{chosen[[1]], chosen[[5]]},
									{"Fast parse", "Input" | "Output"},
									queryBlobMathematicaForm[ToExpression[chosen[[6]], InputForm, Defer]],
									{_, "Content"},
									Grid[{{
											Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "PodTitleIndicator"]]],
											Style[podTitle[{"title" -> chosen[[2]]}, Part[chosen, {3, 4, 5}]], "DialogStyle"]
										}},
										Spacings -> 0.3
									],
									{_, "Input" | "Output" | "ComputableData" | "FormattedData" | "FormulaData" | "NumberData" | "QuantityData" | "SoundData" | "TimeSeriesData"},
									Grid[{{
											Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "PodTitleIndicator"]]],
											Style[podTitle[{"title" -> chosen[[2]]}, Part[chosen, {3, 4, 5}]], "DialogStyle"],
											SpanFromLeft
										},
										{
											"",
											queryBlobMathematicaForm[ToExpression[chosen[[6]], InputForm, Defer]],
											" "
										}},
										Alignment -> Left,
										Spacings -> 0.3
									],
									_,
									Style[Row[{"(", Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoSelection"]], ")"}], "DialogStyle", FontColor -> Gray]
								]]],
								If[assumptionsQ, SpanFromLeft, Unevaluated[Sequence[]]],
								SpanFromLeft
							}},
							Alignment -> Left,
							Dividers -> {False, Center},
							FrameStyle -> LightGray,
							BaselinePosition -> {1,1}
						],
						ImageSize -> Automatic,
						BaselinePosition -> Baseline,
						Background -> queryBlobBackground[Dynamic[q], Dynamic[newq]]
					],
					{"MouseEntered" :> FEPrivate`Set[Typeset`show, True], "MouseExited" :> FEPrivate`Set[Typeset`show, False]}
				]
			];
			
		PaneSelector[{False -> grid, True -> Column[{grid, Pane[content, $AlphaQueryExtrusionWidth]}, BaselinePosition -> {1,1}]},
			Dynamic[open],
			ImageSize -> Automatic,
			BaseStyle -> {Deployed -> True}
		]
	]



queryBlobInputField[Dynamic[newq_]] := 
		InputField[Dynamic[newq],
			String,
			Appearance -> None,
			BaseStyle -> {"CalculateInput"},
			Enabled -> True,
			ContinuousAction -> True,
			FieldSize -> {{1,40},{1,Infinity}},
			If[TrueQ[BoxForm`sufficientVersionQ[8.0, 1]], System`TrapSelection -> False, Unevaluated[Sequence[]]]
		]

queryBlobButton[query_String] := 
	Tooltip[
		Button[
			Mouseover[query, Style[query, FontColor -> Orange]],
			SelectionMove[ButtonNotebook[], All, ButtonCell];
			NotebookWrite[ButtonNotebook[], Cell[query, "WolframAlphaShort", FormatType -> "TextForm"], All];
			SelectionMove[ButtonNotebook[], After, CellContents],
			Alignment -> Left,
			Appearance -> None,
			BaseStyle -> {"CalculateInput"}
		],
		Dynamic[FEPrivate`FrontEndResource["WAStrings", "RemoveResults"]]
	]


queryBlobBackground[Dynamic[q_], Dynamic[newq_], color_:RGBColor[1., 0.975, 0.95]] := 
	Dynamic[
		FEPrivate`If[
			FEPrivate`And[
				FEPrivate`SameQ[FEPrivate`Head[newq], String],
				FEPrivate`UnsameQ[q, newq]
			],
			color,
			GrayLevel[1]
		]
	] /; StringQ[q]

queryBlobBackground[else___] := GrayLevel[1]


queryBlobMathematicaForm[Defer[expr_]] := 
	With[{boxes = AlphaQueryMakeBoxes[expr]},
		shortenMathematicaBoxes[
			Tooltip[
				Button[
					Mouseover[
						Style[RawBoxes[boxes], NumberMarks -> False],
						Style[RawBoxes[boxes], NumberMarks -> False, FontColor -> Orange, ShowSyntaxStyles -> False],
						BaseStyle -> {ShowStringCharacters -> True}
					],
					SelectionMove[ButtonNotebook[], All, ButtonCell];
					NotebookWrite[ButtonNotebook[], Cell[BoxData[boxes], "Input"], All];
					SelectionMove[ButtonNotebook[], After, CellContents],
					Alignment -> Left,
					Appearance -> None,
					BaseStyle -> {},
					ImageSize -> Automatic
				],
				Dynamic[FEPrivate`FrontEndResource["WAStrings", "RemoveResults"]]
			] // ToBoxes,
			{}
		]
	]


shortenMathematicaBoxes[boxes_, {basestyleopts___}, opts___] :=
	Pane[
		RawBoxes[boxes],
		opts,
		ImageSize -> {Automatic, {1,100}},
		Scrollbars -> {False, Automatic},
		AppearanceElements -> {},
		BaseStyle -> {basestyleopts, ShowStringCharacters -> True, ScriptLevel -> 0}
	]




AlphaIntegration`ExtrusionEvaluate[query_String, fmt_, opts___] :=
Block[{result, rawparse, parseassumptions, xml, options, previouslyChosen, oldid, monitor, starttime = AbsoluteTime[], conopen=False,
		$AlphaQueryMMode = "input", hasFlag=False, fastparseQ = False, fastparse},

	If[TrueQ[$AlphaQueryExtrusionMonitor],
		monitor[args___] := Print[Style[Row[{AbsoluteTime[]-starttime, ": ", args}], "Message", "MSG"]],
		monitor[args___] := Null
	];

	If[doQuerySideEffects[query], Return[Null]];
	If[StringTrim[query] === "", Return[Null]];
	
	(* If the query contains symbols with OwnValues, ask for permission to send msessinfo *)
	mathematicaSessionInfoPermissionDialogIfNecessary[query, {}];
	
	(* The choice procedure needs to take into account the previous choice, if there was one.*)
	previouslyChosen = "ExtrusionChosen" /. (Method /. {opts} /. Method -> {});
	If[!MatchQ[previouslyChosen, {_, _, _, _, __}], previouslyChosen = None,  oldid = First[previouslyChosen]];
	
	(* RETURNING THE FAST PARSE *)
	(* if there's a fast parse, return it as the input *)
	monitor["Checking fast parse..."];
	rawparse = If[previouslyChosen === None, Quiet[WolframAlpha[query, "RawWolframParse", opts], WolframAlpha::conopen], {"Parse" -> None}];
	result = Replace[rawparse, {{___, "Parse" -> a_String, ___} :> mparseMakeExpression[a], _ -> $Failed}];
	If[MatchQ[result, _HoldComplete],
		If[$WolframAlphaNotebook === True,
			(* If there's a fast parse, continue, but remember that there was one so we can examine the first pod below *)
			monitor["There is a fast parse. Continuing..."];
			fastparseQ = True;
			fastparse = result;
			,
			parseassumptions = MatchQ[rawparse, {___, "Assumptions" -> {__}, ___}];
			replaceEvaluationCell[formatQueryBlob[query, Defer @@ result, "Fast parse", parseassumptions]];
			Return @ fastParseResult[result]
		];
	];
	
	(* otherwise, we'll be examining the full results *)
	monitor["Getting full results..."];
	options = resolveOptions[
		Asynchronous -> All,
		AppearanceElements -> {"Extrusion", "Warnings", "Assumptions", "Pods"},
		Method -> {
			"Formats" -> Flatten[{If[TrueQ[$WolframAlphaNotebook], {}, "cell"], "minput", If[$AlphaQueryExtrusionShowMOutputs, "moutput", {}, {}], "msound", "dataformats"}],
			"NewQuery" -> None
		},
		opts
	];
	redirectMessages[
		xml = WolframAlpha[query, "ProcessedXML", options] // expandData,
		WolframAlpha::conopen,
		conopen = True
	];
	If[errorBlobQ[conopen, xml, query, options],
		CellPrint[formatErrorBlob[conopen, xml, query, options]];
		Return[Null]
	];
	
	(* If the xml contains appropriate MInput scanner results, ask for permission to send msessinfo *)
	monitor["Ask permission for msessinfo if necessary..."];
	If[ mathematicaSessionInfoPermissionDialogIfNecessary[query, xml] === True,
		(* If the user gave permission, redo the query *)
			monitor["msessinfo permissions changed. Getting full results again..."];
			xml = WolframAlpha[query, "ProcessedXML", options] // expandData;		
	];
	
	(* standardize the ordering of pod attributes, to simplify pattern checks below *)
	xml = xml /. XMLElement["pod", values_, data_] :> XMLElement["pod", SortBy[values, First], data];
	
	(* RETURNING THE FAST PARSE *)
	(* if there was a fast parse, return the minput of the first pod's first subpod, if there is one *)
	If[fastparseQ === True,
		monitor["Comparing results with fast parse..."];
		result = Cases[xml, XMLElement["pod", _, _], Infinity];
		If[result =!= {},
			result = First @ result;
			oldid = Replace[result, {XMLElement["pod", {___, "id" -> a_String, ___}, subpods_] /; (Count[subpods, XMLElement["minput", _, _], Infinity] > 0) :> a, _ -> None}];
			If[StringQ[oldid],
				Return @ chooseAndReturnAValue[query, xml, {oldid, "fake", 1, 0, "Input"}, options]
			],
			(* Otherwise there were no pods. Return the parse. *)
			monitor["No pods found. Returning the parse."];
			Return[{Defer @@ fastparse, {}}]
		];
	];
	
	(* RETURNING A RESULT THAT MATCHES A PREVIOUSLY CHOSEN RESULT *)
	(* if the previously chosen podid also exists in the new results, try to return it *)
	monitor["Checking previously chosen podid..."];
	If[previouslyChosen =!= None,
		result = Cases[xml, XMLElement["pod", {___, "id" -> oldid, ___}, _], Infinity];
		If[result =!= {},
			(* load any async pods with that podid *)
			xml = xml /. XMLElement["pod", {___, "async" -> url_, ___, "id" -> oldid, ___}, _] :> expandAsyncPod[url, True];
			result = Cases[xml, XMLElement["pod", {___, "id" -> oldid, ___}, _], Infinity]
		];
		If[result =!= {}, Return @ chooseAndReturnAValue[query, xml, previouslyChosen, options]]
	];
	
	(* RETURNING A RESULT THAT HAS BEEN TAGGED AS A PRIMARY RESULT *)
	monitor["Checking for a primary result tag..."];
	(* load any async pods that look like primary result pods *)
	xml = xml /. XMLElement["pod", {___, "async" -> url_, ___, "primary" -> "true", ___}, _] :> expandAsyncPod[url, True];
	(* if there are still primary result pods, return information from the first one *)
	result = Cases[xml, XMLElement["pod", {___, "id" -> id_String, ___, "primary" -> "true", ___, "title" -> title_String, ___}, _] :> {id, title}, Infinity];
	If[result =!= {}, Return @ chooseAndReturnAValue[query, xml, First @ result, options] ];
		
	(* RETURNING AN MINPUT-SCANNER RESULT *)
	monitor["Checking for MInput Scanner results..."];
	(* load any async pods from the "MInput" scanner *)
	xml = xml /. XMLElement["pod", {___, "async" -> url_, ___, "scanner" -> "MInput", ___}, _] :> expandAsyncPod[url, True];
	(* if there are any pods from the "MInput" Scanner, use the first one *)
	result = Cases[xml, XMLElement["pod", {___, "scanner" -> "MInput", ___}, _], Infinity];
	result = Cases[result, XMLElement["pod", {___, "id" -> id_String, ___, "title" -> title_String, ___}, _] :> {id, title}, Infinity];
	If[result =!= {}, Return @ chooseAndReturnAValue[query, xml, First @ result, options] ];
	
	(* RETURNING A RESULT FROM A RESULT POD *)
	monitor["Checking for a \"result\" pod..."];
	(* load any async pods that look like results pods *)
	xml = xml /. XMLElement["pod", {___, "async" -> url_, ___, "title" -> (title_String /; StringMatchQ[title, "*Result*"]), ___}, _] :> expandAsyncPod[url, True];
	(* if there are results pods, return information from the first one *)
	result = Cases[xml, XMLElement["pod", {___, "id" -> id_String, ___, "title" -> title_String, ___}, _] :> {id, title}, Infinity];
	result = Select[result, StringMatchQ[#[[2]], "*Result*"]&];	
	If[result =!= {}, Return @ chooseAndReturnAValue[query, xml, First @ result, options] ];
	
	(* RETURNING A RESULT FROM SOME OTHER POD *)
	(* otherwise, if there are any pods, focus on the first couple *)
	monitor["No results pods found. Trying other pods..."];
	result = Cases[xml, XMLElement["pod", {___, "id" -> id_String, ___, "title" -> title_String, ___}, _] :> {id, title}, Infinity];
	result = Take[result, Min[2, Length[result]]];
	If[MatchQ[result, {__List}],
		xml = xml /. XMLElement["pod", {___, "async" -> url_, ___, "id" -> (Alternatives @@ First /@ result), ___}, _] :> expandAsyncPod[url, True];
		result = Cases[xml, XMLElement["pod", {___, "id" -> id:(Alternatives @@ First /@ result), ___, "title" -> title_, ___}, _] :> {id, title}, Infinity];
		result = Take[result, Min[2, Length[result]]];
		If[MatchQ[result, {__List}], Return @ chooseAndReturnAValue[query, xml, Last @ result, options] ];
		(* in the rare instance that there are any pods left, load the remaining async content before proceeding *)
		xml = xml /. XMLElement["pod", {___, "async" -> url_, ___}, _] :> expandAsyncPod[url, True];
		result = Cases[xml, XMLElement["pod", {___, "id" -> id_String, ___, "title" -> title_String, ___}, _] :> {id, title}, Infinity];
		If[result =!= {}, Return @ chooseAndReturnAValue[query, xml, First @ result, options] ];
	];
	
	monitor["No pods found."];

	(* RETURNING EXAMPLES *)
	(* In the $WolframAlphaNotebook case, if there's an "examplepage" element present, return it *)
	If[$WolframAlphaNotebook,
		result = FirstCase[xml, XMLElement["examplepage", {___, "url" -> url_String, ___}, _], None, Infinity];
		If[result =!= None,
			Return @ {result, Cases[xml, XMLElement["assumptions", _, _], Infinity]}
		];
	];
	
	(* RETURNING ASSUMPTIONS *)
	(* In the $WolframAlphaNotebook case, we want the set of assumptions even if there isn't a parse *)
	If[$WolframAlphaNotebook,
		Return @ {None, Cases[xml, XMLElement["assumptions", _, _], Infinity]}
	];
	
	(* RETURNING NOTHING *)
	(* otherwise, there's nothing to do *)
	replaceEvaluationCell[formatQueryBlob[query, None, Row[{"(", Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoInterpretations"]], ")"}]]];
	Return[Null];
]

(*
When the first argument to ExtrusionEvaluate is boxes rather than a string, check to see
whether it contains a known result structure. If it does, those structures have untypeset
rules that take care of interpretation, so just call MakeExpression.

If they don't -- that is, if a user has somehow gotten a "WolframAlphaShort" cell that
contains boxes which aren't W|A result boxes -- then try to intepret those boxes as a
query string, using the same mechanism that AlphaIntegration`AlphaQuery uses.
*)

AlphaIntegration`ExtrusionEvaluate[boxes_, fmt:(StandardForm | TraditionalForm)] := 
Block[{heldexpr, $AlphaQueryMMode = "input",hasFlag=False},
	heldexpr = If[
		!FreeQ[boxes, NamespaceBox["WolframAlphaQueryResults" | "WolframAlphaQueryNoResults" | "WolframAlphaQueryParseResults", ___]],
		MakeExpression[boxes, fmt],
		With[{query = queryBoxesToQueryString[boxes, fmt]},
			HoldComplete[AlphaIntegration`ExtrusionEvaluate[query, InputForm]]
		]
	];
	If[MatchQ[heldexpr, _HoldComplete],
		ReleaseHold[heldexpr],
		Null
	]
]

AlphaIntegration`ExtrusionEvaluate[boxes_, fmt_] := Null (* otherwise, do nothing *)




chooseAndReturnAValue[query_, xml_, {id_, title_}, opts___] :=
	chooseAndReturnAValue[query, xml, {id, title, Automatic, Automatic, Automatic, Automatic}, opts]


chooseAndReturnAValue[query_, xml_, {id_, title_, subpodid_, numberOfSubpods_, type_}, opts___] := 
	chooseAndReturnAValue[query, xml, {id, title, subpodid, numberOfSubpods, type, Automatic}, opts]


chooseAndReturnAValue[query_, xml_, {podid_, title_, subpodid_, numberOfSubpods_, type_, mform_}, opts___] := 
Block[{podxml, subpodxml, primarysubpods, c, n, minputs, moutputs, returntype, str},
	podxml = First[Cases[xml, XMLElement["pod", {___, "id" -> podid, ___}, _], Infinity]];
	subpodxml = Cases[podxml, XMLElement["subpod", _, _], Infinity];
	n = Length[subpodxml];
	c = Which[
		IntegerQ[subpodid] && 1 <= subpodid <= n, subpodid,
		(* If subpodid is Automatic, look for a primary result subpod *)
		(primarysubpods = First /@ Position[subpodxml, XMLElement["subpod", {___, "primary" -> "true", ___}, _]]) =!= {}, First[primarysubpods],
		True, 1
	];
	subpodxml = Part[subpodxml, c];
	
	minputs = Cases[subpodxml, XMLElement["minput", _, {minput_String}] :> minput, Infinity];
	moutputs = Cases[subpodxml, XMLElement["moutput", _, {moutput_String}] :> moutput, Infinity];
	
	returntype = Which[
		(* if the requested type is available, choose it *)
		type === "Content", "Content",
		type === "Input" && minputs =!= {}, "Input",
		type === "Output" && moutputs =!= {}, "Output",
		MemberQ[$AlphaQueryDataFormats, type], type,
		(* otherwise, the requested type is not available, so we have to pick one *)
		(* use minput or moutput if they exist, and plain content otherwise *)
		minputs =!= {}, "Input",
		moutputs =!= {}, "Output",
		True, "Content"
	];
	
	(* Now that the return type has been chosen, time to actually return it *)
	Switch[returntype,
		"Input",
			replaceEvaluationCell[formatQueryBlob[query, xml, {podid, title, c, n, "Input", First @ minputs}, opts]];
			If[
				TrueQ[$WolframAlphaNotebook],
				Return[{minputInput[First @ minputs], Cases[xml, XMLElement["assumptions", _, _], Infinity]}],
				Return[ minputResult[First @ minputs]]
			]
		,
		"Output",
			replaceEvaluationCell[formatQueryBlob[query, xml, {podid, title, c, n, "Output", First @ moutputs}, opts]];
			If[
				TrueQ[$WolframAlphaNotebook],
				Return[{minputInput[First @ moutputs], Cases[xml, XMLElement["assumptions", _, _], Infinity]}],
				Return[ minputResult[First @ moutputs]]
			]
		,
		"Content",
			replaceEvaluationCell[formatQueryBlob[query, xml, {podid, title, c, n, "Content"}, opts]];
			If[
				TrueQ[$WolframAlphaNotebook],
				Return[{
					Defer[WolframAlphaResult[##]]& @@ { query, {{podid, c}, "Content"}, Sequence @@ Cases[{opts}, _[InputAssumptions, _]] },
					Cases[xml, XMLElement["assumptions", _, _], Infinity]}
				],
				Return[ WolframAlphaResult[query, {{podid, c}, "Content"}]]
			]
		,
		_ (* data exposure *),
			str = With[{c=c, returntype=returntype}, ToString[Unevaluated[WolframAlpha[query, {{podid, c}, returntype}]], InputForm]];
			replaceEvaluationCell[formatQueryBlob[query, xml, {podid, title, c, n, returntype, str}, opts]];
			If[
				TrueQ[$WolframAlphaNotebook],
				Return[{ToExpression[str, InputForm, Defer], Cases[xml, XMLElement["assumptions", _, _], Infinity]}],
				Return[	ToExpression[str]]
			]
	]
];


replaceEvaluationCell[expr_] := LinkWrite[$ParentLink, FrontEnd`RewriteExpressionPacket[queryBlobCell[expr]]]


extrudeFromFastParse[query_String, Defer[expr_], label_String] :=
Block[{results},
	results =
		WolframAlpha[query,
			Asynchronous -> All,
			AppearanceElements -> {"Extrusion", "Warnings", "Assumptions", "Pods", "Unsuccessful"},
			Method -> {
				"ExtrusionChosen" -> {label, label, 1, 0, "Input", ToString[Unevaluated[expr], InputForm]},
				"ExtrusionOpen" -> True,
				"Formats" -> Flatten[{"cell", "minput", If[$AlphaQueryExtrusionShowMOutputs, "moutput", {}, {}], "msound", "dataformats"}] }
			];
	If[results =!= $Failed,
		SelectionMove[ButtonNotebook[], All, ButtonCell];
		NotebookWrite[ButtonNotebook[], queryBlobCell[results], All];
	];
]


queryBlobCell[a_Cell] := a;


queryBlobCell[expr_] := Cell[BoxData[ToBoxes[expr]], "WolframAlphaShortInput"]


formatQueryBlob[query_String, None, label_] := 
	Block[{grid},
		grid[Dynamic[q_], Dynamic[newq_]] := innerFrame[
			Grid[{{
					queryBlobInputField[Dynamic[newq]],
					SpanFromLeft
				},
				{
					Style[label, "DialogStyle", FontColor -> Gray],
					suggestionsDialogButton["NoResults", query]
				}},
				Alignment -> Left,
				Dividers -> {False, Center},
				FrameStyle -> LightGray,
				BaselinePosition -> {1,1}
			],
			RoundingRadius -> 5,
			FrameStyle -> LightGray,
			BaselinePosition -> Baseline,
			ImageSize -> Automatic,
			Background -> queryBlobBackground[Dynamic[q], Dynamic[newq]]
		];

		System`DynamicNamespace[
			"WolframAlphaQueryNoResults",
			DynamicModule[{Typeset`q = query, Typeset`newq = query},
				grid[Dynamic[Typeset`q], Dynamic[Typeset`newq]]
			],
			BaseStyle -> {Deployed -> True},
			Editable -> False,
			DeleteWithContents -> True,
			SelectWithContents -> True
		]
	]

formatQueryBlob[query_String, Defer[expr_], label_String, parseassumptions_] :=
	Block[{grid},
		grid[Dynamic[q_], Dynamic[newq_], Dynamic[open_]] := 
			DynamicModule[{Typeset`show = False, Typeset`assumptionsQ = TrueQ[parseassumptions]},
				EventHandler[
					innerFrame[
						Grid[{{
								queryBlobInputField[Dynamic[newq]],
								If[Typeset`assumptionsQ,
									Button[
										Tooltip[
											Style["\[RightGuillemet]", Orange],
											Dynamic[FEPrivate`FrontEndResource["WAStrings", "InterpretationAssumptions"]]
										],
										Quiet[WolframAlpha[]]; (* trigger autoloading *)
										open = True;
										extrudeFromFastParse[query, Defer[expr], label];
										open = False,
										Appearance -> None,
										Method -> "Queued",
										BaselinePosition -> Baseline,
										ContentPadding -> False
									],
									Unevaluated[Sequence[]]
								],
								Item[
									Button[
										PaneSelector[{
											True -> Tooltip[Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "OrangeSquarePlus"]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowAllResults"]]],
											False -> Tooltip[Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "GraySquarePlus"]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowAllResults"]]]
											},
											Dynamic[Typeset`show],
											ImageSize -> All
										],
										Quiet[WolframAlpha[]]; (* trigger autoloading *)
										If[CurrentValue["OptionKey"] || CurrentValue["AltKey"],
											SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
											NotebookWrite[ButtonNotebook[], Cell[newq, "WolframAlphaShort", FormatType -> "TextForm"], All],
											(* else *)
											open = True;
											extrudeFromFastParse[query, Defer[expr], label];
											open = False
										],
										Appearance -> None,
										Method -> "Queued"
									],
									Alignment -> {Right, Top}
								]
							},
							{
								queryBlobMathematicaForm[Defer[expr]],
								If[Typeset`assumptionsQ, SpanFromLeft, Unevaluated[Sequence[]]],
								SpanFromLeft
							}},
							Alignment -> Left,
							Dividers -> {False, Center},
							FrameStyle -> LightGray,
							BaselinePosition -> {1,1}
						],
						RoundingRadius -> 5,
						FrameStyle -> LightGray,
						BaselinePosition -> Baseline,
						ImageSize -> Automatic,
						Background -> queryBlobBackground[Dynamic[q], Dynamic[newq]]
					],
					{"MouseEntered" :> FEPrivate`Set[Typeset`show, True], "MouseExited" :> FEPrivate`Set[Typeset`show, False]}
				]
			];

		System`DynamicNamespace[
			"WolframAlphaQueryParseResults",
			DynamicModule[{Typeset`q = query, Typeset`newq = query, Typeset`chosen = ToString[Unevaluated[expr], InputForm], Typeset`open = False},
				PaneSelector[{
						False -> grid[Dynamic[Typeset`q], Dynamic[Typeset`newq], Dynamic[Typeset`open]],
						True -> Column[{grid[Dynamic[Typeset`q], Dynamic[Typeset`newq], Dynamic[Typeset`open]], necklaceAnimator[20]}, BaselinePosition -> {1,1}]
					},
					Dynamic[TrueQ[Typeset`open]], (* Dynamic made purposely complex so it will update during queued evaluations *)
					ImageSize -> Automatic,
					BaseStyle -> {Deployed -> True}
				]
			],
			BaseStyle -> {Deployed -> True},
			Editable -> False,
			DeleteWithContents -> True,
			SelectWithContents -> True
		]
	]


formatQueryBlob[query_String, xml_, chosen_, opts___] :=
	formatAlphaXML[xml, query, Method -> {"ExtrusionChosen" -> chosen}, opts]


(*
The fastParseResult utility might someday be expanded to heuristically either evaluate
locally or not, depending on what kind of things are in expr.
*)
fastParseResult[HoldComplete[expr_]] := If[FreeQ[HoldComplete[expr], _Placeholder], expr, Null]


(*
This input may return literal references to % or Placeholder, when appropriate
*)
minputInput[str_] := 
	Apply[
		Defer,
		ToExpression[str, InputForm, HoldComplete] /. {
			Hold[a_] :> a,
			HoldForm[a_] :> a,
			Defer[a_] :> a
		}
	]

minputResult[str_] := First[minputInput[str] // suppressPlaceholderMInputs]


WolframAlphaResult /:
MakeBoxes[ WolframAlphaResult[q_, id: {{podid_, subpodid_}, type_}, opts___], fmt_] :=
With[{xmlcache =
	Replace[$cachedWAResult, {{{q, id}, xml_} :> {"ProcessedXML" -> xml}, _ :> {}}]},
	$cachedWAResult = {};
	ToBoxes[
		Interpretation[
			Framed[
				WolframAlpha[q, id, opts,
					Method -> xmlcache],
				FrameStyle -> LightGray,
				RoundingRadius -> 5,
				FrameMargins -> 10,
				BaseStyle -> {Plain}
			],
			WolframAlphaResult[q, id, opts]
		],
		fmt
	]
]


(* Full pod version added for occasional ANB use: *)
WolframAlphaResult /:
MakeBoxes[ WolframAlphaResult[q_, podid_String, opts___], fmt_] :=
With[{xmlcache =
	Replace[$cachedWAResult, {{{q, podid}, xml_} :> {"ProcessedXML" -> xml}, _ :> {} }]},
	$cachedWAResult = {};
	ToBoxes[
		Interpretation[
			WolframAlpha[q, IncludePods -> {podid}, opts,
				AppearanceElements -> {"Pods"}, Method -> xmlcache],
			WolframAlphaResult[q, podid, opts]
		],
		fmt
	]
]


(* ::Subsection::Closed:: *)
(*encoding*)


(*
Use the System` context functions for encoding and decoding URL strings.
*)


urlencode[str_] := System`URLEncode[str]

urldecode[str_] := System`URLDecode[str]


(* ::Subsection::Closed:: *)
(*attaching to '='*)


(* 
GetFEKernelInit.tr sets up the bindings for
FrontEnd`Private`EvaluationModeEvaluate[_, _, "WolframAlphaShort"], aka Single-Equal, and
FrontEnd`Private`EvaluationModeEvaluate[_, _, "WolframAlphaLong"], aka Double-Equal.
*)



(* ::Subsection::Closed:: *)
(*Interpretation rules*)


(* What about version numbers ??? *)


Unprotect[NamespaceBox]; (* Autoload code has a tendency to reset attributes during package loading *)


NamespaceBox /: MakeExpression[NamespaceBox["WolframAlphaQueryNoResults", boxes_, ___?OptionQ], fmt_] := 
Block[{expr, q, newq},
	expr = MakeExpression[boxes, fmt];
	If[!MatchQ[expr, HoldComplete[DynamicModule[{___}, __]]], Return[expr]];
	
	expr = Extract[expr, {1, 1}, HoldComplete];
	q = Replace[expr, {HoldComplete[{___, _[Typeset`q, q_String], ___}] :> q, _ -> $Failed}];
	newq = Replace[expr, {HoldComplete[{___, _[Typeset`newq, newq_String], ___}] :> newq, _ -> $Failed}];
	
	Switch[StringQ[newq] && newq =!= q,
		True, returnExtrusionEvaluate[newq, {}, {}],
		_, HoldComplete[Null]
	]
]


NamespaceBox /: MakeExpression[NamespaceBox["WolframAlphaQueryParseResults", boxes_, ___?OptionQ], fmt_] :=
Block[{expr, q, newq},
	expr = MakeExpression[boxes, fmt];
	If[!MatchQ[expr, HoldComplete[DynamicModule[{___}, __]]], Return[expr]];
	
	expr = Extract[expr, {1, 1}, HoldComplete];
	q = Replace[expr, {HoldComplete[{___, _[Typeset`q, q_String], ___}] :> q, _ -> $Failed}];
	newq = Replace[expr, {HoldComplete[{___, _[Typeset`newq, newq_String], ___}] :> newq, _ -> $Failed}];
	chosen = Replace[expr, {HoldComplete[{___, _[Typeset`chosen, chosen_String], ___}] :> chosen, _ -> $Failed}];
	
	Switch[{StringQ[newq] && newq =!= q, chosen},
		{True, _}, returnExtrusionEvaluate[newq, {}, {}],
		{_, mform_String}, ToExpression[chosen, InputForm, HoldComplete],
		{_, _}, HoldComplete[Null]
	]
]


NamespaceBox /: MakeExpression[NamespaceBox["WolframAlphaQueryResults", boxes_, ___?OptionQ], fmt_] :=
Block[{expr, q, opts, chosen, newq},
	expr = MakeExpression[boxes, fmt];
	If[!MatchQ[expr, HoldComplete[DynamicModule[{___}, __]]], Return[expr]];
	
	expr = Extract[expr, {1, 1}, HoldComplete];
	q = Replace[expr, {HoldComplete[{___, _[Typeset`q, q_String], ___}] :> q, _ -> $Failed}];
	opts = Replace[expr, {HoldComplete[{___, _[Typeset`opts, opts_List], ___}] :> opts, _ -> $Failed}];
	chosen = Replace[expr, {HoldComplete[{___, _[Typeset`chosen, chosen_List], ___}] :> chosen, _ -> $Failed}];
	newq = Replace[expr, {HoldComplete[{___, _[Typeset`newq, newq_String], ___}] :> newq, _ -> $Failed}];
	
	Switch[{StringQ[newq] && newq =!= q, chosen},
		{True, _}, returnExtrusionEvaluate[newq, opts, chosen],
		{_, {podid_, title_, c_, n_, "Input" | "Output", mform_String}}, returnMathematicaForm[q, opts, chosen],
		{_, {podid_, title_, c_, n_, dataexposure_, mform_String}}, ToExpression[chosen[[6]], InputForm, HoldComplete],
		{_, {podid_, title_, c_, n_, "Content"}}, returnSingleResult[q, opts, chosen],
		{_, {} | _}, returnWolframAlphaExpression[q, opts]
	]
]



(*
The contract stipulates: Any input that contains a Placeholder expression is deemed unfit for
evaluation, and so should be suppressed so that it doesn't trigger gratuitous error messages.
*)
suppressPlaceholderMInputs[heldexpr_] := heldexpr /; FreeQ[heldexpr, _Placeholder]
suppressPlaceholderMInputs[other_] := HoldComplete[Null]


returnMathematicaForm[q_, opts_, chosen_] /; $AlphaQueryExtrusionEvaluateCachedMForms :=
	ToExpression[chosen[[6]], InputForm, HoldComplete] // suppressPlaceholderMInputs


returnMathematicaForm[q_, opts_, chosen_] := 
	With[{options = Sequence @@ Cases[opts, _[InputAssumptions | PodStates | Method, _]]},
		WolframAlpha[q, chosen[[{1,3,5}]], options] // suppressPlaceholderMInputs
	]


returnExtrusionEvaluate[q_, opts_, chosen_] :=
	With[{options = Sequence @@ Cases[opts, _[InputAssumptions | PodStates | Method, _]]},
		HoldComplete[AlphaIntegration`ExtrusionEvaluate[q, InputForm, options]]
	]


returnSingleResult[q_, opts_, chosen:{podid_, title_, subpodid_, n_, type_, ___}] :=
	With[{options = Sequence @@ Cases[opts, _[InputAssumptions | PodStates, _]]},
		HoldComplete[WolframAlphaResult[q, {{podid, subpodid}, type}, options]]
	]


returnWolframAlphaExpression[q_, {opts___}] := HoldComplete[WolframAlpha[q, opts]]






(* ::Subsection::Closed:: *)
(*LinguisticAssistant[], aka Control-Equal*)


$LinguisticAssistantQuantityQuery = True;

$LinguisticAssistantCompact = True;

$LinguisticAssistantTemplateBox = BoxForm`sufficientVersionQ[13.1];

$LinguisticAssistantTemplateBoxOptions = Not @ BoxForm`sufficientVersionQ[13.1];

$LinguisticAssistantTimeout = 5.;

$LinguisticAssistantRetryTimeout = 2 * $LinguisticAssistantTimeout;

$LinguisticAssistantUsesURLSaveAsynchronous = True;

$LinguisticAssistantTrackQueryState = True;

$LinguisticAssistantEntityDisplay = True;

$LinguisticAssistantSideEffects = True;

$CloudControlEqualOverride = False;

$CloudControlEqual := TrueQ[$CloudControlEqualOverride || CloudSystem`$CloudNotebooks];


$LinguisticAssistantBoxReference := 
	With[{
			(* reference for non-TemplateBox-based control-equal: *)
			old = FE`BoxReference[MathLink`CallFrontEnd[FrontEnd`Value[FEPrivate`Self[]]],
				{FE`Parent["LinguisticAssistant"]}],
			(* reference for TemplateBox-based control-equal: *)
			new = FE`BoxReference[MathLink`CallFrontEnd[FrontEnd`Value[FEPrivate`Self[]]],
				{FE`Parent["LinguisticAssistantTemplateBoxID"]}, FE`BoxOffset -> {FE`BoxParent[2]}]
		},
		If[TrueQ[MathLink`CallFrontEnd[FrontEnd`BoxReferenceExists[old]]], old, new]
	];


AlphaIntegration`CloudControlEqualBoxes[query_String : "", assumptions_ : {}] :=
	ToBoxes[AlphaIntegration`LinguisticAssistant[query, assumptions, Automatic]]

AlphaIntegration`CloudControlEqualPrint[query_String : "", assumptions_ : {}] :=
	CellPrint[
		Cell[BoxData[AlphaIntegration`CloudControlEqualBoxes[query, assumptions]], "Output"]]



(*
When the argument is a notebook object, we decide whether to insert a 
new linguistic assistant at the insertion point.
*)


AlphaIntegration`LinguisticAssistant[nb_NotebookObject] := 
Block[{info, content, boxes},
	info = MathLink`CallFrontEnd[FrontEnd`CellInformation[nb]];
	(* If there are too many cells in the selection, don't do anything *)
	If[Length[info] > 1,
		Message[WolframAlpha::noiffi];
		Return @ $Failed
	];
	(* If the insertion point is horizontal, insert a typeset input cell and go into Alpha mode *)
	If[Length[info] === 0,
		NotebookWrite[nb, Cell[BoxData[""], "Input"], All];
		SelectionMove[nb, Before, CellContents];
		Return @ doEnterLinguisticAssistant[nb]
	];
	
	info = First[info];

	(* If we're already in a Linguistic Assistant, do nothing, silently *)
	If[
		!FreeQ[MathLink`CallFrontEnd[FrontEnd`GetBoxIDs[nb]], "LinguisticAssistantID"],
		Return @ Null
	];

	(* If we're inside a typeset cell and nothing is selected, enter a new Linguistic Assistant *)
	If[
		MemberQ[info, "ContentData" -> BoxData] &&
		MemberQ[info, "Formatted" -> True] &&
		MemberQ[info, "CursorPosition" -> {n_, n_}],
		Return @ doEnterLinguisticAssistant[nb]
	];

	(* If we're inside a typeset cell with a selection, enter a populated Linguistic Assistant *)
	If[
		MemberQ[info, "ContentData" -> BoxData] &&
		MemberQ[info, "Formatted" -> True] && 
		MemberQ[info, "CursorPosition" -> {m_, n_}] &&
		If[MatchQ[boxes = NotebookRead[nb],
			"\[Placeholder]" | "\[SelectionPlaceholder]" | TagBox[_, "Placeholder" | "SelectionPlaceholder", ___]],
			content = "", 
			content = First[MathLink`CallFrontEnd[FrontEnd`ExportPacket[BoxData[boxes], "InputText"]]]];
		StringQ[content] && 
		StringLength[content] < 200,
		Return @ doEnterLinguisticAssistant[nb, content]
	];

	(* Otherwise, the selection is not supported *)
	Message[WolframAlpha::noiffi];
	(* Beep[] *)
]


doEnterLinguisticAssistant[nb_, str_:""] := 
Module[{contextinfo, head},
	contextinfo = CurrentValue[InputNotebook[], "CodeContext"];
	contextinfo = Switch[contextinfo,
		{"EntityValue", 2},
			{"CodeContext" -> {"EntityValue", 2}},
		{_String, _},
			Automatic,
		(* For Entity[...][control-equal], parse the head to know what the Entity actually is. *)
		{boxes_, 1},
			If[FreeQ[First @ contextinfo, "Entity"],
				Automatic,
				head = MakeExpression[First @ contextinfo];
				If[MatchQ[head, HoldComplete[Entity[_String, _]]],
					{"CodeContext" -> {head, 1}},
					Automatic
				]
			],
		_,
			Automatic
	];
	NotebookWrite[nb, ToBoxes[AlphaIntegration`LinguisticAssistant[str, {}, contextinfo]], After];
	FrontEndTokenExecute[nb, "MoveNextPlaceHolder"];
]



(* Resources for resolving context-sensitive control-equal (CSCE) behaviors and displays *)

(* Support for "CodeContext" -> {"EntityValue", 2} *)

$entityPropertyContextPat = {___, "CodeContext" -> {"EntityValue", 2}, ___};

$curriedEntityContextPat = {___, "CodeContext" -> {HoldComplete[Entity[_String ,_]], 1}, ___}


CSCEParse[query_, contextinfo: $entityPropertyContextPat] := 
	Module[{list},
		list = Internal`MWACompute["MWAEntityPropertyParse", {query}];
		list = Cases[list, EntityProperty[type_String, prop_String, ___] :> {type, prop}, {2}];
		DeleteDuplicates @ Flatten @ {Cases[list, {$PreviousEntityType, prop_} :> prop], list[[All,2]]}
	]

(* If we know what Entity is the Head, ignore properties for other entity types. 431975 *)
CSCEParse[query_, contextinfo: {___, "CodeContext" -> {HoldComplete[Entity[type_String, _]], 1}, ___}] := 
	Module[{list},
		list = Internal`MWACompute["MWAEntityPropertyParse", {query}];
		list = Cases[list, EntityProperty[type, prop_String, ___] :> prop, {2}];
		DeleteDuplicates[list]
	]

csceResources[$entityPropertyContextPat | $curriedEntityContextPat] := {
	"NoCompactAlphaIcon" -> bitmapResource["NoCompactAlphaForm", BaselinePosition->(Bottom -> Baseline)],
	"NoCompactAlphaTooltip" -> stringResource["EntityPropertyNoTranslations"],
	"CompactAlphaIcon" -> bitmapResource["CompactAlphaForm", BaselinePosition->(Bottom -> Baseline)],
	"CompactAlphaTooltip" -> stringResource["EntityPropertyLinguisticFormToMathematica"],
	"CompactMathematicaIcon" -> bitmapResource["CompactMathematicaForm", BaselinePosition->(Bottom -> Baseline)],
	"CompactMathematicaTooltip" -> stringResource["EntityPropertyEditLinguisticForm"],
	"AssumptionsIcon" -> bitmapResource["LinguisticChoicesIcon"],
	"AssumptionsIconHot" -> bitmapResource["LinguisticChoicesIconHot"],
	"AssumptionsTooltip" -> stringResource["EntityPropertyStandardNames"]
};

(* General context-sensitive utilities / resources *)

csceResources[Automatic] :=	{
	"NoCompactAlphaIcon" -> bitmapResource["NoCompactAlphaForm", BaselinePosition->(Bottom -> Baseline)],
	"NoCompactAlphaTooltip" -> stringResource["NoTranslationsVerbose"],
	"CompactAlphaIcon" -> bitmapResource["CompactAlphaForm", BaselinePosition->(Bottom -> Baseline)],
	"CompactAlphaTooltip" -> stringResource["LinguisticFormToMathematica"],
	"CompactMathematicaIcon" -> bitmapResource["CompactMathematicaForm", BaselinePosition->(Bottom -> Baseline)],
	"CompactMathematicaTooltip" -> stringResource["EditLinguisticForm"],
	"AssumptionsIcon" -> bitmapResource["LinguisticAssumptionsIcon"],
	"AssumptionsIconHot" -> bitmapResource["LinguisticAssumptionsIcon"],
	"AssumptionsTooltip" -> stringResource["AlternateInterpretations"]
};

csceResources[unknown_] := csceResources[Automatic]

csceResource[name_, contextinfo_] := name /. Flatten[{csceResources[contextinfo], csceResources[Automatic]}]

$cscePat = Alternatives[$entityPropertyContextPat, $curriedEntityContextPat];

(* end context-sensitive control-equal resources *)



AlphaIntegration`LinguisticAssistant[str_String, chosenAssumptions:{___String}, contextinfo_] /; $LinguisticAssistantTemplateBox :=
	Module[{ Typeset`query = str, Typeset`boxes, Typeset`allassumptions = {},
			Typeset`assumptions = chosenAssumptions, Typeset`open = {1}, Typeset`querystate = {}},
		doNewFastParse[Typeset`query, contextinfo, Dynamic[Typeset`boxes], Dynamic[Typeset`allassumptions], Dynamic[Typeset`assumptions], Dynamic[Typeset`open], Dynamic[Typeset`querystate]];
		
		TemplateBox[Association[
				"query" -> Typeset`query,
				"boxes" -> Typeset`boxes,
				"allassumptions" -> Typeset`allassumptions,
				"assumptions" -> Typeset`assumptions,
				"open" -> Typeset`open,
				"querystate" -> Typeset`querystate,
				"contextinfo" -> contextinfo,
				"version" -> If[MatchQ[contextinfo, $curriedEntityContextPat], 6, 5]
			],
			"LinguisticAssistantTemplate",
			Sequence @@ If[TrueQ @ $LinguisticAssistantTemplateBoxOptions,
				{
					DisplayFunction -> (NamespaceBox["LinguisticAssistantTemplateBoxID", 
						DynamicBox[ToBoxes[
						AlphaIntegration`LinguisticAssistantBoxes[#query, #version, #contextinfo, 
							Dynamic[#query], 
							Dynamic[#boxes], 
							Dynamic[#allassumptions], 
							Dynamic[#assumptions], 
							Dynamic[#open], 
							Dynamic[#querystate]], StandardForm],
							TrackedSymbols :> {#query, #boxes, #allassumptions, #assumptions, #open, #querystate}]]&),
					InterpretationFunction -> (#boxes&),
					DynamicModuleBoxOptions -> {UndoTrackedVariables :> {FETemplate`$$open}}
				},
				{}
			]
		] // RawBoxes
	]




AlphaIntegration`LinguisticAssistant[str_String, chosenAssumptions:{___String}, contextinfo_] :=
With[{boxversion = If[TrueQ[$LinguisticAssistantTrackQueryState], 4, If[contextinfo === Automatic, 1, 3]]},
	System`DynamicNamespace[
		"LinguisticAssistant",
		DynamicModule[{ Typeset`query = str, Typeset`boxes = "None", Typeset`allassumptions = {},
						Typeset`assumptions = chosenAssumptions, Typeset`open = {1}, Typeset`querystate = {}},
			doNewFastParse[Typeset`query, contextinfo, Dynamic[Typeset`boxes], Dynamic[Typeset`allassumptions], Dynamic[Typeset`assumptions], Dynamic[Typeset`open], Dynamic[Typeset`querystate]];
			Dynamic[
				AlphaIntegration`LinguisticAssistantBoxes[##],
				TrackedSymbols :> {Typeset`query, Typeset`boxes, Typeset`allassumptions, Typeset`assumptions, Typeset`open, Typeset`querystate}
			]& @@ 
			Switch[boxversion,
				1, {str, 1, Dynamic[Typeset`query], Dynamic[Typeset`boxes], Dynamic[Typeset`allassumptions], Dynamic[Typeset`assumptions], Dynamic[Typeset`open]},
				3, {str, 3, contextinfo, Dynamic[Typeset`query], Dynamic[Typeset`boxes], Dynamic[Typeset`allassumptions], Dynamic[Typeset`assumptions], Dynamic[Typeset`open]},
				4, {str, 4, contextinfo, Dynamic[Typeset`query], Dynamic[Typeset`boxes], Dynamic[Typeset`allassumptions], Dynamic[Typeset`assumptions], Dynamic[Typeset`open], Dynamic[Typeset`querystate]}
			],
			Evaluate @ If[BoxForm`sufficientVersionQ[10], UndoTrackedVariables :> {Typeset`open}, Unevaluated[Sequence[]]]
		],
		BaseStyle -> {"Deploy"},
		Editable -> False,
		DeleteWithContents -> True,
		SelectWithContents -> True
	]] // If[$CloudControlEqual, AddCloudControlEqualTag[#], #]&



(* Use an inline cell on Cloud so we can use EvaluationCell[] to target it later *)
(*
AddCloudControlEqualTag /: MakeBoxes[AddCloudControlEqualTag[_[_, expr_DynamicModule, ___]], fmt_] :=
	Cell[BoxData[FormBox[
		TagBox[
			MakeBoxes[expr, fmt], 
			"CloudControlEqual",
			Editable -> False,
			DeleteWithContents -> True,
			SelectWithContents -> True ], fmt]],
		"Deploy",
		Background -> None,
		Evaluatable -> True
	]
*)
(* Update: The inline cell above was causing problems for round-tripping the
boxes generated on Cloud for "LinguisticAssistant". Since Cloud is no longer
using these boxes for anything (for now), the inline cell is no longer needed.
So make this utility a no-op; use the same boxes used by Desktop, and fix
round-tripping on Cloud. *)
AddCloudControlEqualTag /: MakeBoxes[AddCloudControlEqualTag[expr_], fmt_] :=
	MakeBoxes[expr, fmt]



(* if there's no contextinfo, add it *)
AlphaIntegration`LinguisticAssistantBoxes[str_String, version:(1|2), Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_]] :=
	AlphaIntegration`LinguisticAssistantBoxes[str, version, Automatic, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open]]	

(* if there's no querystate, add it *)
AlphaIntegration`LinguisticAssistantBoxes[str_String, version:(1|2|3), contextinfo_, Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_]] :=
	With[{querystate = Unique["querystate"]},
		querystate = {};
		AlphaIntegration`LinguisticAssistantBoxes[str, version, contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]]
	]


(* Single-decker *)
AlphaIntegration`LinguisticAssistantBoxes[str_String, version:(1|2|3|4|5|6), contextinfo_, Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
DynamicModule[{Typeset`emphasizeErrors = False},
	(* outer paneling *)
	Which[
		open =!= {1} && $LinguisticAssistantEntityDisplay && MatchQ[boxes, TemplateBox[_, "Entity" | "EntityClass", ___]],
		If[MatchQ[boxes, TemplateBox[_, "Entity", ___]], entityFrame[#], entityClassFrame[#]],
		$CloudControlEqual,
		Panel[
			If[MemberQ[open, 3],
				Column[{#, linguisticAssumptionsDisplay[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]]}, BaselinePosition -> {1,1}],
				#
			],
			If[
				open =!= {1} && MatchQ[boxes, "None" | InterpretationBox[_, _Failure, ___]],
				Appearance :> FEPrivate`FrontEndResource["WAExpressions", "ControlEqualErrorAppearance"],
				Appearance :> FEPrivate`FrontEndResource["WAExpressions", "ControlEqualAppearance"]
			],
			BaselinePosition -> Baseline,
			DefaultBaseStyle -> {}
		],
		True,
		Panel[#,
			Appearance :> FEPrivate`Switch[Typeset`emphasizeErrors,
				"Offline", FEPrivate`FrontEndResource["WAExpressions", "ControlEqualGrayAppearance"],
				"Disallowed", FEPrivate`FrontEndResource["WAExpressions", "ControlEqualGrayAppearance"],
				"MParseTimedOut", FEPrivate`FrontEndResource["WAExpressions", "ControlEqualGrayAppearance"],
				"QueryTimedOut", FEPrivate`FrontEndResource["WAExpressions", "ControlEqualGrayAppearance"],
				"Messages", FEPrivate`FrontEndResource["WAExpressions", "ControlEqualGrayAppearance"],
				"NoParse", FEPrivate`FrontEndResource["WAExpressions", "ControlEqualErrorAppearance"], (* <-- this is the only real pink box case *)
				_, FEPrivate`FrontEndResource["WAExpressions", "ControlEqualAppearance"]
			],
			BaselinePosition -> Baseline,
			DefaultBaseStyle -> {}
		]
	]& @ 
	
	(* inner bits *)
		Which[

		(* display for the initial query field *)
			open === {1},
			Grid[{{
				linguisticEditSwitchToExpression[query, contextinfo, Dynamic[open]],
				Pane[
					linguisticTopInputField[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
					BaselinePosition -> Baseline,
					ImageMargins -> {{0,3},{1,0}}
				]
			}}, Alignment -> If[$CloudControlEqual, {Center, Center}, Baseline], BaselinePosition -> {1,2}, Spacings -> {{0, 0.15, {0.3}}}],

		(* display used when there is no parse *)
			MatchQ[boxes, "None" | InterpretationBox[_, _Failure, ___]],
			Typeset`emphasizeErrors = Switch[querystate,
				{___, "Online" -> False, ___}, "Offline",
				{___, "Allowed" -> False, ___}, "Disallowed",
				{___, "mparse.jsp" -> _, "$TimedOut" -> _, ___}, "QueryTimedOut",
				{___, "$TimedOut" -> _, ___}, "MParseTimedOut",
				{___, "Messages" -> {__}, ___}, "Messages",
				{___, "mparse.jsp" -> _, "query.jsp" -> _, ___} | _, "NoParse"
			];
			If[Typeset`emphasizeErrors === "NoParse", Tooltip[#, csceResource["NoCompactAlphaTooltip", contextinfo]], #]& @
			Grid[{Flatten @ {
				linguisticEditSwitchFromError[Typeset`emphasizeErrors, query, contextinfo, Dynamic[open]],
				EventHandler[
					Pane[
						linguisticTopInputField[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
						BaselinePosition -> Baseline,
						ImageMargins -> {{0,3},{2,1}}
					],
					(* It would be nice if edits toggled this off too, but for now only clicking does it *)
					{"MouseDown" :> (open = {1})},
					PassEventsDown -> True
				],
				linguisticErrorIndicator[Typeset`emphasizeErrors, query, contextinfo, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
				linguisticAssumptions[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]]
			}}, Alignment -> If[$CloudControlEqual, {Center, Center}, Baseline], BaselinePosition -> {1,2}, Spacings -> {{0, 0.15, {0.3}}}],
			
		(* display for when the parse is a formatted Entity or EntityClass *)
			$LinguisticAssistantEntityDisplay && MatchQ[boxes, TemplateBox[_, "Entity" | "EntityClass", ___]],
			Grid[{Flatten @ {
				Pane[#, ImageMargins -> {{5,3},{0,0}}, BaselinePosition -> Baseline]& @
					Tooltip[#, Row[{csceResource["CompactMathematicaTooltip", contextinfo], "\"", query, "\""}]]& @
					MouseAppearance[linguisticEntitySwitchToQuery[query, boxes, Dynamic[open]], "Edit"],
				linguisticAssumptions[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
				linguisticAcceptButton[Dynamic[boxes]]
			}}, Alignment -> Baseline, BaselinePosition -> {1,2}, Spacings -> {{0, 0.15, {0.3}}}],

		(* display for when the parse is a formatted generalized entity *)
			$LinguisticAssistantEntityDisplay && (generalizedEntityLabeledBoxesQ[boxes] || implicitEntityLabeledBoxesQ[boxes]),
			Grid[{Flatten @ {
				linguisticEditSwitchToQuery[query, contextinfo, Dynamic[open]],
				linguisticBottomInputFieldGeneralizedEntity[Dynamic[boxes], Dynamic[open], True,
					Row[{csceResource["CompactMathematicaTooltip", contextinfo], "\"", query, "\""}]],
				linguisticAssumptions[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
				linguisticAcceptButton[Dynamic[boxes]]
			}}, Alignment -> If[$CloudControlEqual, {Center, Center}, Baseline], BaselinePosition -> {1,2}, Spacings -> {{0, 0.15, {0.3}}}],

		(* display for when the parse contains a formatted qualified entity property *)
			$LinguisticAssistantEntityDisplay && (qualifiedEntityPropertyBoxesQ[boxes] || implicitEntityPropertyBoxesQ[boxes]),
			Grid[{Flatten @ {
				linguisticEditSwitchToQuery[query, contextinfo, Dynamic[open]],
				linguisticBottomInputFieldQualifiedEntityProperty[Dynamic[boxes], Dynamic[open], True,
					Row[{csceResource["CompactMathematicaTooltip", contextinfo], "\"", query, "\""}]],
				linguisticAssumptions[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
				linguisticAcceptButton[Dynamic[boxes]]
			}}, Alignment -> If[$CloudControlEqual, {Center, Center}, Baseline], BaselinePosition -> {1,2}, Spacings -> {{0, 0.15, {0.3}}}],

		(* display used for all other successes *)
			True,
			Grid[{Flatten @ {
				linguisticEditSwitchToQuery[query, contextinfo, Dynamic[open]],
				Pane[#, ImageMargins -> {{0,3},{2,0}}, BaselinePosition -> Baseline]& @ 
				Tooltip[#, Row[{csceResource["CompactMathematicaTooltip", contextinfo], "\"", query, "\""}]]& @
					MouseAppearance[linguisticBottomInputField[Dynamic[boxes], Dynamic[open], True], "Edit"],
				linguisticAssumptions[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
				linguisticAcceptButton[Dynamic[boxes]]
			}}, Alignment -> If[$CloudControlEqual, {Center, Center}, Baseline], BaselinePosition -> {1,2}, Spacings -> {{0, 0.15, {0.3}}}]
		],
		BaseStyle -> {"Deploy"}
] /; $LinguisticAssistantCompact


(* Double-decker *)
AlphaIntegration`LinguisticAssistantBoxes[str_String, version:(1|2|3|4), contextinfo_, Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
	Overlay[
		{
			Framed[
				Which[
					open === {1} && MatchQ[boxes, "None" | InterpretationBox[_, _Failure, ___]] && MatchQ[allassumptions, {} | $Failed],
					linguisticTopInputField[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
			
					open === {1},
					Grid[{{
						linguisticTopInputField[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
						linguisticAssumptions[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
						linguisticOpener["Top", Dynamic[open]]
					}}, Spacings -> {{0, 0.5, 1}, 1}],
			
					open === {2},
					Grid[{{
						linguisticBottomInputField[Dynamic[boxes], Dynamic[open], False],
						linguisticOpener["Bottom", Dynamic[open]]
					}}, Spacings -> {{0, 0.5, 1}, 1}],
			
					open === {1, 2},
					Grid[{
						{
							linguisticTopInputField[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
							linguisticAssumptions[contextinfo, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]],
							linguisticCloser["Top", Dynamic[open]]
			
						},
						{
							If[MatchQ[boxes, "None" | InterpretationBox[_, _Failure, ___]],
								(* Beep[]; *)
								Style[
									Row[{
										"(",
										If[allassumptions === {},
											Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoTranslations"]],
											Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoTranslation"]]
										],
										")",
										"  ",
										suggestionsDialogButton["NoTranslations", query, "WolframParse"]	
									}],
									"DialogStyle",
									Gray
								],
								linguisticBottomInputField[Dynamic[boxes], Dynamic[open], False]
							],
							SpanFromLeft,
							linguisticCloser["Bottom", Dynamic[open]]
						}},
						Dividers -> {None, Center},
						FrameStyle -> LightGray,
						Alignment -> {Left, Baseline},
						Spacings -> {{0, 0.5, 1}, 1}
					]
				],
				FrameStyle -> LightGray,
				RoundingRadius -> 5,
				ImageMargins -> {{5, 0}, {0, 0}},
				FrameMargins -> {{6, 4}, {4, 4}}
			],
			Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "EqualSmall"]]]
		},
		{1, 2},
		1,
		Alignment -> {Left, Center}
	]


(* Unrecognized version number *)
AlphaIntegration`LinguisticAssistantBoxes[str_String, otherVersion_, ___] :=
	Framed[
		Grid[{{
			Item[TextCell[Row[{
				"Displaying this content requires a more recent version of the Wolfram System. ",
				Hyperlink["\[RightSkeleton]", $AlphaQueryBaseURLProtocol <> "www.wolfram.com/"]}]],
				Background -> GrayLevel[1],
				Frame -> 1,
				FrameStyle -> LightGray
			]}}],
		BaseStyle -> "DialogStyle",
		RoundingRadius -> 5,
		FrameStyle -> LightGray,
		Background -> GrayLevel[0.965],
		FrameMargins -> 5
	]




linguisticErrorIndicator["NoParse", query_, ___] :=
	If[$CloudControlEqual, {}, Pane[suggestionsDialogButton["NoTranslations", query, "WolframParse"], BaselinePosition -> Scaled[0.2]]]

linguisticErrorIndicator[emphasizeErrors_, query_, contextinfo_, Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
DynamicModule[{Typeset`retrying = False},
	Dynamic[If[TrueQ[Typeset`retrying],
		Pane[Animator[Appearance -> "ArcUp", ImageSize -> 20], BaselinePosition -> Scaled[0.25]],
		Tooltip[
			Button[
				Switch[emphasizeErrors,
					"Offline", bitmapResource["NetworkFailureIcon", BaselinePosition -> Scaled[0.2]],
					"Disallowed", bitmapResource["NetworkDisabledIcon", BaselinePosition -> Scaled[0.2]],
					"MParseTimedOut", bitmapResource["OtherFailureIcon", BaselinePosition -> Scaled[0.2]],
					"QueryTimedOut", bitmapResource["OtherFailureIcon", BaselinePosition -> Scaled[0.2]],
					"Messages", bitmapResource["OtherFailureIcon", BaselinePosition -> Scaled[0.2]]
				],
				If[
					(* option-click: ask to update all control-equal interfaces in this notebook *)
					CurrentValue["OptionKey"] || CurrentValue["AltKey"],
					UpdateLinguisticAssistantsDialog[ButtonNotebook[]],
					(* regular click *)
					Typeset`retrying = True;
					Block[{$LinguisticAssistantTimeout = $LinguisticAssistantRetryTimeout},
						doNewFastParse[query, contextinfo, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]]
					];
					Typeset`retrying = False;
				]
				,
				Appearance -> None,
				BaselinePosition -> Baseline,
				BaseStyle -> {},
				DefaultBaseStyle -> {},
				ImageMargins -> {{0,5},{0,0}},
				Method -> "Queued" (* interesting.... *)
			],
			Switch[emphasizeErrors,
				"Offline", cachedFrontEndResource["WAStrings", "Tooltip:Offline"],
				"Disallowed", cachedFrontEndResource["WAStrings", "Tooltip:Disallowed"],
				"MParseTimedOut", cachedFrontEndResource["WAStrings", "Tooltip:MParseTimedOut"],
				"QueryTimedOut", cachedFrontEndResource["WAStrings", "Tooltip:QueryTimedOut"],
				"Messages", cachedFrontEndResource["WAStrings", "Tooltip:Messages"]
			]
		]
	]],
	UnsavedVariables :> {Typeset`retrying}
]



linguisticAssumptionsDisplay[contextinfo_, Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
(
	(* replace the button- and actionmenu-function with the Cloud LinguisticAssistant version *)
	Column[
		Flatten[FormatAllAssumptions["Assumptions", assumptionsListToXML[allassumptions], Dynamic[query], Dynamic[query], Dynamic[{}]]],
		BaseStyle -> {"DialogStyle", ShowStringCharacters -> False}
	] /.
	HoldPattern[updateWithAssumptions][nb_, assumptioninputs_, Dynamic[q_], Dynamic[opts_]] :> 
		updateWithAssumptionsLinguisticAssistant[query, contextinfo, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate], assumptioninputs]
) /; $CloudControlEqual


linguisticAssumptions[contextinfo_, Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] := 
	If[TrueQ[$LinguisticAssistantCompact], {}, RawBoxes[""]] /; allassumptions === {}


linguisticAssumptions[contextinfo: $cscePat, Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
	Tooltip[
		ActionMenu[
			Mouseover[csceResource["AssumptionsIcon", contextinfo], csceResource["AssumptionsIconHot", contextinfo], BaselinePosition -> Scaled[0.25]],
			(# :> (boxes = ToBoxes[#]))& /@ allassumptions,
			Appearance -> None,
			BaselinePosition -> Baseline,
			ContentPadding -> False,
			DefaultBaseStyle -> {},
			DefaultMenuStyle -> {FontWeight -> Plain}
		],
		csceResource["AssumptionsTooltip", contextinfo]
	]


linguisticAssumptions[contextinfo_, Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
	Tooltip[
		Button[
			Pane[
				bitmapResource["LinguisticAssumptionsIcon"],
				BaselinePosition -> (Center -> Center),
				ImageMargins -> {{0,0},{0,0}}
			],
			If[MemberQ[open, 3], open = DeleteCases[open, 3], AppendTo[open, 3]]
			,
			Appearance -> None,
			BaselinePosition -> Baseline,
			BaseStyle -> {},
			DefaultBaseStyle -> {}
		],
		stringResource["AlternateInterpretations"]
	] /; $CloudControlEqual


linguisticAssumptions[contextinfo_, Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
	Tooltip[
		Button[
			Mouseover[bitmapResource["LinguisticAssumptionsButton"], bitmapResource["LinguisticAssumptionsButtonHot"], BaselinePosition -> Scaled[0.25]],
			Quiet[WolframAlpha[]]; (* trigger autoloading *)
			Module[{box = EvaluationBox[], cell, hpos},
				If[TrueQ[$assumptionsAttached[box]], Return[]];
				hpos = If[NumericQ[#1/#2] && #2 > 500, Scaled[#1/#2//N], Left]&[
					First[MousePosition["WindowAbsolute"]],
					First[AbsoluteCurrentValue[ButtonNotebook[], WindowSize]]
				];
				cell = Cell[BoxData[ToBoxes[
					Panel[
						Grid[{{
							With[{box = box}, DynamicModule[{}, "",
								Initialization :> ($assumptionsAttached[box] = True),
								Deinitialization :> ($assumptionsAttached[box] = False)
							]],
							Item[Button[
								bitmapResource["LinguisticAssumptionsCloseIcon"],
  								NotebookDelete[EvaluationCell[]], Appearance -> None, ImageMargins -> {{0,7},{0,7}}], Alignment -> Right]
							},
							{
							DynamicModule[{},
								(* replace the button- and actionmenu-function with the LinguisticAssistant version *)
								Pane[
									Column[Flatten[FormatAllAssumptions["ControlEqualAssumptions", assumptionsListToXML[allassumptions], Dynamic[query], Dynamic[query], Dynamic[{}]]]],
									ImageSize -> 400,
									ImageMargins -> {{0,0},{0,0}}
								] /.
								HoldPattern[updateWithAssumptions][nb_, assumptioninputs_, Dynamic[q_], Dynamic[opts_]] :> 
									updateWithAssumptionsLinguisticAssistant[query, contextinfo, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate], assumptioninputs],
								InheritScope -> True
							],
							SpanFromLeft
							}}
						],
						Appearance :> FEPrivate`FrontEndResource["WAExpressions", "ControlEqualAssumptionsAppearance"],
						FrameMargins -> {{0,0},{10,0}}
					]]],
					"DialogStyle",
					Deployed -> True,
					Evaluator -> CurrentValue["RunningEvaluator"],
					ShowSelection -> False
				];
				SelectionMove[box, All, Expression];
				MathLink`CallFrontEnd[FrontEnd`AttachCell[
					box, cell, {1, {hpos, Bottom}}, {hpos, Top},
					"ClosingActions" -> {"SelectionDeparture", "ParentChanged", "EvaluatorQuit"}
				]]
			],
			Appearance -> None,
			BaselinePosition -> Baseline,
			BaseStyle -> {},
			DefaultBaseStyle -> {},
			ContentPadding -> False,
			ImageMargins -> 0,
			FrameMargins -> 0
		],
		Dynamic[FEPrivate`FrontEndResource["WAStrings", "AlternateInterpretations"]]
	] /; $LinguisticAssistantCompact


 
linguisticAssumptions[contextinfo_, Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
	Tooltip[
		Button[
			If[TrueQ[$LinguisticAssistantCompact],
				bitmapResource["LinguisticAssumptionsIcon"],
				Mouseover[Style["\[RightGuillemet]", Orange], Style["\[RightGuillemet]", Red]]
			],
			Quiet[WolframAlpha[]]; (* trigger autoloading *)
			CreateDocument[{
					TextCell["", CellMargins -> 0],
					ExpressionCell[
						DynamicModule[{},
							(* replace the button- and actionmenu-function with the LinguisticAssistant version *)
							Column[Flatten[FormatAllAssumptions["Assumptions", assumptionsListToXML[allassumptions], Dynamic[query], Dynamic[query], Dynamic[{}]]]] /.
								HoldPattern[updateWithAssumptions][nb_, assumptioninputs_, Dynamic[q_], Dynamic[opts_]] :> 
									updateWithAssumptionsLinguisticAssistant[query, contextinfo, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate], assumptioninputs],
							InheritScope -> True
						],
						NotebookDefault,
						"DialogStyle", 
						Deployed -> True,
						ShowSelection -> True,
						CellMargins -> {{10, 10}, {0, 0}}
					],
					TextCell["", CellMargins -> 0]
				},
				Evaluator -> CurrentValue["RunningEvaluator"],
				WindowFrame -> "Palette",
				WindowSize -> {600, FitAll},
				WindowTitle -> (cachedFrontEndResource["WAStrings", "AssumptionsFor"] <> ": " <> query),
				WindowElements -> {},
				WindowFrameElements -> {"CloseBox"},
				WindowFloating -> True,
				ShowCellBracket -> False,
				Background -> Lighter[Gray, 0.9],
				ShowSelection -> False,
				Deployed -> True,
				Saveable -> False,
				WindowMargins -> {{#1, Automatic}, {Automatic, #2}} & @@ MousePosition[]
			],
			Appearance -> None,
			BaseStyle -> {},
			DefaultBaseStyle -> {},
			BaselinePosition -> If[TrueQ[$LinguisticAssistantCompact], (Center -> Center), (Axis -> Axis)]
		],
		Dynamic[FEPrivate`FrontEndResource["WAStrings", "InterpretationAssumptions"]]
	]
 

assumptionsListToXML[a_] := If[# === {}, {}, {XMLElement["assumptions", {"count" -> ToString[Length[#]]}, #]}]& @ assumptionsListToXMLElements[a]

assumptionsListToXMLElements[a_List] := Flatten[assumptionListToXMLElement /@ a]

assumptionListToXMLElement[a_List] := XMLElement["assumption", Cases[a, _[_String, _String]], valueListToXMLElements["Values" /. a]]

valueListToXMLElements[a_List] := Flatten[valueListToXMLElement /@ a]

valueListToXMLElement[a:{__Rule}] := XMLElement["value", a, {}]

valueListToXMLElement[other_] := {}


updateWithAssumptionsLinguisticAssistant[query_, contextinfo_, Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_], assumptioninputs:{__}] := 
(
	assumptions = removeDuplicateAssumptions[assumptions, assumptioninputs];
	doNewFastParse[query, contextinfo, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]];
	If[$CloudControlEqual, Null, If[$LinguisticAssistantCompact, NotebookDelete[EvaluationCell[]], NotebookClose[ButtonNotebook[]]]];
)





linguisticEditSwitchFromError[emphasizeErrors_, query_, contextinfo_, Dynamic[open_]] := 
	Tooltip[
		If[$CloudControlEqual,
			Button[
				Pane[
					csceResource["NoCompactAlphaIcon", contextinfo],
					BaselinePosition -> Baseline
				],
				open = {1},
				Appearance -> None,
				BaseStyle -> {},
				DefaultBaseStyle -> {},
				ContentPadding -> False,
				BaselinePosition -> Baseline
			],
			Switch[emphasizeErrors, 
				"NoParse", csceResource["NoCompactAlphaIcon", contextinfo],
				_, csceResource["CompactMathematicaIcon", contextinfo]
			]
		],
		csceResource["NoCompactAlphaTooltip", contextinfo]
	]


linguisticEntitySwitchToQuery[query_, TemplateBox[arg_, style_, opts___], Dynamic[open_]] := 
Module[{boxes, hotboxes},
	boxes = makeFramelessEntity[TemplateBox[arg, style, opts], False];
	hotboxes = makeFramelessEntity[TemplateBox[arg, style, opts], True];
	
	Button[Mouseover[RawBoxes[boxes], RawBoxes[hotboxes]],
		doSwitchToQuery[Dynamic[open]],
		Appearance -> None,
		BaselinePosition -> Baseline,
		BaseStyle -> {},
		DefaultBaseStyle -> {},
		ContentPadding -> True,
		FrameMargins -> {{0,0},{2,3}},
		ImageSize -> Automatic,
		Method -> "Queued",
		Tooltip -> ToBoxes["Edit free-form input: \"" <> query <> "\""]
	]
]



$framelessEntityStyles := $framelessEntityStyles = ($Notebooks && CurrentValue[{StyleDefinitions, "EntityFrameless"}] =!= {})
$frameEntityStyles := $frameEntityStyles = ($Notebooks && CurrentValue[{StyleDefinitions, "EntityFrame"}] =!= {})
$generalizedEntityLabeledStyles := $generalizedEntityLabeledStyles = ($Notebooks && CurrentValue[{StyleDefinitions, "GeneralizedEntityToggleLabeled"}] =!= {})
$implicitEntityLabeledStyles := $implicitEntityLabeledStyles = ($Notebooks && CurrentValue[{StyleDefinitions, "ImplicitEntityToggleLabeled"}] =!= {})

generalizedEntityLabeledBoxesQ[boxes_] := MatchQ[boxes, InterpretationBox[DynamicModuleBox[_, TemplateBox[{_, _, _, _, _}, "GeneralizedEntityToggleLabeled", ___]], (_Entity | _EntityClass), ___]]

implicitEntityLabeledBoxesQ[boxes_] := MatchQ[boxes, InterpretationBox[DynamicModuleBox[_, TemplateBox[{_, _, _, _, _}, "ImplicitEntityToggleLabeled", ___]], (_Entity | _EntityClass), ___]]

qualifiedEntityPropertyBoxesQ[boxes_] := !FreeQ[boxes, InterpretationBox[DynamicModuleBox[_, TemplateBox[{_, _, _, _}, "EntityPropertyToggle"], ___], _EntityProperty, ___]]

implicitEntityPropertyBoxesQ[boxes_] := !FreeQ[boxes, InterpretationBox[DynamicModuleBox[_, TemplateBox[{_, _, _, _, _}, "ImplicitEntityPropertyToggle"], ___], _EntityProperty, ___]]



(* Add a label to generalized entities *)
transformControlEqualBoxes[InterpretationBox[DynamicModuleBox[vars_, TemplateBox[{base_, props_, d_, tip_}, "GeneralizedEntityToggle"]], ent: (_Entity | _EntityClass), opts___], query_] :=
	With[{label = ToBoxes[Row[{"(", query, ")"}]], dispFunc = DisplayFunction -> generalizedEntityDisplayFunction},
		If[TrueQ[$generalizedEntityLabeledStyles],
			InterpretationBox[DynamicModuleBox[vars, TemplateBox[{base, props, d, tip, label}, "GeneralizedEntityToggleLabeled"]], ent, opts],
			InterpretationBox[DynamicModuleBox[vars, TemplateBox[{base, props, d, tip, label}, "GeneralizedEntityToggleLabeled", dispFunc]], ent, opts]
		]
	]
(* Add a label to implicit entities *)
transformControlEqualBoxes[InterpretationBox[DynamicModuleBox[vars_, TemplateBox[{base_, props_, d_, tip_}, "ImplicitEntityToggle"]], ent: (_Entity | _EntityClass), opts___], query_] :=
	With[{label = ToBoxes[Row[{"(", query, ")"}]], dispFunc = DisplayFunction -> implicitEntityDisplayFunction},
		If[TrueQ[$implicitEntityLabeledStyles],
			InterpretationBox[DynamicModuleBox[vars, TemplateBox[{base, props, d, tip, label}, "ImplicitEntityToggleLabeled"]], ent, opts],
			InterpretationBox[DynamicModuleBox[vars, TemplateBox[{base, props, d, tip, label}, "ImplicitEntityToggleLabeled", dispFunc]], ent, opts]
		]
	]
(* fall-through case *)
transformControlEqualBoxes[other_, query_] := other


(* When connected to earlier front ends, we'll need to include an explicit DisplayFunctions *)
generalizedEntityDisplayFunction = (FrameBox[
   PaneSelectorBox[{True -> GridBox[{{
         TooltipBox[#, #4], 
         SetterBox[#3, {False}, 
          DynamicBox[
           FEPrivate`ImportImage[
            FrontEnd`ToFileName[{"Typeset", "Entity"}, "Closer.png"]], 
           ImageSizeCache -> {12., {4., 8.}}], Appearance -> None, 
          BaselinePosition -> (Scaled[0.15] -> Baseline), ContentPadding -> False, FrameMargins -> 
          0]}, {#2, "\[SpanFromLeft]"}, {
         StyleBox[#5, "GeneralizedEntityLabel", FontFamily->"Helvetica",
 FontSize->11,
 FontWeight->"Plain",
 FontColor->GrayLevel[0.6]], "\[SpanFromLeft]"}}, 
       GridBoxAlignment -> {"Columns" -> {{Left}}}, 
       GridBoxSpacings -> {"Columns" -> {{0.5}}, "Rows" -> {{0}}}, BaselinePosition -> {1, 1}], False -> 
     GridBox[{{
         TooltipBox[#, #4], 
         SetterBox[#3, {True}, 
          DynamicBox[
           FEPrivate`ImportImage[
            FrontEnd`ToFileName[{"Typeset", "Entity"}, "Opener.png"]], 
           ImageSizeCache -> {12., {4., 8.}}], Appearance -> None, 
          BaselinePosition -> (Scaled[0.15] -> Baseline), ContentPadding -> False, FrameMargins -> 0]}, {
         StyleBox[#5, "GeneralizedEntityLabel", FontFamily->"Helvetica",
 FontSize->11,
 FontWeight->"Plain",
 FontColor->GrayLevel[0.6]], "\[SpanFromLeft]"}}, BaselinePosition -> {1, 1}, 
       GridBoxAlignment -> {"Columns" -> {{Left}}}, 
       GridBoxSpacings -> {"Columns" -> {{0.5}}, "Rows" -> {{0}}}]}, #3, BaselinePosition -> Baseline, 
    ImageSize -> Automatic], DefaultBaseStyle -> "GeneralizedEntityFrame", BaselinePosition -> Baseline,
    BaseStyle -> {FontFamily -> "Courier"}]& )

implicitEntityDisplayFunction = (FrameBox[
   PaneSelectorBox[{True -> GridBox[{{
         TooltipBox[#, #4], 
         SetterBox[#3, {False}, 
          DynamicBox[
           FEPrivate`ImportImage[
            FrontEnd`ToFileName[{"Typeset", "Entity"}, "Closer.png"]], 
           ImageSizeCache -> {12., {4., 8.}}], Appearance -> None, 
          BaselinePosition -> (Scaled[0.15] -> Baseline), ContentPadding -> False, FrameMargins -> 0]}, {
         PaneBox[#2, BaselinePosition -> Baseline, ImageMargins -> {{0, 0}, {0, 5}}], 
         "\[SpanFromLeft]"}, {
         StyleBox[#5, "ImplicitEntityLabel", FontFamily -> "Helvetica", FontSize -> 11, FontWeight -> 
          "Plain", FontColor -> GrayLevel[0.6]], "\[SpanFromLeft]"}}, 
       GridBoxAlignment -> {"Columns" -> {{Left}}}, BaselinePosition -> {1, 1}, 
       GridBoxSpacings -> {"Columns" -> {{Automatic}}, "Rows" -> {{0.5}}}, 
       GridBoxDividers -> {"Columns" -> {{False}}, "Rows" -> {False, 
           RGBColor[0.789063, 0.789063, 0.789063]}}], False -> GridBox[{{
         TooltipBox[#, #4], 
         SetterBox[#3, {True}, 
          DynamicBox[
           FEPrivate`ImportImage[
            FrontEnd`ToFileName[{"Typeset", "Entity"}, "Opener.png"]], 
           ImageSizeCache -> {12., {4., 8.}}], BaselinePosition -> (Scaled[0.15] -> Baseline), 
          Appearance -> None, ContentPadding -> False, FrameMargins -> 0]}, {
         StyleBox[#5, "ImplicitEntityLabel", FontFamily -> "Helvetica", FontSize -> 11, FontWeight -> 
          "Plain", FontColor -> GrayLevel[0.6]], "\[SpanFromLeft]"}}, BaselinePosition -> {1, 1}, 
       GridBoxAlignment -> {"Columns" -> {{Left}}}, 
       GridBoxSpacings -> {"Columns" -> {{0.5}}, "Rows" -> {{0}}}]}, #3, BaselinePosition -> Baseline, 
    ImageSize -> Automatic, BaseStyle -> {FontFamily -> Dynamic[
        CurrentValue[{StyleHints, "CodeFont"}]]}], DefaultBaseStyle -> "GeneralizedEntityFrame", 
   BaselinePosition -> Baseline, BaseStyle -> {FontFamily -> "Courier", FontWeight -> "Plain"}]& )


entityFrame /: MakeBoxes[entityFrame[expr_], fmt_] := 
	If[TrueQ[$frameEntityStyles], TemplateBox[{MakeBoxes[expr, fmt]}, "EntityFrame"], MakeBoxes[genericEntityFrame[expr], fmt]]

entityClassFrame /: MakeBoxes[entityClassFrame[expr_], fmt_] := 
	If[TrueQ[$frameEntityStyles], TemplateBox[{MakeBoxes[expr, fmt]}, "EntityClassFrame"], MakeBoxes[genericEntityFrame[expr], fmt]]

genericEntityFrame /: MakeBoxes[genericEntityFrame[expr_], fmt_] := 
	MakeBoxes[
		Framed[expr,
			Background -> RGBColor[1., 0.980392, 0.921569],
			BaselinePosition -> Baseline,
			FrameMargins -> 0,
			FrameStyle -> RGBColor[1., 0.504768, 0.],
			RoundingRadius -> 4,
			BaseStyle -> {FontFamily -> "Courier"}
		],
		fmt
	]


makeFramelessEntity[TemplateBox[arg_, style: ("Entity" | "EntityClass"), opts___], hotQ_] :=
If[TrueQ[$framelessEntityStyles],
	If[hotQ,
		TemplateBox[arg, style <> "FramelessHot", opts],
		TemplateBox[arg, style <> "Frameless", opts]
	],
	(* otherwise, take the styles that would be in Core.nb, and inject them manually here.... *)
	Switch[{style, hotQ},
		{"Entity", False},
		TemplateBox[arg, "EntityFrameless", DisplayFunction -> (TooltipBox[
		   PaneSelectorBox[{True -> GridBox[{{
				 StyleBox[#, FontColor -> RGBColor[0.395437, 0.20595, 0.061158]], 
				 StyleBox[
				  RowBox[{"(", #4, ")"}], FontWeight -> "Plain", FontColor -> GrayLevel[0.65], Selectable -> False]}}, 
			   GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}}, GridBoxSpacings -> {"Columns" -> {{0.2}}, "Rows" -> {{0}}}, 
			   BaselinePosition -> {1, 1}], False -> 
			 PaneBox[#, BaseStyle -> {FontColor -> RGBColor[0.395437, 0.20595, 0.061158]}, BaselinePosition -> Baseline]}, 
			Dynamic[
			 CurrentValue[Evaluatable]], ImageSize -> Automatic, BaselinePosition -> Baseline, BaseStyle -> {FontFamily -> "Helvetica"}], #3, 
		   BaseStyle -> {ShowStringCharacters -> False, LineIndent -> 0, PrivateFontOptions -> {"OperatorSubstitution" -> False}}]& ),
		   opts
		]
		,
		{"Entity", True},
		TemplateBox[arg, "EntityFramelessHot", DisplayFunction -> (
		   PaneSelectorBox[{True -> GridBox[{{
				 StyleBox[#, FontColor -> GrayLevel[0]], 
				 StyleBox[
				  RowBox[{"(", #4, ")"}], FontWeight -> "Plain", FontColor -> GrayLevel[0], Selectable -> False]}}, 
			   GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}}, GridBoxSpacings -> {"Columns" -> {{0.2}}, "Rows" -> {{0}}}, 
			   BaselinePosition -> {1, 1}], False -> 
			 PaneBox[#, BaseStyle -> {FontColor -> GrayLevel[0]}, BaselinePosition -> Baseline]}, 
			Dynamic[
			 CurrentValue[Evaluatable]], ImageSize -> Automatic, BaselinePosition -> Baseline, BaseStyle -> {FontFamily -> "Helvetica", 
		     ShowStringCharacters -> False, LineIndent -> 0, PrivateFontOptions -> {"OperatorSubstitution" -> False}}]& ),
		   opts
		]
		,
		{"EntityClass", False},
		TemplateBox[arg, "EntityClassFrameless", DisplayFunction -> (TooltipBox[
		   PaneSelectorBox[{True -> GridBox[{{
				 PaneBox[
				  DynamicBox[
				   FEPrivate`ImportImage[
					FrontEnd`ToFileName[{"Typeset", "Entity"}, "EntityClass.png"]], ImageSizeCache -> {8., {2., 6.}}], BaselinePosition -> 
				  Bottom], 
				 StyleBox[#, FontColor -> RGBColor[0.395437, 0.20595, 0.061158]], 
				 StyleBox[
				  RowBox[{"(", #4, ")"}], FontWeight -> "Plain", FontColor -> GrayLevel[0.65], Selectable -> False]}}, 
			   GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}}, GridBoxSpacings -> {"Columns" -> {{0.2}}, "Rows" -> {{0}}}, 
			   BaselinePosition -> {1, 2}], False -> GridBox[{{
				 PaneBox[
				  DynamicBox[
				   FEPrivate`ImportImage[
					FrontEnd`ToFileName[{"Typeset", "Entity"}, "EntityClass.png"]], ImageSizeCache -> {8., {2., 6.}}], BaselinePosition -> 
				  Bottom], 
				 StyleBox[#, FontColor -> RGBColor[0.395437, 0.20595, 0.061158]]}}, 
			   GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}}, GridBoxSpacings -> {"Columns" -> {{0.2}}, "Rows" -> {{0}}}, 
			   BaselinePosition -> {1, 2}]}, 
			Dynamic[
			 CurrentValue[Evaluatable]], ImageSize -> Automatic, BaselinePosition -> Baseline, BaseStyle -> {FontFamily -> "Helvetica"}], #3, 
		   BaseStyle -> {ShowStringCharacters -> False, LineIndent -> 0, PrivateFontOptions -> {"OperatorSubstitution" -> False}}]& ),
		   opts
		]
		,
		{"EntityClass", True},
		TemplateBox[arg, "EntityClassFramelessHot", DisplayFunction -> (
		   PaneSelectorBox[{True -> GridBox[{{
				 PaneBox[
				  DynamicBox[
				   FEPrivate`ImportImage[
					FrontEnd`ToFileName[{"Typeset", "Entity"}, "EntityClass.png"]], ImageSizeCache -> {8., {2., 6.}}], BaselinePosition -> 
				  Bottom], 
				 StyleBox[#, FontColor -> GrayLevel[0]], 
				 StyleBox[
				  RowBox[{"(", #4, ")"}], FontWeight -> "Plain", FontColor -> GrayLevel[0], Selectable -> False]}}, 
			   GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}}, GridBoxSpacings -> {"Columns" -> {{0.2}}, "Rows" -> {{0}}}, 
			   BaselinePosition -> {1, 2}], False -> GridBox[{{
				 PaneBox[
				  DynamicBox[
				   FEPrivate`ImportImage[
					FrontEnd`ToFileName[{"Typeset", "Entity"}, "EntityClass.png"]], ImageSizeCache -> {8., {2., 6.}}], BaselinePosition -> 
				  Bottom], 
				 StyleBox[#, FontColor -> GrayLevel[0]]}}, GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}}, 
			   GridBoxSpacings -> {"Columns" -> {{0.2}}, "Rows" -> {{0}}}, BaselinePosition -> {1, 2}]}, 
			Dynamic[
			 CurrentValue[Evaluatable]], ImageSize -> Automatic, BaselinePosition -> Baseline, BaseStyle -> {FontFamily -> "Helvetica", 
		       ShowStringCharacters -> False, LineIndent -> 0, PrivateFontOptions -> {"OperatorSubstitution" -> False}}]& ),
		   opts
		]
	]
]



linguisticAcceptButton[Dynamic[boxes_]] :=
	Button[
		Mouseover[bitmapResource["LinguisticAcceptButton"], bitmapResource["LinguisticAcceptButtonHot"], BaselinePosition -> Scaled[0.25]],
		MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[
				$LinguisticAssistantBoxReference,
				boxes,
				AutoScroll -> True
			]],
		Appearance -> None,
		BaselinePosition -> Baseline,
		BaseStyle -> {},
		DefaultBaseStyle -> {},
		ContentPadding -> False,
		ImageMargins -> {{0,2},{0,1}},
		FrameMargins -> 0,
		Tooltip -> ToBoxes[cachedFrontEndResource["WAStrings", "AcceptInterpretation"]]
	]


linguisticEditSwitchToExpression[query_, contextinfo_, Dynamic[open_]] :=
	Tooltip[
		Button[
			Pane[
				csceResource["CompactAlphaIcon", contextinfo],
				BaselinePosition -> Baseline
			],
			open = {1,2};
			MathLink`CallFrontEnd[FrontEnd`BoxReferenceFind[
				$LinguisticAssistantBoxReference,
				AutoScroll -> True
			]];
			,
			Appearance -> None,
			BaselinePosition -> Baseline,
			(*BaseStyle -> {},*)
			ContentPadding -> False,
			Method -> "Queued"
		],
		csceResource["CompactAlphaTooltip", contextinfo]
	]


linguisticEditSwitchToQuery[query_, contextinfo_, Dynamic[open_]] :=
	Tooltip[
		Button[
			Pane[
				csceResource["CompactMathematicaIcon", contextinfo],
				BaselinePosition -> Baseline
			],
			doSwitchToQuery[Dynamic[open]],
			Appearance -> None,
			BaselinePosition -> Baseline,
			(*BaseStyle -> {},*)
			ContentPadding -> False,
			Method -> "Queued"
		],
		Row[{csceResource["CompactMathematicaTooltip", contextinfo], "\"", query, "\""}]
	]


doSwitchToQuery[Dynamic[open_]] := (
	MathLink`CallFrontEnd[FrontEnd`BoxReferenceFind[
		$LinguisticAssistantBoxReference,
		AutoScroll -> True
	]];
	open = {1};
	MathLink`CallFrontEnd[FrontEnd`BoxReferenceFind[
		$LinguisticAssistantBoxReference,
		AutoScroll -> True
	]];
	FrontEndExecute[FrontEnd`FrontEndToken["MoveNextPlaceHolder"]];
	FrontEndExecute[FrontEnd`FrontEndToken["MoveNext"]];
)




linguisticOpener[location_, Dynamic[open_]] :=
	Tooltip[
		Button[
			Mouseover[
				Style["\[FilledDiamond]", Smaller, Orange],
				Style["\[FilledDiamond]", Smaller, Red],
				ContentPadding -> False
			],
			open = {1,2},
			Appearance -> None,
			ContentPadding -> False,
			BaseStyle -> {}
		],
		If[location === "Top",
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowMathematicaForm"]],
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowLinguisticForm"]]
		]
	]


linguisticCloser[location_, Dynamic[open_]] := 
	Block[{label, labelhot},
		{label, labelhot} = If[location === "Top",
			Style["\[FilledDownTriangle]", Smaller, #]& /@ {Orange, Red},
			Style["\[FilledUpTriangle]", Smaller, #]& /@ {Orange, Red}
		];
		Item[
			Tooltip[
				Button[
					Mouseover[
						label,
						labelhot,
						ContentPadding -> False
					],
					Switch[location,
						"Top", open = {2},
						"Bottom" | _, open = {1}
					],
					Appearance -> None,
					ContentPadding -> False,
					BaseStyle -> {},
					Alignment -> If[location === "Top", Bottom, Top]
				],
				If[location === "Top",
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "HideLinguisticForm"]],
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "HideMathematicaForm"]]
				]
			],
			Alignment -> If[location === "Top", Bottom, Top]
		]
	]



Unprotect[TagBox, NamespaceBox];

(* Cloud-specific rules *)

(* legacy rules *)
(* If there's still an inline cell in this TagBox, interpret the contents as a control-equal *)
TagBox /: MakeExpression[TagBox[Cell[BoxData[FormBox[theboxes_DynamicModuleBox, _]], ___], "CloudControlEqual", ___], fmt_] :=
	MakeExpression[NamespaceBox["LinguisticAssistant", theboxes], fmt];

TagBox /: MakeExpression[TagBox[Cell[BoxData[theboxes_DynamicModuleBox], ___], "CloudControlEqual", ___], fmt_] :=
	MakeExpression[NamespaceBox["LinguisticAssistant", theboxes], fmt];

(* Otherwise, the user has replaced the inline cell with a pure boxes, so ignore the TagBox *)
TagBox /: MakeExpression[TagBox[Cell[BoxData[FormBox[boxes_, _]], ___], "CloudControlEqual", ___], fmt_] :=
	MakeExpression[boxes, fmt];

TagBox /: MakeExpression[TagBox[Cell[BoxData[ boxes_], ___], "CloudControlEqual", ___], fmt_] :=
	MakeExpression[boxes, fmt];
(* end legacy rules *)


(* If there's still a DynamicModuleBox in this TagBox, interpret the contents as a control-equal *)
TagBox /: MakeExpression[TagBox[theboxes_DynamicModuleBox, "CloudControlEqual", ___], fmt_] :=
	MakeExpression[NamespaceBox["LinguisticAssistant", theboxes], fmt];

(* Otherwise, the user has replaced the inline cell with a pure boxes, so ignore the TagBox *)
TagBox /: MakeExpression[TagBox[boxes_, "CloudControlEqual", ___], fmt_] :=
	MakeExpression[boxes, fmt];

(* End Cloud-specific rules *)

NamespaceBox /: MakeExpression[NamespaceBox["LinguisticAssistant", theboxes_, ___?OptionQ], fmt_] :=
		(* If the boxes variable contains a box expression, use it as the interpretation *)
		Switch[
			theboxes,
			HoldPattern[DynamicModuleBox][{_[Typeset`query$$,query_], _[Typeset`boxes$$,boxes_], ___, _[Typeset`querystate$$, querystate_]}, ___],
			LinguisticAssistantMakeExpression[ theboxes[[1,1,2]], theboxes[[1,2,2]], theboxes[[1,-1,2]], fmt ],
			HoldPattern[DynamicModuleBox][{_[_,query_], _[_,boxes_], ___}, ___],
			LinguisticAssistantMakeExpression[ theboxes[[1,1,2]], theboxes[[1,2,2]], fmt ],
			_,
			HoldComplete[$Failed]
		]


(* with querystate *)

LinguisticAssistantMakeExpression[query_String, boxes:"None", querystate: {___, "Online" -> False, ___}, fmt_] :=
	heldFailure["Offline", query]

LinguisticAssistantMakeExpression[query_String, boxes:"None", querystate: {___, "Allowed" -> False, ___}, fmt_] :=
	heldFailure["NetworkDisabled", query]

LinguisticAssistantMakeExpression[query_String, boxes:"None", querystate: {___, "mparse.jsp" -> _, "$TimedOut" -> _, ___}, fmt_] :=
	heldFailure["TimedOut", query]

LinguisticAssistantMakeExpression[query_String, boxes:"None", querystate: {___, "$TimedOut" -> _, ___}, fmt_] :=
	heldFailure["TimedOut", query]

LinguisticAssistantMakeExpression[query_String, boxes:"None", querystate: {___, "mparse.jsp" -> _, "query.jsp" -> _, ___}, fmt_] :=
	heldFailure["NoParse", query]

LinguisticAssistantMakeExpression[query_, boxes_, querystate_, fmt_] := LinguisticAssistantMakeExpression[query, boxes, fmt]



(* without querystate *)

LinguisticAssistantMakeExpression[query_String, boxes:"None", fmt_] :=  heldFailure["NoParse", query]

LinguisticAssistantMakeExpression[query_, boxes:"None", fmt_] := heldFailure["NoParse", query]

LinguisticAssistantMakeExpression[query_, boxes_, fmt_] := MakeExpression[StripBoxes[boxes], fmt]



assembleFailure[tag_, query_] := Failure[tag, Association[
	"MessageTemplate" -> Replace[cachedMessageTemplate["WAStrings", "MessageTemplate:" <> tag], Except[_String] :> tag],
	"MessageParameters" -> Association[],
	"Query" -> query ]]

heldFailure[tag_, query_] := With[{failure = assembleFailure[tag, query]}, HoldComplete[failure]]

(* InterpretationBox form used by TemplateBox-based control-equal *)
failureBoxes[tag_, query_] := If[TrueQ[$LinguisticAssistantTemplateBox],
	With[{failure = assembleFailure[tag, query]}, InterpretationBox["", failure]],
	"None"
]






(* Fix the ordering of NamespaceBox rules, which occasionally puts the new rules after the generic ones. *)
FormatValues[NamespaceBox] = With[{formatValues = FormatValues[NamespaceBox]}, Join[
	Select[formatValues, !FreeQ[#, "WolframAlphaQueryNoResults" | "WolframAlphaQueryParseResults" | "WolframAlphaQueryResults" | "LinguisticAssistant"]&],
	Select[formatValues,  FreeQ[#, "WolframAlphaQueryNoResults" | "WolframAlphaQueryParseResults" | "WolframAlphaQueryResults" | "LinguisticAssistant"]&]
]]


linguisticTopInputField[contextinfo_, Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] := 
	EventHandler[
		InputField[
			Dynamic[query, (query = #; doNewFastParse[query, contextinfo, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]])&],
			String,
			Appearance -> None,
			BaseStyle -> If[TrueQ[$LinguisticAssistantCompact], {"CalculateInput", FontWeight -> "Plain"}, {"CalculateInput"}],
			BoxID :> "LinguisticAssistantID",
			FieldSize -> {{1,40},{1,Infinity}},
			If[TrueQ[BoxForm`sufficientVersionQ[8.0, 1]], System`TrapSelection -> False, Unevaluated[Sequence[]]]
		],
		{ "ReturnKeyDown" :> 
			(
				If[$LinguisticAssistantCompact && query =!= "", open = {1,2}];
				MathLink`CallFrontEnd[FrontEnd`BoxReferenceFind[
					$LinguisticAssistantBoxReference,
					AutoScroll -> True
				]];
				FrontEndExecute[FrontEnd`FrontEndToken["MoveNext"]];
			)
		}
	]


(* This approach would be preferable, if we could manage the insertion point better. *)
(*
linguisticBottomInputField[Dynamic[boxes_]] :=
	InputField[
		Dynamic[boxes,
			MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[
				$LinguisticAssistantBoxReference,
				#,
				AutoScroll -> True
			]]&
		],
		Boxes,
		Appearance -> None,
		BaseStyle -> {ScriptLevel -> 0, ShowStringCharacters -> True},
		ContinuousAction -> True,
		Enabled -> True,
		FieldSize -> {{1,40},{1,Infinity}}
	]
*)


linguisticBottomInputField[Dynamic[boxes_], Dynamic[open_], editQ_] :=
	Button[
		Mouseover[RawBoxes[boxes], Style[RawBoxes[boxes], Orange, ShowSyntaxStyles -> False], BaseStyle -> {ShowStringCharacters -> True}], 
		If[$CloudControlEqual,
			NotebookWrite[EvaluationCell[], boxes],
			If[TrueQ[editQ],
				doSwitchToQuery[Dynamic[open]]
				,
				MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[
					$LinguisticAssistantBoxReference,
					boxes,
					AutoScroll -> True
				]]
			]
		],
		Appearance -> None,
		BaselinePosition -> Baseline,
		BaseStyle -> {ScriptLevel -> 0, ShowStringCharacters -> True},
		ContentPadding -> True,
		ImageSize -> Automatic,
		Method -> "Queued"
	]

(* This variant is used for generalized entities *)
linguisticBottomInputFieldGeneralizedEntity[Dynamic[boxes_], Dynamic[open_], editQ: True, editTooltip_] :=
	ReplaceAll[RawBoxes[boxes],
		TemplateBox[{base_, props_, d_, tip_, label_}, tag: ("GeneralizedEntityToggleLabeled" | "ImplicitEntityToggleLabeled"), opts___] :>
			Block[{activeLabel},
				activeLabel = ToBoxes @
					Pane[#, ImageMargins -> {{0,3},{0,0}}, BaselinePosition -> Baseline]& @ 
					Tooltip[#, editTooltip]& @
					MouseAppearance[#, "Edit"]& @ 
					linguisticBottomInputField[Dynamic[StyleBox[label, ShowStringCharacters -> False]], Dynamic[open], editQ];
				TemplateBox[{base, props, d, tip, activeLabel}, tag, opts] /; True
			]
	]


(* Allow interface elements within qualified entity properties to be clickable *)
linguisticBottomInputFieldQualifiedEntityProperty[Dynamic[boxes_], Dynamic[open_], editQ: True, editTooltip_] := 
	Replace[boxes, 
		RowBox[{e_, "[", ep: InterpretationBox[_, _EntityProperty, ___], "]"}] :> (
			RowBox[{
				ToBoxes[
					Tooltip[#, editTooltip]& @
					MouseAppearance[#, "Edit"]& @ 
					linguisticBottomInputField[Dynamic[e], Dynamic[open], editQ]
				],
				"[", ep, "]"
			}]
		)
	] // RawBoxes



(* if there's no querystate, add it -- this will only happen with older versions of AlphaIntegration.mx *)
doNewFastParse[query_String, contextinfo_, Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_]] :=
	With[{querystate = Unique["querystate"]},
		querystate = {};
		doNewFastParse[query, contextinfo, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], Dynamic[querystate]]
	]


(* An empty query or a placeholder query should clear everything *)
doNewFastParse[query_String, contextinfo_, Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
	(
		boxes = failureBoxes["NoParse", query];
		allassumptions = {};
		assumptions = {};
		open = {1};
	) /; (query === "" || StringMatchQ[query, "*FrameBox[*Placeholder*]*"])


(*flag managing special display of Quantity in ctrl+=*)
Internal`SetValueNoTrack[QuantityUnits`Private`$WolframAlphaInputFlag,True];


(* Support for context-sensitive parsers in control-equal *)
doNewFastParse[query_, contextinfo: $cscePat, Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
	Block[{rawparse, online, allowed, now},

		online = If[query === "error: offline", False, If[TrueQ[$Notebooks], CurrentValue["InternetConnectionAvailable"], True (* FIXME *)]];
		allowed = If[query === "error: disallowed", False, If[TrueQ[$Notebooks], CurrentValue["AllowDownloads"], PacletManager`$AllowInternet]];
		querystate = {"Online" -> online, "Allowed" -> allowed};
		
		If[online === False || allowed === False || MemberQ[{"error: timedout", "error: no parse"}, query],
			(* if internet is off or disallowed, take a shortcut *)
			rawparse = {};
			If[online === False, Message[WolframAlpha::conopen]];
			If[allowed === False, Message[WolframAlpha::offline]];
			If[query === "error: timedout", querystate = Join[querystate, {"$TimedOut" -> 5}]];
			If[query === "error: no parse", querystate = Join[querystate, {"CSCEParse" -> RandomReal[{1,2}]}]];			
			,
			(* otherwise, do the work *)
			now = AbsoluteTime[];
			TimeConstrained[
				rawparse = CSCEParse[query, contextinfo];
				AppendTo[querystate, "CSCEParse" -> (AbsoluteTime[] - now)],
				$LinguisticAssistantTimeout,
				rawparse = $TimedOut;
				AppendTo[querystate, "$TimedOut" -> (AbsoluteTime[] - now)]
			]
		];
		Switch[rawparse,
			{__},
			boxes = ToBoxes @ rawparse[[1]];
			allassumptions = rawparse,
			_,
			(* Build the appropriate Failure boxes for the interpretation *)
			boxes = failureBoxes[#, query]& @ Which[
				online =!= True, "Offline",
				allowed =!= True, "NetworkDisabled",
				MatchQ[querystate, {___, "$TimedOut" -> _, ___}], "TimedOut",
				True, "NoParse"
			];
			allassumptions = {}
		];
		
		open = {1,2};
		assumptions = {};
		rawparse
	]



(* If $LinguisticAssistantUsesURLSaveAsynchronous, use a background call to query.jsp *)
doNewFastParse[query_, contextinfo_, Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
	Block[{queryurl, querylocalfile, queryobj, rawparse, parse, mparseQ = False, newassumptions = {}, $AlphaQueryMMode = "inline", QuantityUnits`Private`$WolframAlphaInputFlag = True,
			online, allowed, now, rememberState, newquerystate, $MessageList = {}, asyncTaskTimeout, asyncTaskDone = False, asyncTaskStatus = None, rawboxes},
		$echoLevel = 0;
		echoTiming[Row[{"doNewFastParse[", query, ", ...]"}],
		echoInfo["$Version", $Version];
		echoInfo["$InternetProxyRules", Symbol["PacletManager`$InternetProxyRules"]];
		online = If[query === "error: offline", False, If[TrueQ[$Notebooks], CurrentValue["InternetConnectionAvailable"], True (* FIXME *)]];
		allowed = If[query === "error: disallowed", False, If[TrueQ[$Notebooks], CurrentValue["AllowDownloads"], PacletManager`$AllowInternet]];
		newquerystate = {"Online" -> online, "Allowed" -> allowed};

		If[online === False || allowed === False || MemberQ[{"error: mparse timedout", "error: query timedout", "error: no parse", "error: messages"}, query],
			(* if internet is off or disallowed, take a shortcut *)
			parse = None;
			If[online === False, Message[WolframAlpha::conopen]];
			If[allowed === False, Message[WolframAlpha::offline]];
			If[query === "error: mparse timedout", newquerystate = Join[newquerystate, {"$TimedOut" -> 5}]];
			If[query === "error: query timedout", newquerystate = Join[newquerystate, {"mparse.jsp" -> RandomReal[], "$TimedOut" -> 5}]];
			If[query === "error: no parse", newquerystate = Join[newquerystate, {"mparse.jsp" -> RandomReal[], "query.jsp" -> RandomReal[{3,4}]}]];
			If[query === "error: messages", newquerystate = Join[newquerystate, {"mparse.jsp" -> RandomReal[], "query.jsp" -> RandomReal[{3,4}]}]; 1/0; Sin[0, 1]];
			,
			(* otherwise, do the work *)
			now = AbsoluteTime[];
			rememberState[expr_] := AppendTo[newquerystate, expr -> (AbsoluteTime[]-now)];
			TimeConstrained[

				(* fire off a background call to query.jsp *)
				queryurl = WolframAlpha[query, "URL", InputAssumptions -> assumptions, Method -> {"Formats" -> {"minput", "moutput"}}];
				querylocalfile = Close[OpenTemporary[]];
				echoTiming[Tooltip["URLSaveAsynchronous[...]", Defer[URLSaveAsynchronous][queryurl, querylocalfile]],
					queryobj = URLSaveAsynchronous[queryurl, querylocalfile,
						Switch[#2, "data", asyncTaskDone = True, "statuscode", asyncTaskStatus = Replace[#3, {{code_} :> code, _ :> None}]]&,
						BinaryFormat->True, "Progress"->False
					];
				];
				writeToLogWithTimestamp[{"Initiating URLSaveAsynchronous", queryobj -> querylocalfile}];

				(* proceed to call mparse.jsp in the usual way *)
				echoTiming["Parse API call", 
					rawparse = WolframAlpha[query, "RawWolframParse", InputAssumptions -> assumptions];
				];
				rememberState["mparse.jsp"];
				Switch[rawparse,
					{__Rule},
					parse = Replace["Parse" /. rawparse, {Except[_String] -> None}];
					mparseQ = StringQ[parse];
					newassumptions = Replace["Assumptions" /. rawparse, {Except[_List] -> {}}],
					_,
					parse = None;
					newassumptions = {}
				];
				echoInfo["Parse API result", parse];
				(* If mparse.jsp returned no parse, keep the assumptions and dig into query result *)
				If[parse === None,
					writeToLog["Nothing found by mparse.jsp -- waiting for URLSaveAsynchronous (" <> querylocalfile <> ")"];
					(* Wait as long as you can for the async task to return *)
					If[(asyncTaskTimeout = now + $LinguisticAssistantTimeout - AbsoluteTime[]) > 0.1,
						echoTiming["WaitAsynchronousTask",
							Quiet[WaitAsynchronousTask[queryobj, "Timeout" -> (asyncTaskTimeout - 0.1)], WaitAsynchronousTask::asyncobj];
						];
						Which[
							(* if the server returned a status code of 200: *)
							TrueQ[asyncTaskDone] && asyncTaskStatus === 200,
							rawparse = WolframAlpha[query, "PrimaryWolframString", InputAssumptions -> assumptions, Method -> {"SubstituteURL" -> File[querylocalfile]}];
							rememberState["query.jsp"];
							parse = Replace[rawparse, {Except[_String] -> None}];
							echoInfo["WaitAsynchronousTask result", parse];
							,
							(* if the server returned a status code other than 200: *)
							TrueQ[asyncTaskDone],
							Message[WolframAlpha::kbserr, Lookup[URLParse[queryurl], "Domain", queryurl], asyncTaskStatus];
							rememberState["query.jsp"];
							writeToLogWithTimestamp["Query failed with status code " <> ToString[asyncTaskStatus]];
							echoInfo["Query failed with status code", asyncTaskStatus];
							parse = None;
							,
							(* otherwise, the query didn't finish in the allotted time *)
							True,
							writeToLogWithTimestamp["Query timed out"];
							echoInfo["Query timed out", ""];
							rememberState["$TimedOut"];
							parse = None;
							$TimedOut
						]
					]
				],
				$LinguisticAssistantTimeout,
				writeToLogWithTimestamp["Control-equal timed out"];
				echoInfo["Control-equal timed out", ""];
				rememberState["$TimedOut"];
				parse = None;
				$TimedOut
			]
		];

		(* clean up after the asynchronous task, if necessary *)
		Quiet[RemoveAsynchronousTask[queryobj]; DeleteFile[querylocalfile]];
		AppendTo[newquerystate, "Messages" -> $MessageList];
		
		If[parse === None,
			(* Build the appropriate Failure boxes for the interpretation *)
			boxes = failureBoxes[#, query]& @ Which[
				online =!= True, "Offline",
				allowed =!= True, "NetworkDisabled",
				MatchQ[newquerystate, {___, "$TimedOut" -> _, ___}], "TimedOut",
				True, "NoParse"
			],
			rawboxes = parse;
			parse = If[mparseQ, mparseMakeExpression[parse], ToExpression[parse, InputForm, HoldComplete]];
			Replace[parse, HoldComplete[Entity[type_String, __]] :> ($PreviousEntityType = type)];
			echoInfo["Time remaining until LinguisticAssistantTimeout: ", $LinguisticAssistantTimeout - (AbsoluteTime[] - now)];
			boxes = TimeConstrained[
				(* if typesetting takes too long, fall back to the parse string returned from W|A *)
				echoTiming["Typeset the parse", transformControlEqualBoxes[AlphaQueryMakeBoxes @@ parse, query]],
				$LinguisticAssistantTimeout - (AbsoluteTime[] - now),
				--$echoLevel;
				echoInfo["typesetting did not complete before LinguisticAssistantTimeout -- using the parse string", ""];
				rawboxes
			];
		];
		querystate = newquerystate;
		open = If[TrueQ[$LinguisticAssistantCompact], {1,2}, {1,2}];
		allassumptions = DeleteCases[newassumptions, {___, "type" -> (Alternatives @@ $FormulaAssumptionTypes), ___}];
		assumptions = Select[assumptions, !FreeQ[allassumptions, #]&]
	]] /; $LinguisticAssistantUsesURLSaveAsynchronous


(* Otherwise, call mparse.jsp, followed by query.jsp if necessary *)
doNewFastParse[query_, contextinfo_, Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], Dynamic[querystate_]] :=
	Block[{rawparse, parse, mparseQ = False, newassumptions, $AlphaQueryMMode = "inline", QuantityUnits`Private`$WolframAlphaInputFlag = True},
		rawparse = TimeConstrained[WolframAlpha[query, "RawWolframParse", InputAssumptions -> assumptions], $LinguisticAssistantTimeout * 0.6, $TimedOut];
		Switch[rawparse,
			{__Rule},
			parse = Replace["Parse" /. rawparse, {Except[_String] -> None}];
			mparseQ = StringQ[parse];
			newassumptions = Replace["Assumptions" /. rawparse, {Except[_List] -> {}}],
			_,
			parse = None;
			newassumptions = {}
		];
		
		If[parse === None && newassumptions === {},
			rawparse = TimeConstrained[WolframAlpha[query, "PrimaryWolframString", InputAssumptions -> assumptions], $LinguisticAssistantTimeout * 0.4, $TimedOut];
			parse = Replace[rawparse, {Except[_String] -> None}];
			newassumptions = {}
		];
		
		If[parse === None,
			boxes = failureBoxes["NoParse", query],
			If[mparseQ,
				boxes = transformControlEqualBoxes[AlphaQueryMakeBoxes @@ mparseMakeExpression[parse], query],
				boxes = transformControlEqualBoxes[ToExpression[parse, InputForm, AlphaQueryMakeBoxes], query]
			];
		];
		open = If[TrueQ[$LinguisticAssistantCompact], {1,2}, {1,2}];
		allassumptions = DeleteCases[newassumptions, {___, "type" -> (Alternatives @@ $FormulaAssumptionTypes), ___}];
		assumptions = Select[assumptions, !FreeQ[allassumptions, #]&]
	]



(* Utilities for updating control-equal interfaces which have failed for some reason: *)

(*
Detectable failure states:

1. offline
2. online but not allowed to use internet
3. mparse.jsp didn't complete within $LinguisticAssistantTimeout seconds
4. mparse.jsp completes and returns no expr; query.jsp didn't complete within $LinguisticAssistantTimeout seconds
5. mparse.jsp completes and returns no expr; query.jsp completes but returns no expr

Success states:

1. mparse.jsp completes and returns an expr
2. mparse.jsp completes and returns no expr; query.jsp completes and returns an expr
*)


(*
Note that these update functions actually go through and read in every "Input"
cell in the notebook, which is about as suboptimal as you can get. It would be
far better if there were some incantation of something like BoxReferenceFind
which did an optimized search for existing control-equal interfaces. For that,
we might need to add a BoxID to the NamespaceBox.
*)




$badQueryStatePattern =
	NamespaceBox["LinguisticAssistant",
		HoldPattern[DynamicModuleBox][{___, _[Typeset`querystate$$, {___, ("Allowed" -> False) | ("Online" -> False) | ("$TimedOut" -> _), ___}], ___}, ___], ___]


updateLinguisticAssistantBoxes[boxes_] := (Message[WolframAlpha::conopen]; boxes) /; CurrentValue["InternetConnectionAvailable"] === False

updateLinguisticAssistantBoxes[boxes_] := (Message[WolframAlpha::offline]; boxes) /; CurrentValue["AllowDownloads"] === False

updateLinguisticAssistantBoxes[
		NamespaceBox["LinguisticAssistant", DynamicModuleBox[
			{___, _[Typeset`query$$, query_], ___, _[Typeset`assumptions$$, assumptions_], ___},
			___], ___]] :=
	ToBoxes[WolframAlpha[query, "LinguisticAssistant", InputAssumptions -> assumptions]]

updateLinguisticAssistantBoxes[boxes_] := boxes;



UpdateCellLinguisticAssistants[cellobj_CellObject] :=
Module[{cellexpr, newcellexpr},
	cellexpr = NotebookRead[cellobj];
	newcellexpr = cellexpr /. ce: $badQueryStatePattern :> updateLinguisticAssistantBoxes[ce];
	If[newcellexpr =!= cellexpr, NotebookWrite[cellobj, newcellexpr]; "updated", "stet"]
]


UpdateNotebookLinguisticAssistants[] := UpdateNotebookLinguisticAssistants[InputNotebook[]]

UpdateNotebookLinguisticAssistants[nbobj_NotebookObject] :=
Module[{list},
	list = UpdateCellLinguisticAssistants /@ Cells[nbobj, CellStyle -> "Input"];
	{Length[list], Count[list, "updated"], Count[list, "stet"]}
]


LinguisticAssistantsNeedUpdatingQ[] := LinguisticAssistantsNeedUpdatingQ[InputNotebook[]]

LinguisticAssistantsNeedUpdatingQ[nbobj_NotebookObject] :=
	Not @ FreeQ[NotebookRead[Cells[nbobj, CellStyle -> "Input"]], $badQueryStatePattern]



UpdateLinguisticAssistantsDialog[nbobj_NotebookObject] :=
CreateDialog[
	DynamicModule[{Typeset`mode="Init", Typeset`cells=0, Typeset`updated=0, Typeset`stet=0},
		Column[{
			PaneSelector[{
				"Init" -> TextCell @ "Retry all failed inline free-form input parses in this notebook?",
				"Running" -> Pane[Animator[Appearance -> "Necklace"], 340, Alignment -> Center],
				"Done" -> Dynamic[Switch[Typeset`updated, 0, "No cells updated.", 1, "One cell updated.", _, Row[{Typeset`updated, " cells updated."}]]]},
				Dynamic[First @ {Typeset`mode}]
			],
			Item[PaneSelector[{
				"Init" -> ChoiceButtons[
					{"OK", "Cancel"},
					{(Typeset`mode = "Running"; {Typeset`cells, Typeset`updated, Typeset`stet} = UpdateNotebookLinguisticAssistants[nbobj]; Typeset`mode = "Done"), DialogReturn[]},
					{Method -> "Queued", Method -> "Preemptive"}
				],
				"Running" -> "",
				"Done" -> ChoiceButtons[]},
				Dynamic[First @ {Typeset`mode}],
				Alignment -> Right
			], Alignment -> Right]
			},
			ItemSize -> Scaled[0.99]
		],
		BaseStyle -> {"ControlStyle"}
	],
	WindowSize -> {350, FitAll},
	NotebookEventActions->{},
	WindowTitle -> "Retry All"
]




(* ::Subsection::Closed:: *)
(*WolframAlphaDate*)


Options[WolframAlphaDate] = {
	TimeZone -> Automatic };
	
(* Code block for translation of WolframAlphaDate[] into DateObject[] courtesy of Nick Lariviere. *)

WolframAlphaDate[args__] := With[{res=iWolframAlphaDate[args]},
	res /; FreeQ[res,$Failed]
]


iWolframAlphaDate[date_List, opts: OptionsPattern[]] /; 1 <= Length[date] <=6 := Module[{tz,dObj},
	tz = OptionValue[WolframAlphaDate, {opts}, TimeZone]/.Automatic:>$TimeZone;
	dObj = DateObject[date,TimeZone->tz];
	If[DateObjectQ[dObj],
		dObj,
		$Failed
	]
]

iWolframAlphaDate[___] := $Failed

WolframAlphaDate /: MakeBoxes[WolframAlphaDate[date_List, opts___], fmt_] /; (1 <= Length[Unevaluated[date]] <= 6) :=
With[{boxes = ToBoxes[Framed[
		Switch[Length[Unevaluated[date]],
			1, DateString[date, "Year"],
			2, DateString[date, {"MonthNameShort", " ", "Year"}],
			3, DateString[date, "DateShort"],
			4, DateString[date, {"DateShort", " ", "Hour12Short", " ", "AMPMLowerCase"}],
			5, DateString[date, {"DateShort", " ", "Hour12Short", ":", "Minute", " ", "AMPMLowerCase"}],
			6, DateString[date, {"DateShort", " ", "Hour12Short", ":", "Minute", ":", "Second", " ", "AMPMLowerCase"}] ],
		RoundingRadius -> 4,
		FrameStyle -> RGBColor[1, 0.9, 0.8],
		FrameMargins -> 3], fmt]},
	InterpretationBox[boxes, WolframAlphaDate[date, opts], BaseStyle -> {FontFamily -> "Helvetica", ShowStringCharacters -> False}] ]	



(* ::Subsection::Closed:: *)
(*WolframAlphaQuantity*)


Options[WolframAlphaQuantity] = {
	"Accuracy" -> Automatic,
	Appearance -> Automatic,
	"Precision" -> Automatic };



(* Code block for translation of WolframAlphaQuantity[] into Quantity[] courtesy of Itai Seggev. *)

WolframAlphaQuantity[args__] := With[{res=iWolframAlphaQuantity[args]},
	res /; FreeQ[res,$Failed]
]

(* convert a description to Quantity[1, unitstring] into which we will insert the quantity part. *)
descriptionToUnitalQuantity[s_] := Replace[
	(* Numbers on their own do not parse to quantities.  Since WAQ uses "" for PureUnities, we need a special case for that *)
	If[s === "",
		Quantity[1, "PureUnities"],
		Quiet @ ReleaseHold @ WolframAlpha["1 " <> s,"ToQuantity",InputAssumptions->{"ClashPrefs_Unit"}]
	],
	(* If the parse failed, just wrap the description in InertUnit*)
	Except[_Quantity] :> Quantity[1,System`IndependentUnit @ s]
]

iWolframAlphaQuantity[{number__}, {description__String}, opts: OptionsPattern[]] /; Length @ {number}  == Length @ {description} := Module[{quantities},
	quantities = Map[descriptionToUnitalQuantity, {description}];
	(* Don't worry about precision for mixed radix.  Just put the units in the second argument and correct the head in the first argument. *)
	Quantity[System`MixedRadix[number], Last /@ quantities]
]

iWolframAlphaQuantity[number_, description_String, opts: OptionsPattern[]] := Module[{acc, prec, quant, accNumber, precNumber},
	{acc, prec} = OptionValue[WolframAlphaQuantity, {opts}, {"Accuracy", "Precision"}];
	quant = descriptionToUnitalQuantity[description];
	(* Try to adjust the accuracy/precision of the quantity part to what Alpha would display*)
	Switch[{acc, prec},
		{Automatic, Automatic},
		accNumber = precNumber = number,
		
		{_?NumericQ, Automatic},
		accNumber = SetAccuracy[number, acc];
		If[Precision[accNumber] < 1, accNumber = SetPrecision[number, 1]];
		precNumber = accNumber,
		
		{Automatic, _?NumericQ},
		accNumber = precNumber = SetPrecision[number, prec],
		
		{_?NumericQ,_?NumericQ},
		accNumber = SetAccuracy[number, acc];
		precNumber = SetPrecision[number, prec],
		
		_,
		accNumber = precNumber = $Failed		
	];
	(* Stick the new number in the quantity*)
	If[Precision[accNumber] < Precision[precNumber],
		ReplacePart[quant, 1 -> accNumber],
		ReplacePart[quant, 1 -> precNumber]
	]
]

iWolframAlphaQuantity[__] = $Failed;



WolframAlphaQuantity /: MakeBoxes[WolframAlphaQuantity[amts_List, units_List, opts___], fmt_] /; (Length[amts] === Length[units]) :=
With[{
	boxes = Map[Function[x, MakeBoxes[x, fmt], HoldAllComplete], amts],
	rowbox = RowBox[Flatten[Riffle[Map[{Slot @@ {#}, "  ", StyleBox[ToBoxes[units[[#]], fmt],
		FontColor -> GrayLevel[0.5], FontWeight -> "Plain", ShowStringCharacters -> False]}&, Range[Length[units]]], "  "]]],
	rest = Sequence @@ Flatten[Function[x, {",", MakeBoxes[x, fmt]}, HoldAllComplete] /@ Unevaluated[{units, opts}]]},

	TemplateBox[boxes, "WolframAlphaQuantities",
		DisplayFunction -> (FrameBox[rowbox, RoundingRadius -> 4, FrameStyle -> RGBColor[1, 0.9, 0.8], FrameMargins -> 3]&),
		InterpretationFunction -> (RowBox[{"WolframAlphaQuantity", "[",
			RowBox[{ RowBox[{"{", RowBox[{TemplateSlotSequence[1, ","]}], "}"}], rest }], "]"}]&),
		BaseStyle -> {FontFamily -> "Helvetica"}] ]


WolframAlphaQuantity /: MakeBoxes[WolframAlphaQuantity[amt_, unit_, opts___], fmt_] :=
With[{
	boxes = {MakeBoxes[amt, fmt]},
	rowbox = RowBox[{Slot[1], "  ", StyleBox[ToBoxes[unit, fmt], FontColor -> GrayLevel[0.5], FontWeight -> "Plain", ShowStringCharacters -> False]}],
	rest = Sequence @@ Flatten[Function[x, {",", MakeBoxes[x, fmt]}, HoldAllComplete] /@ Unevaluated[{unit, opts}]]},

	TemplateBox[boxes, "WolframAlphaQuantity",
		DisplayFunction -> (FrameBox[rowbox, RoundingRadius -> 4, FrameStyle -> RGBColor[1, 0.9, 0.8], FrameMargins -> 3]&),
		InterpretationFunction -> (RowBox[{"WolframAlphaQuantity", "[", RowBox[{#, rest}], "]"}]&),
		BaseStyle -> {FontFamily -> "Helvetica"}] ]


(* Utility for changing precision *)
changePrecision[n_, prec_] := N[FromDigits[RealDigits[n]], prec]


(* ::Subsection::Closed:: *)
(*Controlled Autoloading*)


$echoLevel = 1;
echoTiming["trigger autoloading of other utilities",
	(* Force the url encoding infrastructure to load *)
	echoTiming["warm up urlencode", urlencode[""]];

	(* Force the relevant Import infrastructure to load *)
	echoTiming["warm up URLFetch", URLFetch];
	Module[{readtimeout = Options[URLFetch, "ReadTimeout"]},
		Internal`WithLocalSettings[
			SetOptions[URLFetch, "ReadTimeout" -> 0.1], (* 291081 *)
			Quiet[
				(* URLFetch doesn't need to warm up Import *)
				If[TrueQ[$AlphaQueryUseURLFetch], Null, echoTiming["warm up URL import", Import[$AlphaQueryBaseURLProtocol, "Text"]]];
				echoTiming["warm up XML import", ImportString["", "XML"]];
				echoTiming["warm up Text import", ImportString["", "Text"]]
			],
			SetOptions[URLFetch, readtimeout]
		]
	];
];
$echoLevel = 0;



(* ::Section::Closed:: *)
(*Package footer*)


End[] (* WolframAlphaClient`Private` *)

System`Private`RestoreContextPath[];
