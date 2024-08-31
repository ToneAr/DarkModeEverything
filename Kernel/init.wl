(*
	Check for non-empty .backup folder
*)
With[{
		backupDir = FileNameJoin[{PacletObject[]["Location"], ".backup"}]
	},
	If[FileExistsQ[FileNameJoin[{backupDir, "info.wl"}]],
		(*
			Define all system files effected by DME
		*)
		Null
		(*
			Backup all system files
		*)
	]
]