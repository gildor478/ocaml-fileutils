
type filename_part =
	Root of string
	| ParentDir 
	| CurrentDir
	| Component of string

type filename = string

type extension = string
