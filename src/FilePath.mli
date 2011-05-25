(********************************************************************************)
(*  ocaml-fileutils: files and filenames common operations                      *)
(*                                                                              *)
(*  Copyright (C) 2003-2011, Sylvain Le Gall                                    *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** Operations on abstract filenames. 
    
    This module allow to manipulate string or abstract representation of a
    filename.

    Abstract representation of a filename allow to decode it only once, and
    should speed up further operation on it (comparison in particular). If you
    intend to do a lot of processing on filename, you should consider using its
    abstract representation.

    This module manipulate abstract path that are not bound to a real filesystem.
    In particular, it makes the assumption that there is no symbolic link that
    should modify the meaning of a path. If you intend to use this module
    against a real set of filename, the best solution is to apply to every
    filename to solve symbolic link through {!FileUtil.readlink}.

    @author Sylvain Le Gall
  *)

(** Filename type. *)
type filename = string;;

(** Extension type. *)
type extension = string;;

(** {2 Exceptions and types} *)

(** Cannot pass a base filename which is relative. *)
exception BaseFilenameRelative of filename;;

(** We do not have recognized any OS, please contact upstream. *)
exception UnrecognizedOS of string;;

(** The filename use was empty. *)
exception EmptyFilename;;

(** The last component of the filename does not support extension (Root,
     ParentDir...)
  *)
exception NoExtension of filename;;

(** The filename used is invalid. *)
exception InvalidFilename of filename;;  

(** {2 Ordering} *)

(** [is_subdir fl1 fl2] Is [fl2] a sub directory of [fl1] *)
val is_subdir: filename -> filename -> bool

(** [is_updir fl1 fl2] Is [fl1] a sub directory of [fl2] *)
val is_updir: filename -> filename -> bool

(** [compare fl1 fl2] Give an order between the two filename. The
    classification is done by sub directory relation, [fl1] < [fl2] iff [fl1] is 
    a subdirectory of [fl2], and lexicographical order of each part of the
    reduce filename when [fl1] and [fl2] has no hierarchical relation 
  *)
val compare: filename -> filename -> int

(** {2 Standard operations } *) 

(** Current dir. *)
val current_dir: filename

(** Upper dir. *)
val parent_dir: filename

(** Make a filename from a set of strings. *)
val make_filename: string list -> filename 

(** Extract only the file name of a filename. *)
val basename: filename -> filename

(** Extract the directory name of a filename. *)
val dirname: filename -> filename

(** Append a filename to a filename. *)
val concat: filename -> filename -> filename

(** Return the shortest filename which is equal to the filename given. It remove the 
    "." in Unix filename, for example.
    If [no_symlink] flag is set, consider that the path doesn't contain symlink
    and in this case ".." for Unix filename are also reduced.
  *)
val reduce: ?no_symlink:bool -> filename -> filename

(** Create an absolute filename from a filename relative and an absolute base
    filename. 
  *)
val make_absolute: filename -> filename -> filename

(** Create a filename which is relative to the base filename. *)
val make_relative: filename -> filename -> filename

(** [reparent fln_src fln_dst fln] Return the same filename as [fln]
    but the root is no more [fln_src] but [fln_dst]. It replaces the 
    [fln_src] prefix by [fln_dst].
  *)
val reparent: filename -> filename -> filename -> filename 

(** Identity for testing the stability of implode/explode. *)
val identity: filename -> filename

(** Test if the filename is a valid one. *)
val is_valid: filename -> bool

(** Check if the filename is relative to a dir or not. 
  *)
val is_relative: filename -> bool

(** Check if the filename is the current directory. 
  *)
val is_current: filename -> bool

(** Check if the filename is the parent directory. 
  *)
val is_parent: filename -> bool

(** {2 Extension}*)

(** Extension is define as the suffix of a filename, just after the last ".". 
  *)

(** Remove extension and the trailing ".". *)
val chop_extension: filename -> filename

(** Extract the extension. *)
val get_extension: filename -> extension 

(** Check the extension. *)
val check_extension: filename -> extension -> bool

(** Add an extension with a "." before. *)
val add_extension: filename -> extension -> filename

(** Replace extension. *)
val replace_extension: filename -> extension -> filename

(** {2 PATH-like operation}*)

(** PATH-like refers the environment variable PATH. This variable holds a list
    of filename. The functions [string_of_path] and [path_of_string] allow to
    convert this kind of list by using the good separator between filename.
  *)

(** Create a PATH-like string. *)
val string_of_path: filename list -> string

(** Extract filenames from a PATH-like string. *)
val path_of_string: string -> filename list

(** {2 Filename specifications} *)

(** Definition of operations for path manipulation. *)

(** Generic operations. *)
module type PATH_SPECIFICATION =
sig
  type filename  
  type extension 

  (** {3 Converting abstract type from/to string } *)

  (** Create a filename from a string. *)
  val string_of_filename : filename -> string

  (** Create a string from a filename. *)
  val filename_of_string : string -> filename

  (** Create an extension from a string. *)
  val extension_of_string: string -> extension

  (** Return string representation of an extension. *)
  val string_of_extension: extension -> string

  (** {3 Standard operations} *)

  (** See {!FilePath.make_filename} *)
  val make_filename: string list -> filename 

  (** See {!FilePath.is_subdir} *)
  val is_subdir: filename -> filename -> bool

  (** See {!FilePath.is_updir} *)
  val is_updir: filename -> filename -> bool

  (** See {!FilePath.compare} *)
  val compare: filename -> filename -> int

  (** See {!FilePath.basename} *)
  val basename: filename -> filename

  (** See {!FilePath.dirname} *)
  val dirname: filename -> filename

  (** See {!FilePath.concat} *)
  val concat: filename -> filename -> filename

  (** See {!FilePath.reduce} *)
  val reduce: ?no_symlink:bool -> filename -> filename

  (** See {!FilePath.make_absolute} *)
  val make_absolute: filename -> filename -> filename

  (** See {!FilePath.make_relative} *)
  val make_relative: filename -> filename -> filename

  (** See {!FilePath.reparent} *)
  val reparent: filename -> filename -> filename -> filename 

  (** See {!FilePath.identity} *)
  val identity: filename -> filename

  (** See {!FilePath.is_valid} *)
  val is_valid: filename -> bool

  (** See {!FilePath.is_relative} *)
  val is_relative: filename -> bool

  (** See {!FilePath.is_current} *)
  val is_current: filename -> bool

  (** See {!FilePath.is_parent} *)
  val is_parent: filename -> bool

  (** See {!FilePath.chop_extension} *)
  val chop_extension: filename -> filename

  (** See {!FilePath.get_extension} *)
  val get_extension: filename -> extension 

  (** See {!FilePath.check_extension} *)
  val check_extension: filename -> extension -> bool

  (** See {!FilePath.add_extension} *)
  val add_extension: filename -> extension -> filename

  (** See {!FilePath.replace_extension} *)
  val replace_extension: filename -> extension -> filename

  (** See {!FilePath.string_of_path} *)
  val string_of_path: filename list -> string

  (** See {!FilePath.path_of_string} *)
  val path_of_string: string -> filename list

  (** See {!FilePath.current_dir} *)
  val current_dir: filename

  (** See {!FilePath.parent_dir} *)
  val parent_dir: filename
end
;;

(** Generic operations, with type filename and extension as strings. *)
module type PATH_STRING_SPECIFICATION =
sig
  module Abstract: PATH_SPECIFICATION 

  include PATH_SPECIFICATION with 
    type filename = string and 
    type extension = string
end
;;

(** Operations on filenames for other OS. The {!DefaultPath} always match the 
    current OS.
  *)

(** Default operating system. *)
module DefaultPath: PATH_STRING_SPECIFICATION;;

(** Unix operating system. *)
module UnixPath: PATH_STRING_SPECIFICATION;;

(** MacOS operating system. *)
module MacOSPath: PATH_STRING_SPECIFICATION;;

(** Win32 operating system. *)
module Win32Path: PATH_STRING_SPECIFICATION;;

(** Cygwin operating system. *)
module CygwinPath: PATH_STRING_SPECIFICATION;;
