(*===-- llvm_analysis.mli - LLVM OCaml Interface --------------*- OCaml -*-===*
 *
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 *===----------------------------------------------------------------------===*)

type lldibuilder

type llmetadata = Llvm.llmetadata

type lllocation = private llmetadata
type llscope = private llmetadata
type llsubprogram = private llscope
type llfile = private llmetadata
type llvariable = private llmetadata

(** Source languages known by DWARF. *)
module DWARFSourceLanguageKind : sig
  type t =
    | C89
    | C
    | Ada83
    | C_plus_plus
    | Cobol74
    | Cobol85
    | Fortran77
    | Fortran90
    | Pascal83
    | Modula2
    (*  New in DWARF v3: *)
    | LLVMJava
    | C99
    | Ada95
    | Fortran95
    | PLI
    | ObjC
    | ObjC_plus_plus
    | UPC
    | D
    (*  New in DWARF v4: *)
    | LLVMPython
    (*  New in DWARF v5: *)
    | LLVMOpenCL
    | Go
    | Modula3
    | Haskell
    | C_plus_plus_03
    | C_plus_plus_11
    | OCaml
    | Rust
    | C11
    | Swift
    | Julia
    | Dylan
    | C_plus_plus_14
    | Fortran03
    | Fortran08
    | RenderScript
    | BLISS
    (*  Vendor extensions: *)
    | LLVMMips_Assembler
    | GOOGLE_RenderScript
    | BORLAND_Delphi
end

module DIFlag : sig
  type t =
    | Zero
    | Private
    | Protected
    | Public
    | FwdDecl
    | AppleBlock
    | ReservedBit4
    | Virtual
    | Artificial
    | Explicit
    | Prototyped
    | ObjcClassComplete
    | ObjectPointer
    | Vector
    | StaticMember
    | LValueReference
    | RValueReference
    | Reserved
    | SingleInheritance
    | MultipleInheritance
    | VirtualInheritance
    | IntroducedVirtual
    | BitField
    | NoReturn
    | TypePassByValue
    | TypePassByReference
    | EnumClass
    | FixedEnum
    | Thunk
    | NonTrivial
    | BigEndian
    | LittleEndian
    | IndirectVirtualBase
    | Accessibility
    | PtrToMemberRep
end

(** An opaque type to represent OR of multiple DIFlag.t. *)
type lldiflags

val diflags_get : DIFlag.t -> lldiflags

val diflags_set : lldiflags -> DIFlag.t -> lldiflags

val diflags_test : lldiflags -> DIFlag.t -> bool

(** The kind of metadata nodes. *)
module MetadataKind : sig
  type t =
    | MDStringMetadataKind
    | ConstantAsMetadataMetadataKind
    | LocalAsMetadataMetadataKind
    | DistinctMDOperandPlaceholderMetadataKind
    | MDTupleMetadataKind
    | DILocationMetadataKind
    | DIExpressionMetadataKind
    | DIGlobalVariableExpressionMetadataKind
    | GenericDINodeMetadataKind
    | DISubrangeMetadataKind
    | DIEnumeratorMetadataKind
    | DIBasicTypeMetadataKind
    | DIDerivedTypeMetadataKind
    | DICompositeTypeMetadataKind
    | DISubroutineTypeMetadataKind
    | DIFileMetadataKind
    | DICompileUnitMetadataKind
    | DISubprogramMetadataKind
    | DILexicalBlockMetadataKind
    | DILexicalBlockFileMetadataKind
    | DINamespaceMetadataKind
    | DIModuleMetadataKind
    | DITemplateTypeParameterMetadataKind
    | DITemplateValueParameterMetadataKind
    | DIGlobalVariableMetadataKind
    | DILocalVariableMetadataKind
    | DILabelMetadataKind
    | DIObjCPropertyMetadataKind
    | DIImportedEntityMetadataKind
    | DIMacroMetadataKind
    | DIMacroFileMetadataKind
    | DICommonBlockMetadataKind
end

(** The amount of debug information to emit. *)
module DWARFEmissionKind : sig
  type t = None | Full | LineTablesOnly
end

val debug_metadata_version : unit -> int

val get_module_debug_metadata_version : Llvm.llmodule -> int

val dibuilder : Llvm.llmodule -> lldibuilder

val dibuild_finalize : lldibuilder -> unit

val dibuild_create_compile_unit :
  lldibuilder ->
  DWARFSourceLanguageKind.t ->
  file_ref:llmetadata ->
  producer:string ->
  is_optimized:bool ->
  flags:string ->
  runtime_ver:int ->
  split_name:string ->
  DWARFEmissionKind.t ->
  dwoid:int ->
  di_inlining:bool ->
  di_profiling:bool ->
  sys_root:string ->
  sdk:string ->
  llmetadata

val dibuild_create_file :
  lldibuilder -> filename:string -> directory:string -> llmetadata

val dibuild_create_module :
  lldibuilder ->
  parent_ref:llmetadata ->
  name:string ->
  config_macros:string ->
  include_path:string ->
  sys_root:string ->
  llmetadata

val dibuild_create_namespace :
  lldibuilder ->
  parent_ref:llmetadata ->
  name:string ->
  bool:string ->
  llmetadata

val dibuild_create_function :
  lldibuilder ->
  scope:llmetadata ->
  name:string ->
  linkage_name:string ->
  file:llmetadata ->
  line_no:int ->
  ty:llmetadata ->
  is_local_to_unit:bool ->
  is_definition:bool ->
  scope_line:int ->
  flags:lldiflags ->
  is_optimized:bool ->
  llmetadata

val dibuild_create_lexical_block :
  lldibuilder ->
  scope:llmetadata ->
  file:llmetadata ->
  line:int ->
  column:int ->
  llmetadata

val llmetadata_null : unit -> llmetadata

val dibuild_create_debug_location :
  ?inlined_at:llmetadata ->
  Llvm.llcontext ->
  line:int ->
  column:int ->
  scope:llmetadata ->
  lllocation

val di_location_get_line : lllocation -> int

val di_location_get_column : lllocation -> int

val di_location_get_scope : lllocation -> llscope

val di_location_get_inlined_at : lllocation -> llscope

val di_scope_get_file : llscope -> llfile option

val di_file_get_directory : llfile -> string

val di_file_get_filename : llfile -> string

val di_file_get_source : llfile -> string

val dibuild_get_or_create_type_array :
  lldibuilder -> data:llmetadata array -> llmetadata

val dibuild_create_subroutine_type :
  lldibuilder ->
  file:llmetadata ->
  param_types:llmetadata array ->
  lldiflags ->
  llmetadata

val dibuild_create_enumerator :
  lldibuilder -> name:string -> value:int -> is_unsigned:bool -> llmetadata

val dibuild_create_enumeration_type :
  lldibuilder ->
  scope:llmetadata ->
  name:string ->
  file:llmetadata ->
  line_number:int ->
  size_in_bits:int ->
  align_in_bits:int ->
  elements:llmetadata array ->
  class_ty:llmetadata ->
  llmetadata

val dibuild_create_union_type :
  lldibuilder ->
  scope:llmetadata ->
  name:string ->
  file:llmetadata ->
  line_number:int ->
  size_in_bits:int ->
  align_in_bits:int ->
  lldiflags ->
  elements:llmetadata array ->
  run_time_language:int ->
  unique_id:string ->
  llmetadata

val dibuild_create_array_type :
  lldibuilder ->
  size:int ->
  align_in_bits:int ->
  ty:llmetadata ->
  subscripts:llmetadata array ->
  llmetadata

val dibuild_create_vector_type :
  lldibuilder ->
  size:int ->
  align_in_bits:int ->
  ty:llmetadata ->
  subscripts:llmetadata array ->
  llmetadata

val dibuild_create_unspecified_type : lldibuilder -> name:string -> llmetadata

val dibuild_create_basic_type :
  lldibuilder ->
  name:string ->
  size_in_bits:int ->
  encoding:int ->
  lldiflags ->
  llmetadata

val dibuild_create_pointer_type :
  lldibuilder ->
  pointee_ty:llmetadata ->
  size_in_bits:int ->
  align_in_bits:int ->
  address_space:int ->
  name:string ->
  llmetadata

val dibuild_create_struct_type :
  lldibuilder ->
  scope:llmetadata ->
  name:string ->
  file:llmetadata ->
  line_number:int ->
  size_in_bits:int ->
  align_in_bits:int ->
  lldiflags ->
  derived_from:llmetadata ->
  elements:llmetadata array ->
  run_time_lang:int ->
  vtable_holder:llmetadata ->
  unique_id:string ->
  llmetadata

val dibuild_create_member_type :
  lldibuilder ->
  scope:llmetadata ->
  name:string ->
  file:llmetadata ->
  line_number:int ->
  size_in_bits:int ->
  align_in_bits:int ->
  offset_in_bits:int ->
  lldiflags ->
  ty:llmetadata ->
  llmetadata

val dibuild_create_static_member_type :
  lldibuilder ->
  scope:llmetadata ->
  name:string ->
  file:llmetadata ->
  line_number:int ->
  ty:llmetadata ->
  lldiflags ->
  const_val:Llvm.llvalue ->
  align_in_bits:int ->
  llmetadata

val dibuild_create_member_pointer_type :
  lldibuilder ->
  pointee_type:llmetadata ->
  class_type:llmetadata ->
  size_in_bits:int ->
  align_in_bits:int ->
  lldiflags ->
  llmetadata

val dibuild_create_object_pointer_type : lldibuilder -> llmetadata -> llmetadata

val dibuild_create_qualified_type :
  lldibuilder -> tag:int -> llmetadata -> llmetadata

val dibuild_create_reference_type :
  lldibuilder -> tag:int -> llmetadata -> llmetadata

val dibuild_create_null_ptr_type : lldibuilder -> llmetadata

val dibuild_create_typedef :
  lldibuilder ->
  ty:llmetadata ->
  name:string ->
  file:llmetadata ->
  line_no:int ->
  scope:llmetadata ->
  align_in_bits:int ->
  llmetadata

val dibuild_create_inheritance_native :
  lldibuilder ->
  ty:llmetadata ->
  base_ty:llmetadata ->
  base_offset:int ->
  vb_ptr_offset:int ->
  lldiflags ->
  llmetadata

val dibuild_create_forward_decl :
  lldibuilder ->
  tag:int ->
  name:string ->
  scope:llmetadata ->
  file:llmetadata ->
  line:int ->
  runtime_lang:int ->
  size_in_bits:int ->
  align_in_bits:int ->
  unique_identifier:string ->
  llmetadata

val dibuild_create_replaceable_composite_type :
  lldibuilder ->
  tag:int ->
  name:string ->
  scope:llmetadata ->
  file:llmetadata ->
  line:int ->
  runtime_lang:int ->
  size_in_bits:int ->
  align_in_bits:int ->
  lldiflags ->
  unique_identifier:string ->
  llmetadata

val dibuild_create_bit_field_member_type :
  lldibuilder ->
  scope:llmetadata ->
  name:string ->
  file:llmetadata ->
  line_num:int ->
  size_in_bits:int ->
  offset_in_bits:int ->
  storage_offset_in_bits:int ->
  lldiflags ->
  ty:llmetadata ->
  llmetadata

val dibuild_create_class_type :
  lldibuilder ->
  scope:llmetadata ->
  name:string ->
  file:llmetadata ->
  line_number:int ->
  size_in_bits:int ->
  align_in_bits:int ->
  offset_in_bits:int ->
  lldiflags ->
  derived_from:llmetadata ->
  elements:llmetadata array ->
  vtable_holder:llmetadata ->
  template_params_node:llmetadata ->
  unique_identifier:string ->
  llmetadata

val dibuild_create_artificial_type : lldibuilder -> ty:llmetadata -> llmetadata

val ditype_get_name : llmetadata -> string

val ditype_get_size_in_bits : llmetadata -> int

val ditype_get_offset_in_bits : llmetadata -> int

val ditype_get_align_in_bits : llmetadata -> int

val ditype_get_line : llmetadata -> int

val ditype_get_flags : llmetadata -> lldiflags

val get_subprogram : Llvm.llvalue -> llsubprogram option

val set_subprogram : Llvm.llvalue -> llsubprogram -> unit

val di_subprogram_get_line : llsubprogram -> int

val instruction_get_debug_loc : Llvm.llvalue -> lllocation option

val instruction_set_debug_loc : Llvm.llvalue -> lllocation -> unit

(** [di_global_variable_expression_get_variable gve] returns the debug variable
    of [gve], which must be a [DIGlobalVariableExpression].
    See the [llvm::DIGlobalVariableExpression::getVariable()] method. *)
val di_global_variable_expression_get_variable : llmetadata -> llvariable option

(** [di_variable_get_line v] returns the line number of the variable [v].
    See the [llvm::DIVariable::getLine()] method. *)
val di_variable_get_line : llvariable -> int

(** [di_variable_get_file v] returns the file of the variable [v].
    See the [llvm::DIVariable::getFile()] method. *)
val di_variable_get_file : llvariable -> llfile option

val get_metadata_kind : llmetadata -> MetadataKind.t
