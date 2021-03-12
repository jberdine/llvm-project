(*===-- llvm_debuginfo.ml - LLVM OCaml Interface ---------------*- OCaml -*-===*
 *
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 *===----------------------------------------------------------------------===*)

type lldibuilder

type llmetadata = Llvm.llmetadata

type lllocation = llmetadata
type llscope = llmetadata
type llsubprogram = llscope
type llfile = llmetadata
type llvariable = llmetadata

(** Source languages known by DWARF. *)
module DWARFSourceLanguageKind = struct
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

module DIFlag = struct
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

type lldiflags

external diflags_get : DIFlag.t -> lldiflags = "llvm_diflags_get"

external diflags_set : lldiflags -> DIFlag.t -> lldiflags = "llvm_diflags_set"

external diflags_test : lldiflags -> DIFlag.t -> bool = "llvm_diflags_test"

(** The kind of metadata nodes. *)
module MetadataKind = struct
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
module DWARFEmissionKind = struct
  type t = None | Full | LineTablesOnly
end

external debug_metadata_version : unit -> int = "llvm_debug_metadata_version"

external get_module_debug_metadata_version : Llvm.llmodule -> int
  = "llvm_get_module_debug_metadata_version"

external dibuilder : Llvm.llmodule -> lldibuilder = "llvm_dibuilder"

external dibuild_finalize : lldibuilder -> unit = "llvm_dibuild_finalize"

(* See LLVMDIBuilderCreateCompileUnit for argument details. *)
external dibuild_create_compile_unit :
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
  = "llvm_dibuild_create_compile_unit_bytecode" "llvm_dibuild_create_compile_unit_native"

external dibuild_create_file :
  lldibuilder -> filename:string -> directory:string -> llmetadata
  = "llvm_dibuild_create_file"

external dibuild_create_module :
  lldibuilder ->
  parent_ref:llmetadata ->
  name:string ->
  config_macros:string ->
  include_path:string ->
  sys_root:string ->
  llmetadata
  = "llvm_dibuild_create_module_bytecode" "llvm_dibuild_create_module_native"

external dibuild_create_namespace :
  lldibuilder ->
  parent_ref:llmetadata ->
  name:string ->
  bool:string ->
  llmetadata = "llvm_dibuild_create_namespace"

external dibuild_create_function :
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
  = "llvm_dibuild_create_function_bytecode" "llvm_dibuild_create_function_native"

external dibuild_create_lexical_block :
  lldibuilder ->
  scope:llmetadata ->
  file:llmetadata ->
  line:int ->
  column:int ->
  llmetadata = "llvm_dibuild_create_lexical_block"

external dibuild_create_debug_location_helper :
  Llvm.llcontext ->
  line:int ->
  column:int ->
  scope:llmetadata ->
  inlined_at:llmetadata ->
  llmetadata = "llvm_dibuild_create_debug_location"

external llmetadata_null : unit -> llmetadata = "llvm_metadata_null"

let dibuild_create_debug_location ?(inlined_at = llmetadata_null ()) llctx ~line
    ~column ~scope =
  dibuild_create_debug_location_helper llctx line column scope inlined_at

external di_location_get_line : lllocation -> int
  = "llvm_di_location_get_line"

external di_location_get_column : lllocation -> int
  = "llvm_di_location_get_column"

external di_location_get_scope : lllocation -> llscope
  = "llvm_di_location_get_scope"

external di_location_get_inlined_at : lllocation -> llscope
  = "llvm_di_location_get_inlined_at"

external di_scope_get_file : llscope -> llfile option
  = "llvm_di_scope_get_file"

external di_file_get_directory : llfile -> string
  = "llvm_di_file_get_directory"

external di_file_get_filename : llfile -> string
  = "llvm_di_file_get_filename"

external di_file_get_source : llfile -> string
  = "llvm_di_file_get_source"

external dibuild_get_or_create_type_array :
  lldibuilder -> data:llmetadata array -> llmetadata
  = "llvm_dibuild_get_or_create_type_array"

external dibuild_create_subroutine_type :
  lldibuilder ->
  file:llmetadata ->
  param_types:llmetadata array ->
  lldiflags ->
  llmetadata = "llvm_dibuild_create_subroutine_type"

external dibuild_create_enumerator :
  lldibuilder -> name:string -> value:int -> is_unsigned:bool -> llmetadata
  = "llvm_dibuild_create_enumerator"

external dibuild_create_enumeration_type :
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
  = "llvm_dibuild_create_enumeration_type_native" "llvm_dibuild_create_enumeration_type_bytecode"

external dibuild_create_union_type :
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
  = "llvm_dibuild_create_union_type_native" "llvm_dibuild_create_union_type_bytecode"

external dibuild_create_array_type :
  lldibuilder ->
  size:int ->
  align_in_bits:int ->
  ty:llmetadata ->
  subscripts:llmetadata array ->
  llmetadata = "llvm_dibuild_create_array_type"

external dibuild_create_vector_type :
  lldibuilder ->
  size:int ->
  align_in_bits:int ->
  ty:llmetadata ->
  subscripts:llmetadata array ->
  llmetadata = "llvm_dibuild_create_array_type"

external dibuild_create_unspecified_type :
  lldibuilder -> name:string -> llmetadata
  = "llvm_dibuild_create_unspecified_type"

external dibuild_create_basic_type :
  lldibuilder ->
  name:string ->
  size_in_bits:int ->
  encoding:int ->
  lldiflags ->
  llmetadata = "llvm_dibuild_create_basic_type"

external dibuild_create_pointer_type :
  lldibuilder ->
  pointee_ty:llmetadata ->
  size_in_bits:int ->
  align_in_bits:int ->
  address_space:int ->
  name:string ->
  llmetadata
  = "llvm_dibuild_create_pointer_type_native" "llvm_dibuild_create_pointer_type_bytecode"

external dibuild_create_struct_type :
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
  = "llvm_dibuild_create_struct_type_native" "llvm_dibuild_create_struct_type_bytecode"

external dibuild_create_member_type :
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
  = "llvm_dibuild_create_member_type_native" "llvm_dibuild_create_member_type_bytecode"

external dibuild_create_static_member_type :
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
  = "llvm_dibuild_create_static_member_type_native" "llvm_dibuild_create_static_member_type_bytecode"

external dibuild_create_member_pointer_type :
  lldibuilder ->
  pointee_type:llmetadata ->
  class_type:llmetadata ->
  size_in_bits:int ->
  align_in_bits:int ->
  lldiflags ->
  llmetadata
  = "llvm_dibuild_create_member_pointer_type_native" "llvm_dibuild_create_member_pointer_type_bytecode"

external dibuild_create_object_pointer_type :
  lldibuilder -> llmetadata -> llmetadata
  = "llvm_dibuild_create_object_pointer_type"

external dibuild_create_qualified_type :
  lldibuilder -> tag:int -> llmetadata -> llmetadata
  = "llvm_dibuild_create_qualified_type"

external dibuild_create_reference_type :
  lldibuilder -> tag:int -> llmetadata -> llmetadata
  = "llvm_dibuild_create_reference_type"

external dibuild_create_null_ptr_type : lldibuilder -> llmetadata
  = "llvm_dibuild_create_null_ptr_type"

external dibuild_create_typedef :
  lldibuilder ->
  ty:llmetadata ->
  name:string ->
  file:llmetadata ->
  line_no:int ->
  scope:llmetadata ->
  align_in_bits:int ->
  llmetadata
  = "llvm_dibuild_create_typedef_native" "llvm_dibuild_create_typedef_bytecode"

external dibuild_create_inheritance_native :
  lldibuilder ->
  ty:llmetadata ->
  base_ty:llmetadata ->
  base_offset:int ->
  vb_ptr_offset:int ->
  lldiflags ->
  llmetadata
  = "llvm_dibuild_create_inheritance_native" "llvm_dibuild_create_inheritance_bytecode"

external dibuild_create_forward_decl :
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
  = "llvm_dibuild_create_forward_decl_native" "llvm_dibuild_create_forward_decl_bytecode"

external dibuild_create_replaceable_composite_type :
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
  = "llvm_dibuild_create_replaceable_composite_type_native" "llvm_dibuild_create_replaceable_composite_type_bytecode"

external dibuild_create_bit_field_member_type :
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
  = "llvm_dibuild_create_bit_field_member_type_native" "llvm_dibuild_create_bit_field_member_type_bytecode"

external dibuild_create_class_type :
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
  = "llvm_dibuild_create_class_type_native" "llvm_dibuild_create_class_type_bytecode"

external dibuild_create_artificial_type :
  lldibuilder -> ty:llmetadata -> llmetadata
  = "llvm_dibuild_create_artificial_type"

external ditype_get_name : llmetadata -> string = "llvm_ditype_get_name"

external ditype_get_size_in_bits : llmetadata -> int
  = "llvm_ditype_get_size_in_bits"

external ditype_get_offset_in_bits : llmetadata -> int
  = "llvm_ditype_get_offset_in_bits"

external ditype_get_align_in_bits : llmetadata -> int
  = "llvm_ditype_get_align_in_bits"

external ditype_get_line : llmetadata -> int = "llvm_ditype_get_line"

external ditype_get_flags : llmetadata -> lldiflags = "llvm_ditype_get_flags"

external get_subprogram : Llvm.llvalue -> llsubprogram option
  = "llvm_get_subprogram"

external set_subprogram : Llvm.llvalue -> llsubprogram -> unit
  = "llvm_set_subprogram"

external di_subprogram_get_line : llsubprogram -> int
  = "llvm_di_subprogram_get_line"

external instruction_get_debug_loc : Llvm.llvalue -> lllocation option
  = "llvm_instruction_get_debug_loc"

external instruction_set_debug_loc : Llvm.llvalue -> lllocation -> unit
  = "llvm_instruction_set_debug_loc"

external di_global_variable_expression_get_variable
  : llmetadata -> llvariable option
  = "llvm_di_global_variable_expression_get_variable"

external di_variable_get_line : llvariable -> int
  = "llvm_di_variable_get_line"

external di_variable_get_file : llvariable -> llfile option
  = "llvm_di_variable_get_file"

external get_metadata_kind : llmetadata -> MetadataKind.t
  = "llvm_get_metadata_kind"
