add_ocaml_library(llvm_debuginfo
  OCAML    llvm_debuginfo
  OCAMLDEP llvm
  C        debuginfo_ocaml
  LLVM     debuginfo)
