DECLARE PLUGIN "export_sexpr_plugin"

open Declarations
open Environ
open Names
open Stdarg

let build_sexpr (node : string) (args : string list) =
  "(" ^ (String.concat " " (node :: args)) ^ ")"

let rec build_structure_field_body_sexpr (label, sfb) : string =
  match sfb with
    | SFBconst cb -> "Constant " ^ Label.to_string label
    | SFBmind mib -> "MInductive " ^ Label.to_string label
    | SFBmodule mb ->
      build_sexpr "Module" [ Label.to_string label ; build_module_body_sexpr mb ]
    | SFBmodtype mtb ->
      build_sexpr "ModuleType" [ Label.to_string label ; build_module_type_body_sexpr mtb ]

and build_structure_body_sexpr (me : Declarations.structure_body) : string =
  build_sexpr "StructureBody" (List.map build_structure_field_body_sexpr me)

and build_modsig_sexpr (ms : Declarations.module_signature) : string =
  match ms with
    | Declarations.NoFunctor sb -> build_structure_body_sexpr sb
    | Declarations.MoreFunctor (mbid, mtb1, ms) ->
      build_sexpr "Functor" [ Names.MBId.to_string mbid ; build_modsig_sexpr ms]

and build_modalgexpr_sexpr (ma : Declarations.module_alg_expr) : string =
  match ma with
    | MEident mp -> ModPath.to_string mp
    | MEapply (ma, mp) -> build_sexpr "Apply" [ build_modalgexpr_sexpr ma ; ModPath.to_string mp ]
    | MEwith (ma, wd) -> "Unhandled:Algebraic:WithDeclaration"

and build_modexpr_sexpr (me : Declarations.module_expression) : string =
  match me with
    | NoFunctor ma -> build_modalgexpr_sexpr ma
    | MoreFunctor (mbid, mtb1, me) -> build_sexpr "Functor" [ MBId.to_string mbid ; build_modexpr_sexpr me ]

and build_module_body_sexpr (mb : Declarations.module_body) : string =
  let body =
  (match mb.mod_expr with
    | Algebraic me -> build_modexpr_sexpr me
    | Struct ms -> (
      match mb.mod_type_alg with
        | None -> build_modsig_sexpr mb.mod_type
        | Some ty -> build_sexpr "Typed" [ build_modexpr_sexpr ty ; build_modsig_sexpr ms ])
    | FullStruct -> build_modsig_sexpr mb.mod_type
    | Abstract -> "Unhandled:Abstract"
  ) in
  body

and build_module_type_body_sexpr (mtb : Declarations.module_type_body) : string =
  build_modsig_sexpr mtb.mod_type

let build_module_sexpr (mp : ModPath.t) : string =
  let mb = Global.lookup_module mp in
  let body = build_module_body_sexpr mb in
  build_sexpr "Module" [ ModPath.to_string mp ; body ]




let handle_export (qid : Libnames.qualid) : unit =
  try
    let mp = Nametab.locate_module qid in
    Feedback.msg_notice (Pp.strbrk (build_module_sexpr mp))
  with
    | Not_found ->
      try
        let mp = Nametab.locate_modtype qid in
        Feedback.msg_notice (Pp.strbrk "modtype")
      with
        | Not_found ->
          try
            let gref = Nametab.locate qid in
            Feedback.msg_notice (Pp.strbrk "gref")
          with
            | Not_found -> CErrors.user_err (Pp.str "Not found")

VERNAC COMMAND EXTEND ExportSExpr CLASSIFIED AS SIDEFF
| [ "ExportSExpr" global(qid) ] ->
  [ handle_export qid ]
END
