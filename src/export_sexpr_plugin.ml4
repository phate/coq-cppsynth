DECLARE PLUGIN "export_sexpr_plugin"

open Stdarg

let handle_export (qid : Libnames.qualid) : unit =
  Feedback.msg_notice (Pp.strbrk "export")

VERNAC COMMAND EXTEND ExportSExpr CLASSIFIED AS SIDEFF
| [ "ExportSExpr" global(qid) ] ->
  [ handle_export qid ]
END
