

procedure rst_reloadindex; 
begin

  _mi[mi_cl_arg_limit] := rst_cl_arg_limit;
  _mi[mi_cl_sbuf_limit] := rst_cl_sbuf_limit;
  _mi[mi_cl_colitem_limit] := rst_cl_colitem_limit;
  _mi[mi_cl_objtrace_limit] := rst_cl_objtrace_limit;
  _mi[mi_cl_tplstack_limit] := rst_cl_tplstack_limit;
  _mi[mi_cl_objstack_limit] := rst_cl_objstack_limit;
  _mi[mi_cl_crecurse_limit] := rst_cl_crecurse_limit;
  _mi[mi_cl_bcloader_limit] := rst_cl_bcloader_limit;
end;
