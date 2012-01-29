{?
  USE "ctab.use";
  USE "mindex";
  
  /* write resourcestring index reload (for intl) */
  
?}

procedure rst_reloadindex; 
begin
{? foreach k:v in resourcestr.con do ?}
  {? write '_mi[mi_'+k.lowercase()+'] := rst_' + k.lowercase()+';';
  end; ?}
end;
