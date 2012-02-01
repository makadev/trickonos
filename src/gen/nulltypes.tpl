{? open 'nulltypes.inc'; 
?}(* Generated Type Classes, dont modify
 * This file is part of Trickonos.  
 *)

{? // }

function TypeFunName( TName, Name, Gen, Decl := false );
begin
  if Gen then
    Result := 'socls_'+TName+'_'+Name;
    if not Decl then
      Result := '@'+Result;
    end;
  else
    Result := 'nil { @socls_'+TName+'_'+Name+' }';
  end;
end;

function NewType( Name, IsMortal, Wipe, Con, Des, GCEnum, MC, MCO, 
                  OC := true, TC := true, OwnRef := false );
begin
  // write const def
?}
const
  TSOCls{? write Name; ?}: TSOTypeCls = (
    IsMortal: {? write IsMortal; ?};
    Wipe: {? write Wipe; ?};
    InstanceSize: SizeOf( TSO_{? write Name; ?} );
    PostConstructor: {? write TypeFunName( Name, 'PostConstructor', Con ); ?};
    PreDestructor: {? write TypeFunName( Name, 'PreDestructor', Des ); ?};
    GCEnumerator: {? write TypeFunName( Name, 'GCEnumerator', GCEnum ); ?};
    MethodCall: {? write TypeFunName( Name, 'MethodCall', MC ); ?};
    MethodCallOverride: {? write TypeFunName( Name, 'MethodCallOverride', MCO ); ?};
    Compare: {? 
      if oc then
        write TypeFunName( Name, 'Compare', true ); 
      else
        write '@DefaultCompare';
      end;
    ?};
    TypeQuery: {? write TypeFunName( Name, 'TypeQuery', TC ); ?}
  );

{? // }
  end;
?}

{? // }
  NewType('DummyObject',
    false,  // mortal
    false,  // wipe
    false,  // con
    false,  // des
    false,  // gcenum
    true,   // methodcall
    true,   // methodcall override
    false   // use default compare
  );

  NewType('None',
    false,  // mortal
    false,  // wipe
    false,  // con
    false,  // des
    false,  // gcenum
    true,   // methodcall
    false,  // methodcall override
    false   // use default compare
  );

  NewType('Boolean',
    false,  // mortal
    false,  // wipe
    false,  // con
    false,  // des
    false,  // gcenum
    true,   // methodcall
    false,  // methodcall override
    false   // use default compare
  );

  NewType('Error',
    false,  // mortal
    true,   // wipe
    false,  // con
    true,   // des
    false,  // gcenum
    false,  // methodcall
    false,  // methodcall override
    false   // use default compare
  );

  NewType('Integer',
    true,   // mortal
    false,  // wipe
    true,   // con
    true,   // des
    false,  // gcenum
    true,   // methodcall
    false   // methodcall override
  );

  NewType('String',
    true,   // mortal
    false,  // wipe
    true,   // con
    true,   // des
    false,  // gcenum
    true,   // methodcall
    false,  // methodcall override
    true,   // own cmp
    true    // tc
  );

  NewType('List',
    true,   // mortal
    false,  // wipe
    true,   // con
    true,   // des
    true,   // gcenum
    true,   // methodcall
    false   // methodcall override
  );

  NewType('Dict',
    true,   // mortal
    true,   // wipe
    true,   // con
    true,   // des
    true,   // gcenum
    true,   // methodcall
    false   // methodcall override
  );

  NewType('Function',
    true,   // mortal
    true,   // wipe
    false,  // con
    false,  // des
    false,  // gcenum
    true,   // methodcall
    true    // methodcall override
  );

  NewType('RT_ENV',
    false,  // mortal
    true,   // wipe
    true,   // con
    true,   // des
    true,   // gcenum
    false,  // methodcall
    false,  // methodcall override
    false   // use default compare
  );

  NewType('RT_STACK',
    false,  // mortal
    true,   // wipe
    true,   // con
    true,   // des
    true,   // gcenum
    false,  // methodcall
    false,  // methodcall override
    false   // use default compare
  );

  NewType('System',
    false,  // mortal
    true,   // wipe
    true,   // con
    true,   // des
    true,   // gcenum
    true,   // methodcall
    false,  // methodcall override
    false   // use default compare
  );

  NewType('IO',
    true,  // mortal
    true,  // wipe
    false, // con
    true,  // des
    true,  // gcenum
    true,  // methodcall
    false, // methodcall override
    false  // use default compare
  );

  NewType('SubSys_FW',
    true,  // mortal
    true,  // wipe
    false, // con
    true,  // des
    false, // gcenum
    true,  // methodcall
    false, // methodcall override
    false  // use default compare
  );

  NewType('SubSys_OW',
    true,  // mortal
    true,  // wipe
    false, // con
    true,  // des
    false, // gcenum
    true,  // methodcall
    false, // methodcall override
    false  // use default compare
  );

  NewType('Class',
    true,  // mortal
    true,  // wipe
    true,  // con
    true,  // des
    true,  // gcenum
    true,  // methodcall
    true,  // methodcall override
    true   // own compare
  );
  
  NewType('ClsInstance',
    true,  // mortal
    true,  // wipe
    true,  // con
    true,  // des
    true,  // gcenum
    true,  // methodcall
    true,  // methodcall override
    false  // use default compare
  );
?}
