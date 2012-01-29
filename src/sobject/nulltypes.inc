(* Generated Type Classes, dont modify
 * This file is part of Trickonos.  
 *)




const
  TSOClsDummyObject: TSOTypeCls = (
    IsMortal: false;
    Wipe: false;
    InstanceSize: SizeOf( TSO_DummyObject );
    PostConstructor: nil { @socls_DummyObject_PostConstructor };
    PreDestructor: nil { @socls_DummyObject_PreDestructor };
    GCEnumerator: nil { @socls_DummyObject_GCEnumerator };
    MethodCall: @socls_DummyObject_MethodCall;
    MethodCallOverride: @socls_DummyObject_MethodCallOverride;
    Compare: @DefaultCompare;
    TypeQuery: @socls_DummyObject_TypeQuery
  );


const
  TSOClsNone: TSOTypeCls = (
    IsMortal: false;
    Wipe: false;
    InstanceSize: SizeOf( TSO_None );
    PostConstructor: nil { @socls_None_PostConstructor };
    PreDestructor: nil { @socls_None_PreDestructor };
    GCEnumerator: nil { @socls_None_GCEnumerator };
    MethodCall: @socls_None_MethodCall;
    MethodCallOverride: nil { @socls_None_MethodCallOverride };
    Compare: @DefaultCompare;
    TypeQuery: @socls_None_TypeQuery
  );


const
  TSOClsBoolean: TSOTypeCls = (
    IsMortal: false;
    Wipe: false;
    InstanceSize: SizeOf( TSO_Boolean );
    PostConstructor: nil { @socls_Boolean_PostConstructor };
    PreDestructor: nil { @socls_Boolean_PreDestructor };
    GCEnumerator: nil { @socls_Boolean_GCEnumerator };
    MethodCall: @socls_Boolean_MethodCall;
    MethodCallOverride: nil { @socls_Boolean_MethodCallOverride };
    Compare: @DefaultCompare;
    TypeQuery: @socls_Boolean_TypeQuery
  );


const
  TSOClsError: TSOTypeCls = (
    IsMortal: false;
    Wipe: true;
    InstanceSize: SizeOf( TSO_Error );
    PostConstructor: nil { @socls_Error_PostConstructor };
    PreDestructor: @socls_Error_PreDestructor;
    GCEnumerator: nil { @socls_Error_GCEnumerator };
    MethodCall: nil { @socls_Error_MethodCall };
    MethodCallOverride: nil { @socls_Error_MethodCallOverride };
    Compare: @DefaultCompare;
    TypeQuery: @socls_Error_TypeQuery
  );


const
  TSOClsInteger: TSOTypeCls = (
    IsMortal: true;
    Wipe: false;
    InstanceSize: SizeOf( TSO_Integer );
    PostConstructor: @socls_Integer_PostConstructor;
    PreDestructor: @socls_Integer_PreDestructor;
    GCEnumerator: nil { @socls_Integer_GCEnumerator };
    MethodCall: @socls_Integer_MethodCall;
    MethodCallOverride: nil { @socls_Integer_MethodCallOverride };
    Compare: @socls_Integer_Compare;
    TypeQuery: @socls_Integer_TypeQuery
  );


const
  TSOClsString: TSOTypeCls = (
    IsMortal: true;
    Wipe: false;
    InstanceSize: SizeOf( TSO_String );
    PostConstructor: @socls_String_PostConstructor;
    PreDestructor: @socls_String_PreDestructor;
    GCEnumerator: nil { @socls_String_GCEnumerator };
    MethodCall: @socls_String_MethodCall;
    MethodCallOverride: nil { @socls_String_MethodCallOverride };
    Compare: @socls_String_Compare;
    TypeQuery: @socls_String_TypeQuery
  );


const
  TSOClsList: TSOTypeCls = (
    IsMortal: true;
    Wipe: false;
    InstanceSize: SizeOf( TSO_List );
    PostConstructor: @socls_List_PostConstructor;
    PreDestructor: @socls_List_PreDestructor;
    GCEnumerator: @socls_List_GCEnumerator;
    MethodCall: @socls_List_MethodCall;
    MethodCallOverride: nil { @socls_List_MethodCallOverride };
    Compare: @socls_List_Compare;
    TypeQuery: @socls_List_TypeQuery
  );


const
  TSOClsDict: TSOTypeCls = (
    IsMortal: true;
    Wipe: true;
    InstanceSize: SizeOf( TSO_Dict );
    PostConstructor: @socls_Dict_PostConstructor;
    PreDestructor: @socls_Dict_PreDestructor;
    GCEnumerator: @socls_Dict_GCEnumerator;
    MethodCall: @socls_Dict_MethodCall;
    MethodCallOverride: nil { @socls_Dict_MethodCallOverride };
    Compare: @socls_Dict_Compare;
    TypeQuery: @socls_Dict_TypeQuery
  );


const
  TSOClsFunction: TSOTypeCls = (
    IsMortal: true;
    Wipe: true;
    InstanceSize: SizeOf( TSO_Function );
    PostConstructor: nil { @socls_Function_PostConstructor };
    PreDestructor: nil { @socls_Function_PreDestructor };
    GCEnumerator: nil { @socls_Function_GCEnumerator };
    MethodCall: @socls_Function_MethodCall;
    MethodCallOverride: @socls_Function_MethodCallOverride;
    Compare: @socls_Function_Compare;
    TypeQuery: @socls_Function_TypeQuery
  );


const
  TSOClsRT_ENV: TSOTypeCls = (
    IsMortal: false;
    Wipe: true;
    InstanceSize: SizeOf( TSO_RT_ENV );
    PostConstructor: @socls_RT_ENV_PostConstructor;
    PreDestructor: @socls_RT_ENV_PreDestructor;
    GCEnumerator: @socls_RT_ENV_GCEnumerator;
    MethodCall: nil { @socls_RT_ENV_MethodCall };
    MethodCallOverride: nil { @socls_RT_ENV_MethodCallOverride };
    Compare: @DefaultCompare;
    TypeQuery: @socls_RT_ENV_TypeQuery
  );


const
  TSOClsRT_STACK: TSOTypeCls = (
    IsMortal: false;
    Wipe: true;
    InstanceSize: SizeOf( TSO_RT_STACK );
    PostConstructor: @socls_RT_STACK_PostConstructor;
    PreDestructor: @socls_RT_STACK_PreDestructor;
    GCEnumerator: @socls_RT_STACK_GCEnumerator;
    MethodCall: nil { @socls_RT_STACK_MethodCall };
    MethodCallOverride: nil { @socls_RT_STACK_MethodCallOverride };
    Compare: @DefaultCompare;
    TypeQuery: @socls_RT_STACK_TypeQuery
  );


const
  TSOClsSystem: TSOTypeCls = (
    IsMortal: false;
    Wipe: true;
    InstanceSize: SizeOf( TSO_System );
    PostConstructor: @socls_System_PostConstructor;
    PreDestructor: @socls_System_PreDestructor;
    GCEnumerator: @socls_System_GCEnumerator;
    MethodCall: @socls_System_MethodCall;
    MethodCallOverride: nil { @socls_System_MethodCallOverride };
    Compare: @DefaultCompare;
    TypeQuery: @socls_System_TypeQuery
  );


const
  TSOClsIO: TSOTypeCls = (
    IsMortal: true;
    Wipe: true;
    InstanceSize: SizeOf( TSO_IO );
    PostConstructor: nil { @socls_IO_PostConstructor };
    PreDestructor: @socls_IO_PreDestructor;
    GCEnumerator: @socls_IO_GCEnumerator;
    MethodCall: @socls_IO_MethodCall;
    MethodCallOverride: @socls_IO_MethodCallOverride;
    Compare: @DefaultCompare;
    TypeQuery: @socls_IO_TypeQuery
  );


const
  TSOClsSubSys_FW: TSOTypeCls = (
    IsMortal: true;
    Wipe: true;
    InstanceSize: SizeOf( TSO_SubSys_FW );
    PostConstructor: nil { @socls_SubSys_FW_PostConstructor };
    PreDestructor: @socls_SubSys_FW_PreDestructor;
    GCEnumerator: nil { @socls_SubSys_FW_GCEnumerator };
    MethodCall: @socls_SubSys_FW_MethodCall;
    MethodCallOverride: nil { @socls_SubSys_FW_MethodCallOverride };
    Compare: @DefaultCompare;
    TypeQuery: @socls_SubSys_FW_TypeQuery
  );


const
  TSOClsSubSys_OW: TSOTypeCls = (
    IsMortal: true;
    Wipe: true;
    InstanceSize: SizeOf( TSO_SubSys_OW );
    PostConstructor: nil { @socls_SubSys_OW_PostConstructor };
    PreDestructor: @socls_SubSys_OW_PreDestructor;
    GCEnumerator: nil { @socls_SubSys_OW_GCEnumerator };
    MethodCall: @socls_SubSys_OW_MethodCall;
    MethodCallOverride: nil { @socls_SubSys_OW_MethodCallOverride };
    Compare: @DefaultCompare;
    TypeQuery: @socls_SubSys_OW_TypeQuery
  );


const
  TSOClsClass: TSOTypeCls = (
    IsMortal: true;
    Wipe: true;
    InstanceSize: SizeOf( TSO_Class );
    PostConstructor: @socls_Class_PostConstructor;
    PreDestructor: @socls_Class_PreDestructor;
    GCEnumerator: @socls_Class_GCEnumerator;
    MethodCall: @socls_Class_MethodCall;
    MethodCallOverride: @socls_Class_MethodCallOverride;
    Compare: @socls_Class_Compare;
    TypeQuery: @socls_Class_TypeQuery
  );


const
  TSOClsClsInstance: TSOTypeCls = (
    IsMortal: true;
    Wipe: true;
    InstanceSize: SizeOf( TSO_ClsInstance );
    PostConstructor: @socls_ClsInstance_PostConstructor;
    PreDestructor: @socls_ClsInstance_PreDestructor;
    GCEnumerator: @socls_ClsInstance_GCEnumerator;
    MethodCall: @socls_ClsInstance_MethodCall;
    MethodCallOverride: @socls_ClsInstance_MethodCallOverride;
    Compare: @DefaultCompare;
    TypeQuery: @socls_ClsInstance_TypeQuery
  );


