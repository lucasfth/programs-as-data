// method line 2
.method public static hidebysig 
		default void SelectionSort (int32[] arr)  cil managed 
{
	// Method begins at RVA 0x20b8
// Code size 57 (0x39)
.maxstack 4
.locals init (
	int32	V_0,
	int32	V_1,
	int32	V_2,
	int32	V_3)
IL_0000:  ldc.i4.0          // Push 0 onto the stack as int32
IL_0001:  stloc.0           // Pop a value from stack into local variable 0
IL_0002:  br.s IL_0032      // Branch to target (IL_0032), short form

IL_0004:  ldloc.0           // Load local variable 0 onto stack
IL_0005:  stloc.1           // Pop a value from stack into local variable 1
IL_0006:  ldloc.0           // Load local variable 0 onto stack
IL_0007:  ldc.i4.1          // Push 1 onto the stack as int32
IL_0008:  add               // Add two values, returning a new value
IL_0009:  stloc.3           // Pop a value from stack into local variable 3
IL_000a:  br.s IL_001a      // Branch to target (IL_001a), short form

IL_000c:  ldarg.0           // Load argument 0 onto the stack
IL_000d:  ldloc.3           // Load local variable 3 onto stack
IL_000e:  ldelem.i4         // Load the element with type int32 at index onto the top of the stack as an int32
IL_000f:  ldarg.0           // Load argument 0 onto the stack
IL_0010:  ldloc.1           // Load local variable 1 onto stack
IL_0011:  ldelem.i4         // Load the element with type int32 at index onto the top of the stack as an int32
IL_0012:  bge.s IL_0016     // Branch to target (IL_0016) if greater than or equal to, short form

IL_0014:  ldloc.3           // Load local variable 3 onto stack
IL_0015:  stloc.1           // Pop a value from stack into local variable 1
IL_0016:  ldloc.3           // Load local variable 3 onto stack
IL_0017:  ldc.i4.1          // Push 1 onto the stack as int32
IL_0018:  add               // Add two values, returning a new value
IL_0019:  stloc.3           // Pop a value from stack into local variable 3
IL_001a:  ldloc.3           // Load local variable 3 onto stack
IL_001b:  ldarg.0           // Load argument 0 onto the stack
IL_001c:  ldlen             // Push the length (of type native unsigned int) of array on the stack
IL_001d:  conv.i4           // Convert to int32, pushing int32 on stack
IL_001e:  blt.s IL_000c     // Branch to target (IL_000c) if less than, short form

IL_0020:  ldarg.0           // Load argument 0 onto the stack
IL_0021:  ldloc.0           // Load local variable 0 onto stack
IL_0022:  ldelem.i4         // Load the element with type int32 at index onto the top of the stack as an int32
IL_0023:  stloc.2           // Pop a value from stack into local variable 2
IL_0024:  ldarg.0           // Load argument 0 onto the stack
IL_0025:  ldloc.0           // Load local variable 0 onto stack
IL_0026:  ldarg.0           // Load argument 0 onto the stack
IL_0027:  ldloc.1           // Load local variable 1 onto stack
IL_0028:  ldelem.i4         // Load the element with type int32 at index onto the top of the stack as an int32
IL_0029:  stelem.i4         // Replace array element at index with the int32 value on the stack
IL_002a:  ldarg.0           // Load argument 0 onto the stack
IL_002b:  ldloc.1           // Load local variable 1 onto stack
IL_002c:  ldloc.2           // Load local variable 2 onto stack
IL_002d:  stelem.i4         // Replace array element at index with the int32 value on the stack
IL_002e:  ldloc.0           // Load local variable 0 onto stack
IL_002f:  ldc.i4.1          // Push 1 onto the stack as int32
IL_0030:  add               // Add two values, returning a new value
IL_0031:  stloc.0           // Pop a value from stack into local variable 0
IL_0032:  ldloc.0           // Load local variable 0 onto stack
IL_0033:  ldarg.0           // Load argument 0 onto the stack
IL_0034:  ldlen             // Push the length (of type native unsigned int) of array on the stack
IL_0035:  conv.i4           // Convert to int32, pushing int32 on stack
IL_0036:  blt.s IL_0004     // Branch to target (IL_004) if less than, short form

IL_0038:  ret 
} // end of method Selsort::SelectionSort