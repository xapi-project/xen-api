BEGIN { n = 0; }
# A function definition and its address
# Remember its address and update current symbol
# 0000000000850330 <unix_select>:
match($0, /^0*([0-9a-fA-F]+) <([^>]+)>/, symdef) {
 SYMBOL = symdef[2];
 ADDR = symdef[1];

 SYMADDR[ADDR] = SYMBOL;

 if (ADDR in LOADED) {
   for (idx in LOADED[ADDR]) {
     caller = LOADED[ADDR][idx]
     CALLS[symdef[2]][n++] = caller 
   }
 }
}

# Indirect calls (mostly used for C stubs)
# mov    $0x850330,%rax
# call   872834 <caml_c_call>
match($0, /mov.*0x([0-9a-fA-F]*),/, addr) {
  # this will have gaps, but the indexes will be unique
  LOADED[addr[1]][n++] = SYMBOL 
}

match($0, /call.*<([^>]+)>/, call) {
  CALLS[call[1]][n++] = SYMBOL 
}

END {
  SYM = "unix_select"
  had_calls = 0
  for (idx in CALLS[SYM]) {
    caller = CALLS[SYM][idx];
    print "--"
    if (caller ~ /caml(Thread|Unix__fun_).*/) {
      # direct calls from these functions to unix_select are expected
      print caller "[expected]"
    } else {
      print caller "[bad]"
      had_calls++
    }
    for (idx2 in CALLS[caller]) {
      caller2 = CALLS[caller][idx2];
      if (caller2 ~ /caml(Thread).*/) {
        print caller2 "[expected]"
      } else {
        print caller2 "[bad]"
        had_calls++
      }
      for (idx3 in CALLS[caller2]) {
        caller3 = CALLS[caller2][idx3];
        # but we don't expect anyone calling these functions from OCaml code,
        # reject that
        had_calls++
        print caller3 "[bad]"
        for (idx4 in CALLS[caller3]) {
          caller4 = CALLS[caller3][idx4];
          print caller4 "[bad]"
          for (idx5 in CALLS[caller4]) {
            caller5 = CALLS[caller4][idx5];
            print caller5 "[bad]"
          }
        }
      }
    }
  }
  if (had_calls > 0) {
    exit 2
  }
}
