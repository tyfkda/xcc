#!/bin/bash

CPP=${CPP:-../cpp}

try() {
  title="$1"
  expected=$(echo -e "$2")
  input="$3"

  echo -n "$title => "

  actual=$(echo -e "$input" | $CPP | tr -d '\n') || exit 1

  if [ "$actual" = "$expected" ]; then
    echo "OK"
  else
    echo "NG: $expected expected, but got $actual"
    exit 1
  fi
}

compile_error() {
  title="$1"
  input="$2"

  echo -n "$title => "

  echo -e "$input" | $CPP
  result="$?"

  if [ "$result" = "0" ]; then
    echo "NG: Compile error expected, but succeeded"
    exit 1
  fi
}

try 'NULL' '0 ' "#define NULL 0\nNULL"
try 'Param' '1+ 2' "#define ADD(x,y) x+y\nADD(1,2)"
try 'Newline in macro' '1+ 2' "#define ADD(x,y) x+y\nADD(1,\n2)"
try '#ifdef' 'x' "#define X\n#ifdef X\nx\n#else\ny\n#endif"
try '#ifdef else' 'y' "#ifdef X\nx\n#else\ny\n#endif"
try '#if 1' '2' "#if 1\n2\n#else\n3\n#endif"
try '#if 0' '3' "#if 0\n3\n#else\n3\n#endif"
try '#elif-1' '2' "#if 1\n2\n#elif 1\n3\n#elif 1\n4#else\n5\n#endif"
try '#elif-01' '3' "#if 0\n2\n#elif 1\n3\n#elif 1\n4\n#else\n5\n#endif"
try '#elif-001' '4' "#if 0\n2\n#elif 0\n3\n#elif 1\n4\n#else\n5\n#endif"
try '#if defined' '3' "#if defined(XXX)\n2\n#else\n3\n#endif"
try '#if !defined' '2' "#if !defined(XXX)\n2\n#else\n3\n#endif"
try '#if &&' '3' "#if 1 && 2\n3\n#else\n4\n#endif"
try '#if &&-2' '4' "#if 1 && 0\n3\n#else\n4\n#endif"
try '#if ||' '3' "#if 0 || 1\n3\n#else\n4\n#endif"
try '#if ||-2' '4' "#if 0 || 0\n3\n#else\n4\n#endif"
try '__FILE__' '"*stdin*"' "__FILE__"
try '__LINE__' "3" "\n\n__LINE__"
try 'Block comment' '/*block comment*/' "/*\nblock comment\n*/"
try 'Quote in comment' "/*I'm fine*/" "/*\nI'm fine\n*/"

compile_error '#error' '#error !!!\nvoid main(){}'
