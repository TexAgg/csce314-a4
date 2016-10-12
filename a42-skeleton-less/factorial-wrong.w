// factorial with syntax errors
var x = 12;

var acc = 1;
while x > 1 {  // should be (x > 1)
  acc = acc * x;
  x = x - 1;   // no symbol "-" for subtraction
}
print "result is ";
print acc;
print "\n";
